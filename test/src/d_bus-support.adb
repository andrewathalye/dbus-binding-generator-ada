pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with D_Bus.Connection;
with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;
with D_Bus.Messagebox;

with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;
with dbus_types_h;

package body D_Bus.Support is
   Connection : constant D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect;

   ------------------
   -- Get_Property --
   ------------------
   function Get_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String) return D_Bus.Arguments.Argument_Type'Class
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Request, Reply : D_Bus.Arguments.Argument_List_Type;
   begin
      --  Generate request
      D_Bus.Arguments.Append (Request, +Iface);
      D_Bus.Arguments.Append (Request, +Property);

      --  Reply
      Reply :=
        Call_Blocking
          (Destination, Node, "org.freedesktop.DBus.Properties", "Get",
           Request);

      return
        D_Bus.Arguments.Containers.Variant_Type (Reply.First_Element)
          .Get_Argument;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------
   procedure Set_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String; Value : D_Bus.Arguments.Argument_Type'Class)
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Variant          : D_Bus.Arguments.Containers.Variant_Type;
      Request, Discard : D_Bus.Arguments.Argument_List_Type;
   begin
      --  Generate request
      Variant := D_Bus.Arguments.Containers.Create (Value);
      D_Bus.Arguments.Append (Request, +Iface);
      D_Bus.Arguments.Append (Request, +Property);
      D_Bus.Arguments.Append (Request, Variant);

      --  Reply
      Discard :=
        Call_Blocking
          (Destination, Node, "org.freedesktop.DBus.Properties", "Set",
           Request);
   end Set_Property;

   -------------------
   -- Call_Blocking --
   -------------------
   function Call_Blocking
     (Destination : String; Path : D_Bus.Types.Obj_Path; Iface : String;
      Method      : String; Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type
   is
   begin
      return
        D_Bus.Connection.Call_Blocking
          (Connection, Destination, Path, Iface, Method,
           D_Bus.Connection.Default_Timeout, Args);
   end Call_Blocking;

   -------------
   -- SIGNALS --
   -------------
   function Matches_Id
     (Msg : D_Bus.Messages.Message_Type; Id : Signal_Id) return Boolean;
   function Matches_Id
     (Msg : D_Bus.Messages.Message_Type; Id : Signal_Id) return Boolean
   is
      use D_Bus.Messages;
      use D_Bus.Types;
      use Ada.Strings.Unbounded;
   begin
      return Get_Path (Msg) = To_String (Id.Node)
         and then Get_Interface (Msg) = To_String (Id.Iface)
         and then Get_Member (Msg) = To_String (Id.Signal);
   end Matches_Id;

   ---------------------
   -- Register_Signal --
   ---------------------
   function Register_Signal
     (Node : D_Bus.Types.Obj_Path;
      Iface : String;
      Signal : String) return Signal_Id
   is
      use Ada.Strings.Unbounded;

      Match_Rule : constant String :=
         "path=" & D_Bus.Types.To_String (Node) & ", interface=" & Iface &
         ", member=" & Signal;
   begin
      D_Bus.Connection.Add_Match (Connection, Match_Rule);

      return
        (Registered => True,
         Rule => To_Unbounded_String (Match_Rule),
         Node => Node,
         Iface => To_Unbounded_String (Iface),
         Signal => To_Unbounded_String (Signal));
   end Register_Signal;

   -----------------------
   -- Unregister_Signal --
   -----------------------
   procedure Unregister_Signal (Id : out Signal_Id)
   is
      use Ada.Strings.Unbounded;

      procedure Remove_Match (Rule : String);
      procedure Remove_Match (Rule : String) is
         use D_Bus.Arguments.Basic;
         use type D_Bus.Types.Obj_Path;

         Args, Discard : D_Bus.Arguments.Argument_List_Type;
      begin
         D_Bus.Arguments.Append (Args, +Rule);
         Discard :=
           Call_Blocking
             ("org.freedesktop.DBus", +"/org/freedesktop/DBus",
              "org.freedesktop.DBus", "RemoveMatch", Args);
      end Remove_Match;
   begin
      if not Id.Registered then
         raise D_Bus_Error with
            "'Id' was not registered prior to Unregister_Signal.";
      end if;

      Remove_Match (To_String (Id.Rule));
      Id.Registered := False;
   end Unregister_Signal;

   ------------------
   -- Await_Signal --
   ------------------
   Should_Continue : Boolean := True;
   Signal_Msg : D_Bus.Messages.Message_Type;
   Signal_Msgs : D_Bus.Messagebox.Msg_List;
   Active_Signal : Signal_Id;

   function Call_Back
     (D_Conn   : access dbus_connection_h.DBusConnection;
      Msg      : access dbus_message_h.DBusMessage;
      Usr_Data : System.Address) return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Call_Back);

   function Call_Back
     (D_Conn   : access dbus_connection_h.DBusConnection;
      Msg      : access dbus_message_h.DBusMessage;
      Usr_Data : System.Address) return dbus_shared_h.DBusHandlerResult
   is
      M : constant D_Bus.Messages.Message_Type :=
         D_Bus.Messages.Create (Msg);
      pragma Unreferenced (D_Conn, Usr_Data);
   begin
      if Matches_Id (M, Active_Signal) then
         Should_Continue := False;
         Signal_Msg := M;
      else
         Signal_Msgs.Append (M);
      end if;

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Call_Back;

   procedure Null_Free_Data (Item : System.Address) is null;
   pragma Convention (C, Null_Free_Data);

   function Await_Signal (Id : Signal_Id) return D_Bus.Messages.Message_Type is
      use Interfaces.C;

      type Connection_Overlay is record
         Thin_Connection : access dbus_connection_h.DBusConnection;
      end record;

      function Convert is new Ada.Unchecked_Conversion
        (D_Bus.Connection.Connection_Type, Connection_Overlay);

      Thin_Connection : access dbus_connection_h.DBusConnection;

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      if not Id.Registered then
         raise D_Bus_Error with
            "'Id' was not registered prior to Await_Signal.";
      end if;

      --  Check queued messages
      for Msg of Signal_Msgs loop
         if Matches_Id (Msg, Id) then
            declare
               C : D_Bus.Messagebox.ML.Cursor;
            begin
               C := Signal_Msgs.Find (Msg);
               Signal_Msgs.Delete (C);
            end;
            return Msg;
         end if;
      end loop;

      --  Set up filter
      Thin_Connection := Convert (Connection).Thin_Connection;
      D_Res := dbus_connection_h.dbus_connection_add_filter
        (connection => Thin_Connection,
        c_function => Call_Back'Access,
        user_data => System.Null_Address,
        free_data_function => Null_Free_Data'Access);

      if D_Res = 0 then
         raise D_Bus_Error with "Could not add connection filter";
      end if;

      --  Dispatch
      Active_Signal := Id;
      pragma Warnings (Off, "is not modified in loop body");
      Should_Continue := True;
      while Should_Continue loop
         D_Res := dbus_connection_h.dbus_connection_read_write_dispatch
           (Thin_Connection, -1);

         if D_Res = 0 then
            raise D_Bus_Error with "Dispatch failed";
         end if;
      end loop;
      pragma Warnings (On, "is not modified in loop body");
      Active_Signal := Null_Signal_Id;

      --  Remove filter
      dbus_connection_h.dbus_connection_remove_filter
        (Thin_Connection, Call_Back'Access, System.Null_Address);

      return Signal_Msg;
   end Await_Signal;
end D_Bus.Support;
