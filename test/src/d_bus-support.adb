pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with System;

with D_Bus.Arguments.Basic;

with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;
with dbus_types_h;
with System.Address_To_Access_Conversions;

package body D_Bus.Support is
   ---------------
   -- Internals --
   ---------------
   type Connection_Overlay is record
      Thin_Connection : access dbus_connection_h.DBusConnection;
   end record;

   function Convert is new Ada.Unchecked_Conversion
     (D_Bus.Connection.Connection_Type, Connection_Overlay);

   type Signal_Data_Pack is record
      Node : D_Bus.Types.Obj_Path;
      Iface : Ada.Strings.Unbounded.Unbounded_String;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Match : Boolean := True;
      Msg : D_Bus.Messages.Message_Type;
   end record;

   package SDP_Conversions is new System.Address_To_Access_Conversions
     (Signal_Data_Pack);

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
      pragma Unreferenced (D_Conn);

      use D_Bus.Messages;
      use D_Bus.Types;
      use Ada.Strings.Unbounded;

      M : constant D_Bus.Messages.Message_Type :=
         D_Bus.Messages.Create (Msg);
      SDP : constant access Signal_Data_Pack := SDP_Conversions.To_Pointer
        (Usr_Data);
   begin
      if Get_Path (M) = To_String (SDP.Node)
         and Get_Interface (M) = To_String (SDP.Iface)
         and Get_Member (M) = To_String (SDP.Name)
      then
         SDP.Match := True;
      end if;

      SDP.Msg := M;

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Call_Back;
   --  TODO: currently consumes non-signals

   procedure Null_Free_Data (Item : System.Address) is null;
   pragma Convention (C, Null_Free_Data);

   procedure Assert_Valid (O : Root_Object);
   procedure Assert_Valid (O : Root_Object) is
   begin
      if not O.Valid then
         raise D_Bus_Error with "Invalid D_Bus Object was used.";
      end if;
   end Assert_Valid;

   ----------------
   -- OO Signals --
   ----------------
   procedure Register_Signal
     (O : in out Root_Object;
      Iface : String;
      Name : String)
   is
      Match_Rule : constant String :=
         "path=" & D_Bus.Types.To_String (O.Node) & ", interface=" & Iface &
         ", member=" & Name;
   begin
      Assert_Valid (O);

      if O.Signals.Contains (Iface & "/" & Name) then
         raise D_Bus_Error with "Asked to register duplicate signal.";
      end if;

      D_Bus.Connection.Add_Match (O.Connection, Match_Rule);
      O.Signals.Insert (Iface & "/" & Name, Match_Rule);
   end Register_Signal;

   procedure Unregister_Signal
    (O : in out Root_Object;
     Iface : String;
     Name : String)
   is
      procedure Remove_Match (Rule : String);
      procedure Remove_Match (Rule : String) is
         use D_Bus.Arguments.Basic;
         use type D_Bus.Types.Obj_Path;

         Args, Discard : D_Bus.Arguments.Argument_List_Type;
      begin
         D_Bus.Arguments.Append (Args, +Rule);
         Discard :=
           D_Bus.Connection.Call_Blocking
             (Connection => O.Connection,
              Destination => "org.freedesktop.DBus",
              Path => +"/org/freedesktop/DBus",
              Iface => "org.freedesktop.DBus",
              Method => "RemoveMatch",
              Args => Args);
      end Remove_Match;
   begin
      Assert_Valid (O);

      if not O.Signals.Contains (Iface & "/" & Name) then
         raise D_Bus_Error with "Asked to unregister nonexisting signal.";
      end if;

      Remove_Match (O.Signals (Iface & "/" & Name));
      O.Signals.Delete (Iface & "/" & Name);
   end Unregister_Signal;

   function Await_Signal
     (O : Root_Object;
      Iface : String;
      Name : String) return D_Bus.Messages.Message_Type
   is
      use Ada.Strings.Unbounded;

      use D_Bus.Messages;
      use D_Bus.Types;

      use type Interfaces.C.int;
      use type dbus_types_h.dbus_bool_t;

      --  Variables
      CO : Connection_Overlay;
      SDP : aliased Signal_Data_Pack;
      D_Res : dbus_types_h.dbus_bool_t;
   begin
      Assert_Valid (O);
      CO := Convert (O.Connection);

      --  Check queued messages
      for Msg of O.Messages.all loop
         if Get_Path (Msg) = To_String (O.Node)
            and Get_Interface (Msg) = Iface
            and Get_Member (Msg) = Name
         then
            declare
               C : D_Bus.Messagebox.ML.Cursor;
            begin
               C := O.Messages.Find (Msg);
               O.Messages.Delete (C);
            end;

            return Msg;
         end if;
      end loop;

      --  Set up filter
      D_Res := dbus_connection_h.dbus_connection_add_filter
        (connection => CO.Thin_Connection,
        c_function => Call_Back'Access,
        user_data => SDP'Address,
        free_data_function => Null_Free_Data'Access);

      if D_Res = 0 then
         raise D_Bus_Error with "Could not add connection filter";
      end if;

      --  Dispatch
      SDP.Node := O.Node;
      SDP.Iface := To_Unbounded_String (Iface);
      SDP.Name := To_Unbounded_String (Name);

      while not SDP.Match loop
         D_Res := dbus_connection_h.dbus_connection_read_write_dispatch
           (CO.Thin_Connection, -1);

         if D_Res = 0 then
            raise D_Bus_Error with "Dispatch failed";
         end if;

         if not SDP.Match then
            O.Messages.Append (SDP.Msg);
         end if;
      end loop;

      --  Remove filter
      dbus_connection_h.dbus_connection_remove_filter
        (CO.Thin_Connection, Call_Back'Access, SDP'Address);

      return SDP.Msg;
   end Await_Signal;

   ----------------
   -- OO Methods --
   ----------------
   function Call_Blocking
     (O      : Root_Object;
      Iface  : String;
      Method : String;
      Args   : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type
   is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid (O);

      if O.Destination = Null_Unbounded_String then
         raise D_Bus_Error with
            "Asked to call method on object with no destination.";
      end if;

      return D_Bus.Connection.Call_Blocking
        (Connection => O.Connection,
         Destination => To_String (O.Destination),
         Path => O.Node,
         Iface => Iface,
         Method => Method,
         Args => Args);
   end Call_Blocking;

   --------------------------------
   -- Constructor and Destructor --
   --------------------------------
   procedure Create
     (O : out Root_Object;
      Node : Unbounded_Object_Path)
   is
      use Ada.Strings.Unbounded;
      use type D_Bus.Types.Obj_Path;
   begin
      if O.Valid then
         raise D_Bus_Error with "Asked to recreate a valid object.";
      end if;

      O.Connection := D_Bus.Connection.Connect;
      O.Node := +To_String (Node);
      O.Messages := new D_Bus.Messagebox.Msg_List;
      O.Valid := True;
   end Create;

   procedure Set_Destination
     (O : out Root_Object;
      Destination : String)
   is
      use Ada.Strings.Unbounded;
   begin
      O.Destination := To_Unbounded_String (Destination);
   end Set_Destination;

   procedure Destroy (O : in out Root_Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (D_Bus.Messagebox.Msg_List, Msg_List_Access);

      CO : constant Connection_Overlay := Convert (O.Connection);
   begin
      Assert_Valid (O);

      O.Valid := False;
      dbus_connection_h.dbus_connection_close (CO.Thin_Connection);
      O.Destination := Ada.Strings.Unbounded.Null_Unbounded_String;
      O.Signals.Clear;
      O.Messages.Clear;
      Free (O.Messages);
   end Destroy;
end D_Bus.Support;
