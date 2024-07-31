pragma Ada_2012;

with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;

with D_Bus.Arguments.Basic;
with D_Bus.Messagebox;

with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;
with dbus_types_h;

package body D_Bus.Support.Client is
   ----------
   -- Base --
   ----------
   procedure Check_Signature (Actual, Expected : String);
   procedure Check_Signature (Actual, Expected : String) is
   begin
      if Actual /= Expected then
         raise Invalid_Signature
           with "Expected signature " & Expected & " but found " & Actual;
      end if;
   end Check_Signature;

   procedure Check_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type; Signature : String)
   is
   begin
      Check_Signature (Get_Signature (Arguments), Signature);
   end Check_Signature;

   procedure Check_Signature
     (Argument : D_Bus.Arguments.Argument_Type'Class; Signature : String)
   is
   begin
      Check_Signature (Argument.Get_Signature, Signature);
   end Check_Signature;

   ---------------
   -- Internals --
   ---------------
   --  Shared State (Needs Locking)
   Messages : D_Bus.Messagebox.Msg_List;

   --  Callback Implementation
   type Signal_Data_Pack is record
      Ran : Boolean := False;
      Msg : D_Bus.Messages.Message_Type;
   end record;

   package SDP_Conversions is new System.Address_To_Access_Conversions
     (Signal_Data_Pack);

   function Call_Back
     (D_Conn : access dbus_connection_h.DBusConnection;
      Msg    : access dbus_message_h.DBusMessage; Usr_Data : System.Address)
      return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Call_Back);

   function Call_Back
     (D_Conn : access dbus_connection_h.DBusConnection;
      Msg    : access dbus_message_h.DBusMessage; Usr_Data : System.Address)
      return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (D_Conn);

      M : constant D_Bus.Messages.Message_Type := D_Bus.Messages.Create (Msg);
      SDP : constant access Signal_Data_Pack     :=
        SDP_Conversions.To_Pointer (Usr_Data);
   begin
      SDP.Msg := M;
      SDP.Ran := True;

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Call_Back;

   procedure Null_Free_Data (Item : System.Address) is null;
   pragma Convention (C, Null_Free_Data);

   ----------------
   -- OO Signals --
   ----------------
   procedure Register_Signal
     (O : in out Client_Object; Iface : String; Name : String)
   is
      Match_Rule : constant String :=
        "path=" & D_Bus.Types.To_String (O.Node) & ", interface=" & Iface &
        ", member=" & Name;
   begin
      Assert_Valid (O);

      if O.Signals.Contains (Iface & "/" & Name) then
         raise D_Bus_Error with "Asked to register duplicate signal.";
      end if;

      --  Note: changing global state
      D_Bus_Lock.Acquire;
      Critical_Section :
      begin
         D_Bus.Connection.Add_Match (Connection, Match_Rule);
      end Critical_Section;
      D_Bus_Lock.Release;

      O.Signals.Insert (Iface & "/" & Name, Match_Rule);
   end Register_Signal;

   procedure Unregister_Signal
     (O : in out Client_Object; Iface : String; Name : String)
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
             (Connection => Connection, Destination => "org.freedesktop.DBus",
              Path       => +"/org/freedesktop/DBus",
              Iface      => "org.freedesktop.DBus", Method => "RemoveMatch",
              Args       => Args);
      end Remove_Match;
   begin
      Assert_Valid (O);
      if not O.Signals.Contains (Iface & "/" & Name) then
         raise D_Bus_Error with "Asked to unregister nonexisting signal.";
      end if;

      --  Note: Changing global Connection state
      D_Bus_Lock.Acquire;
      Critical_Section :
      begin
         Remove_Match (O.Signals (Iface & "/" & Name));
      end Critical_Section;
      D_Bus_Lock.Release;

      O.Signals.Delete (Iface & "/" & Name);
   end Unregister_Signal;

   procedure Await_Signal
     (O : Client_Object; Msg : out D_Bus.Messages.Message_Type; Iface : String;
      Name : String)
   is
      use type Interfaces.C.int;
      use type dbus_types_h.dbus_bool_t;

      function Is_Match
        (Msg   : D_Bus.Messages.Message_Type; Node : D_Bus.Types.Obj_Path;
         Iface : String; Member : String) return Boolean;
      function Is_Match
        (Msg   : D_Bus.Messages.Message_Type; Node : D_Bus.Types.Obj_Path;
         Iface : String; Member : String) return Boolean
      is
         use D_Bus.Messages;
         use D_Bus.Types;
      begin
         return
           Get_Path (Msg) = To_String (Node) and
           Get_Interface (Msg) = Iface and Get_Member (Msg) = Member;
      end Is_Match;

      --  Variables
      CO    : Connection_Overlay;
      SDP   : aliased Signal_Data_Pack;
      D_Res : dbus_types_h.dbus_bool_t;
   begin
      Assert_Valid (O);
      CO := Convert (Connection);

      --  Note: Accessing global variables
      D_Bus_Lock.Acquire;
      Critical_Section :
      begin
         --  Check queued messages
         for Q_Msg of Messages loop
            if Is_Match (Q_Msg, O.Node, Iface, Name) then
               declare
                  C : D_Bus.Messagebox.ML.Cursor;
               begin
                  C := Messages.Find (Q_Msg);
                  Messages.Delete (C);
               end;

               Msg := Q_Msg;
               return;
            end if;
         end loop;

         --  Set up filter
         D_Res :=
           dbus_connection_h.dbus_connection_add_filter
             (connection => CO.Thin_Connection, c_function => Call_Back'Access,
              user_data          => SDP'Address,
              free_data_function => Null_Free_Data'Access);

         if D_Res = 0 then
            raise D_Bus_Error with "Could not add connection filter";
         end if;

         --  Dispatch
         loop
            SDP.Ran := False;
            D_Res   :=
              dbus_connection_h.dbus_connection_read_write_dispatch
                (CO.Thin_Connection, -1);

            if D_Res = 0 then
               raise D_Bus_Error with "Dispatch failed";
            end if;

            --  Exit if the message matches the pattern
            if SDP.Ran then
               if Is_Match (SDP.Msg, O.Node, Iface, Name) then
                  exit;
               else
                  Messages.Append (SDP.Msg);
               end if;
            end if;
         end loop;

         --  Remove filter
         dbus_connection_h.dbus_connection_remove_filter
           (CO.Thin_Connection, Call_Back'Access, SDP'Address);
      end Critical_Section;
      D_Bus_Lock.Release;

      Msg := SDP.Msg;
   end Await_Signal;

   ----------------
   -- OO Methods --
   ----------------
   function Call_Blocking
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type
   is
      use Ada.Strings.Unbounded;

      Result : D_Bus.Arguments.Argument_List_Type;
   begin
      Assert_Valid (O);
      if O.Destination = Null_Unbounded_String then
         raise D_Bus_Error
           with "Asked to call method on object with no destination.";
      end if;

      --  Note: accessing global Connection
      D_Bus_Lock.Acquire;
      Critical_Section :
      begin
         Result :=
           D_Bus.Connection.Call_Blocking
             (Connection  => Connection,
              Destination => To_String (O.Destination), Path => O.Node,
              Iface       => Iface, Method => Method, Args => Args);
      end Critical_Section;
      D_Bus_Lock.Release;

      return Result;
   end Call_Blocking;

   procedure Set_Property
     (O     : Client_Object; Iface : String; Name : String;
      Value : D_Bus.Arguments.Containers.Variant_Type)
   is
      use D_Bus.Arguments.Basic;

      Request : D_Bus.Arguments.Argument_List_Type;
      Discard : D_Bus.Arguments.Argument_List_Type;
   begin
      Request.Append (+Iface);
      Request.Append (+Name);
      Request.Append (Value);

      Discard :=
        O.Call_Blocking ("org.freedesktop.DBus.Properties", "Set", Request);
   end Set_Property;

   procedure Get_Property
     (O     :     Client_Object; Iface : String; Name : String;
      Value : out D_Bus.Arguments.Containers.Variant_Type)
   is
      use D_Bus.Arguments.Basic;

      Request : D_Bus.Arguments.Argument_List_Type;
      Reply   : D_Bus.Arguments.Argument_List_Type;
   begin
      Request.Append (+Iface);
      Request.Append (+Name);

      Reply :=
        O.Call_Blocking ("org.freedesktop.DBus.Properties", "Get", Request);

      --  Type Consistency Checks
      if Reply.Get_Count = 1 and then Reply.First_Element.Get_Signature = "v"
      then
         Value :=
           D_Bus.Arguments.Containers.Variant_Type (Reply.First_Element);
      else
         raise D_Bus_Error with "Get_Property returned invalid data.";
      end if;
   end Get_Property;

   --------------------------------
   -- Constructor and Destructor --
   --------------------------------
   procedure Create (O : out Client_Object; Node : D_Bus.Types.Obj_Path) is
   begin
      Assert_Invalid (O);

      O.Node  := Node;
      O.Valid := True;
   end Create;

   procedure Set_Destination (O : in out Client_Object; Destination : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid (O);
      O.Destination := To_Unbounded_String (Destination);
   end Set_Destination;

   procedure Destroy (O : in out Client_Object) is
   begin
      Assert_Valid (O);

      O.Valid       := False;
      O.Destination := Ada.Strings.Unbounded.Null_Unbounded_String;
      O.Signals.Clear;
   end Destroy;
end D_Bus.Support.Client;
