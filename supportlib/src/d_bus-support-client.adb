pragma Ada_2012;

with Ada.Containers.Doubly_Linked_Lists;
with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;

with D_Bus.Arguments.Basic;
with D_Bus.Connection;
with D_Bus.Messages;

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

   ----------------------
   -- Signal Internals --
   ----------------------
   type Message_Reference is record
      Connection : D_Bus.Connection.Connection_Type;
      Msg        : D_Bus.Messages.Message_Type;
   end record;

   package MRL is new Ada.Containers.Doubly_Linked_Lists (Message_Reference);

   protected Msg_Box is
      function Length return Ada.Containers.Count_Type;
      procedure Enqueue (R : Message_Reference);
      procedure Consume (R : out Message_Reference);
      entry Lock;
      entry Unlock;
   private
      Data   : MRL.List;
      Locked : Boolean := False;
   end Msg_Box;

   protected body Msg_Box is
      function Length return Ada.Containers.Count_Type is (Data.Length);

      procedure Enqueue (R : Message_Reference) is
      begin
         Data.Append (R);
      end Enqueue;

      procedure Consume (R : out Message_Reference) is
         Pos : MRL.Cursor := Data.First;
      begin
         if not MRL.Has_Element (Pos) then
            raise D_Bus_Error with "No message in Msg_Box!";
         end if;

         R := MRL.Element (Pos);
         Data.Delete (Pos);
      end Consume;

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      entry Unlock when Locked is
      begin
         Locked := False;
      end Unlock;
   end Msg_Box;

   --  Callback Implementation
   type Message_Access is access all D_Bus.Messages.Message_Type;
   package MA_Conversions is new System.Address_To_Access_Conversions
     (D_Bus.Messages.Message_Type);

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

      use type D_Bus.Messages.Message_Variant;

      M : constant D_Bus.Messages.Message_Type := D_Bus.Messages.Create (Msg);
      Result : constant Message_Access              :=
        Message_Access (MA_Conversions.To_Pointer (Usr_Data));
   begin
      Result.all := D_Bus.Messages.Null_Message;

      --  Ref the message here so it doesn’t get deallocated
      if D_Bus.Messages.Get_Type (M) = D_Bus.Messages.Signal then
         Result.all := D_Bus.Messages.Ref (M);
         return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
      end if;

      --  Don’t consume anything but signals
      return dbus_shared_h.DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
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

      if O.Signals.Contains (Iface & ":" & Name) then
         raise D_Bus_Error with "Asked to register duplicate signal.";
      end if;

      D_Bus.Connection.Add_Match (O.Connection, Match_Rule);

      O.Signals.Insert (Iface & ":" & Name, Match_Rule);
   end Register_Signal;

   procedure Unregister_Signal
     (O : in out Client_Object; Iface : String; Name : String)
   is
   begin
      Assert_Valid (O);
      if not O.Signals.Contains (Iface & ":" & Name) then
         raise D_Bus_Error with "Asked to unregister nonexisting signal.";
      end if;

      D_Bus.Connection.Remove_Match
        (O.Connection, O.Signals (Iface & ":" & Name));

      O.Signals.Delete (Iface & ":" & Name);
   end Unregister_Signal;

   ------------------
   -- Await_Signal --
   ------------------
   function Await_Signal
     (O : Client_Object; Iface : String; Name : String)
      return D_Bus.Arguments.Argument_List_Type
   is
      use type D_Bus.Messages.Message_Type;

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
   begin
      Assert_Valid (O);

      --  It is an error to wait for a signal which hasn’t been registered.
      if not O.Signals.Contains (Iface & ":" & Name) then
         raise D_Bus_Error
           with "Cannot wait for unregistered signal " & Iface & "." & Name;
      end if;

      --  Check for a cached message first
      Search_Cache :
      declare
         use type D_Bus.Connection.Connection_Type;

         Ref : Message_Reference;
      begin
         --  Stop any other tasks from trying to consume messages
         Msg_Box.Lock;
         for I in 1 .. Msg_Box.Length loop
            Msg_Box.Consume (Ref);

            if Ref.Connection = O.Connection
              and then Is_Match (Ref.Msg, O.Node, Iface, Name)
            then
               Msg_Box.Unlock;

               --  Prevent memory leaks
               return Arguments : D_Bus.Arguments.Argument_List_Type do
                  Arguments := D_Bus.Messages.Get_Arguments (Ref.Msg);
                  D_Bus.Messages.Unref (Ref.Msg);
               end return;
            else
               Msg_Box.Enqueue (Ref);
            end if;
         end loop;
         Msg_Box.Unlock;
      end Search_Cache;

      Wait_For_Signal :
      declare
         use type Interfaces.C.int;
         use type dbus_types_h.dbus_bool_t;

         CO           : constant Connection_Overlay := Convert (O.Connection);
         Callback_Msg : aliased D_Bus.Messages.Message_Type;
      begin
         --  Set up filter
         if dbus_connection_h.dbus_connection_add_filter
             (connection => CO.Thin_Connection, c_function => Call_Back'Access,
              user_data          => Callback_Msg'Address,
              free_data_function => Null_Free_Data'Access) =
           0
         then
            raise D_Bus_Error with "Could not add connection filter";
         end if;

         --  Dispatch
         Dispatch :
         loop
            Callback_Msg := D_Bus.Messages.Null_Message;

            if dbus_connection_h.dbus_connection_read_write_dispatch
                (CO.Thin_Connection, -1) =
              0
            then
               raise D_Bus_Error with "Dispatch failed";
            end if;

            --  Exit if the message matches or save it for later.
            if Callback_Msg /= D_Bus.Messages.Null_Message then
               if Is_Match (Callback_Msg, O.Node, Iface, Name) then
                  exit Dispatch;
               else
                  Msg_Box.Enqueue ((O.Connection, Callback_Msg));
               end if;
            end if;
         end loop Dispatch;

         --  Remove filter
         dbus_connection_h.dbus_connection_remove_filter
           (CO.Thin_Connection, Call_Back'Access, Callback_Msg'Address);

         --  Prevent memory leaks
         return Arguments : D_Bus.Arguments.Argument_List_Type do
            Arguments := D_Bus.Messages.Get_Arguments (Callback_Msg);
            D_Bus.Messages.Unref (Callback_Msg);
         end return;
      end Wait_For_Signal;
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
   begin
      Assert_Valid (O);
      if O.Destination = Null_Unbounded_String then
         raise D_Bus_Error
           with "Asked to call method on object with no destination.";
      end if;

      return
        D_Bus.Connection.Call_Blocking
          (Connection  => O.Connection,
           Destination => To_String (O.Destination), Path => O.Node,
           Iface       => Iface, Method => Method, Args => Args);
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
   procedure Create
     (O    : out Client_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path)
   is
   begin
      Assert_Invalid (O);

      O.Connection := Connection;
      O.Node       := Node;
      O.Valid      := True;
   end Create;

   function Destination (O : Client_Object) return String is
   begin
      Assert_Valid (O);

      return Ada.Strings.Unbounded.To_String (O.Destination);
   end Destination;

   procedure Set_Destination (O : in out Client_Object; Destination : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid (O);

      O.Destination := To_Unbounded_String (Destination);
   end Set_Destination;

   procedure Destroy (O : in out Client_Object) is
      use D_Bus.Types;
   begin
      Assert_Valid (O);

      if not O.Signals.Is_Empty then
         raise D_Bus_Error
           with "Cannot destroy object " & To_String (O.Node) &
           " because it has registered signals.";
      end if;

      O.Valid       := False;
      O.Destination := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Destroy;
end D_Bus.Support.Client;
