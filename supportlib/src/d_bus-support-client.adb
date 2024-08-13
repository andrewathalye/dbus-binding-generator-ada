pragma Ada_2012;

with D_Bus.Arguments.Basic;
with D_Bus.Connection;

with Interfaces.C;

with dbus_shared_h;
with dbus_types_h;

package body D_Bus.Support.Client is
   ----------------------
   -- Signal Internals --
   ----------------------
   function Signal_Handler
     (O : access Root_Object'Class;
      Connection : D_Bus.Connection.Connection_Type;
      Message    : D_Bus.Messages.Message_Type)
     return dbus_shared_h.DBusHandlerResult;

   function Signal_Handler
     (O : access Root_Object'Class;
      Connection : D_Bus.Connection.Connection_Type;
      Message    : D_Bus.Messages.Message_Type)
     return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (Connection);

      use D_Bus.Messages;
   begin
      Assert_Valid (O.all);

      --  Only process signals
      case Get_Type (Message) is
         when Signal =>
            Client_Object (O.all).Last_Signal := D_Bus.Messages.Ref (Message);
            return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
         when others =>
            return dbus_shared_h.DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
      end case;
   end Signal_Handler;

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

   ------------------
   -- Await_Signal --
   ------------------
   function Await_Signal
     (O : in out Client_Object;
      Iface : String;
      Name : String)
      return D_Bus.Arguments.Argument_List_Type
   is
      use D_Bus.Support.Message_Handlers;
      use D_Bus.Types;
      use dbus_connection_h;

      use type D_Bus.Messages.Message_Type;

      use type Interfaces.C.int;
      use type dbus_types_h.dbus_bool_t;

      CO : constant Connection_Overlay := Convert (O.Connection);

      Signal_Hash : constant Message_Hash := Hash
        (Path => To_String (O.Node),
         Iface => Iface,
         Member => Name);

      Msg : D_Bus.Messages.Message_Type;
   begin
      Assert_Valid (O);

      if not O.Registered then
         raise D_Bus_Error with
            "Object must be registered before waiting for signals.";
      end if;

      --  Check cache
      begin
         O.Messages.Consume (Signal_Hash, Msg);

         return A : D_Bus.Arguments.Argument_List_Type do
            A := D_Bus.Messages.Get_Arguments (Msg);
            D_Bus.Messages.Unref (Msg);
         end return;
      exception
         when D_Bus.Support.Message_Handlers.No_Cached_Message => null;
      end;

      --  Fetch live
      while dbus_connection_read_write_dispatch (CO.Thin_Connection, -1) = 1
      loop
         if O.Last_Signal /= D_Bus.Messages.Null_Message then
            Msg := O.Last_Signal;
            O.Last_Signal := D_Bus.Messages.Null_Message;

            if Hash (Msg) = Signal_Hash then
               return A : D_Bus.Arguments.Argument_List_Type do
                  A := D_Bus.Messages.Get_Arguments (Msg);

                  --  Deallocate the message
                  D_Bus.Messages.Unref (Msg);
               end return;
            else
               O.Messages.Enqueue (Msg);

               --  The message queue now holds ownership
               D_Bus.Messages.Unref (Msg);
            end if;
         end if;
      end loop;

      raise D_Bus_Error with "Dispatch failed";
   end Await_Signal;

   ------------------
   -- Purge_Signal --
   ------------------
   procedure Purge_Signal
     (O : in out Client_Object;
      Iface : String; Name : String)
   is
      use D_Bus.Types;

      use dbus_connection_h;

      CO : constant Connection_Overlay := Convert (O.Connection);
   begin
      Assert_Valid (O);

      --  While there is data to dispatch, do so.
      while
        dbus_connection_dispatch (CO.Thin_Connection) /= DBUS_DISPATCH_COMPLETE
      loop
         null;
      end loop;

      O.Messages.Clear
        (D_Bus.Support.Message_Handlers.Hash
           (Path       => To_String (O.Node),
            Iface      => Iface,
            Member     => Name));
   end Purge_Signal;

   ----------------
   -- OO Methods --
   ----------------
   procedure Assert_Valid_Call (O : Client_Object);
   procedure Assert_Valid_Call (O : Client_Object) is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid (O);

      if O.Destination = Null_Unbounded_String then
         raise D_Bus_Error
           with "Asked to call method on object with no destination.";
      end if;
   end Assert_Valid_Call;

   function Call_Blocking
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List)
      return D_Bus.Arguments.Argument_List_Type
   is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid_Call (O);

      return
        D_Bus.Connection.Call_Blocking
          (Connection  => O.Connection,
           Destination => To_String (O.Destination), Path => O.Node,
           Iface       => Iface, Method => Method, Args => Args);
   end Call_Blocking;

   procedure Call_No_Reply
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List)
   is
      use Ada.Strings.Unbounded;
   begin
      Assert_Valid_Call (O);

      D_Bus.Connection.Call_No_Reply
        (Connection => O.Connection, Destination => To_String (O.Destination),
         Path       => O.Node, Iface => Iface, Method => Method, Args => Args);
   end Call_No_Reply;

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
     (O          : out Client_Object;
      Connection : D_Bus.Connection.Connection_Type;
      Node       : D_Bus.Types.Obj_Path)
   is
   begin
      Root_Object (O).Create (Connection, Node);

      O.Valid := True;
   end Create;

   procedure Register (O : access Client_Object'Class) is
   begin
      D_Bus.Support.Message_Handlers.Register (O, Signal_Handler'Access);
   end Register;

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
   begin
      Assert_Valid (O);
      Root_Object (O).Destroy;
      O.Destination := Ada.Strings.Unbounded.Null_Unbounded_String;
      O.Messages.Clear;
   end Destroy;
end D_Bus.Support.Client;
