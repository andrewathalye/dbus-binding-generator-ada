pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

with D_Bus.Arguments.Basic;

with dbus_types_h;
with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;

package body D_Bus.Support.Server is
   ----------
   -- BASE --
   ----------
   ---------------------
   -- Check_Signature --
   ---------------------
   procedure Check_Signature (Actual : String; Expected : String);
   procedure Check_Signature (Actual : String; Expected : String) is
   begin
      if Actual /= Expected then
         raise Invalid_Signature
           with "Expected signature """ & Expected & """ not found. Found """ &
           Actual & """ instead";
      end if;
   end Check_Signature;

   procedure Check_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type; Signature : String)
   is
      Arg_Sig : constant String := Get_Signature (Arguments);
   begin
      Check_Signature (Arg_Sig, Signature);
   end Check_Signature;

   procedure Check_Signature
     (Argument : D_Bus.Arguments.Argument_Type'Class; Signature : String)
   is
   begin
      Check_Signature (Argument.Get_Signature, Signature);
   end Check_Signature;

   ----------------------------------------------------------------------------
   ----------------------------
   -- SERVER_OBJECT'CLASS --
   ----------------------------
   -------------------------
   -- Object Registration --
   -------------------------
   ---------------
   -- Internals --
   ---------------
   type Server_Object_Access is access all Server_Object'Class;
   --  Note: Must _not_ be deallocated as it doesn’t belong to us.
   --  We are 'borrowing' the object.

   type Object_Data_Type is limited record
      Object   : Server_Object_Access;
      Handlers : Handler_Map;
   end record;

   type Object_Data_Access is access all Object_Data_Type;

   package Object_Data_Conversions is new System.Address_To_Access_Conversions
     (Object_Data_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object_Data_Type, Object_Data_Access);

   procedure Unregister_Function
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data  : System.Address);
   pragma Convention (C, Unregister_Function);

   procedure Unregister_Function
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data  : System.Address)
   is
      pragma Unreferenced (Connection);

      OJA : Object_Data_Access :=
        Object_Data_Access (Object_Data_Conversions.To_Pointer (User_Data));
   begin
      Free (OJA);
   end Unregister_Function;

   function Message_Function
     (Connection : access dbus_connection_h.DBusConnection;
      Message : access dbus_message_h.DBusMessage; User_Data : System.Address)
      return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Message_Function);

   function Message_Function
     (Connection : access dbus_connection_h.DBusConnection;
      Message : access dbus_message_h.DBusMessage; User_Data : System.Address)
      return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (Connection);
      use D_Bus.Messages;

      --  Variables
      OJA : constant Object_Data_Access :=
        Object_Data_Access (Object_Data_Conversions.To_Pointer (User_Data));

      --  Messages
      Request : constant Message_Type := Create (Message);
      Reply   : Message_Type;
   begin
      --  Ensure that the message is a Method_Call
      --
      --  This prevents leaking other types of messages to
      --  the method call handler table, where they could theoretically
      --  have the right arguments to pass the signature check.
      if Get_Type (Request) /= Method_Call then
         return dbus_shared_h.DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
      end if;

      --  Try to execute the handler
      if not OJA.Handlers.Contains (Get_Interface (Request)) then
         Reply :=
           New_Error
             (Reply_To      => Request,
              Error_Name    => "org.freedesktop.DBus.Error.UnknownInterface",
              Error_Message =>
                "Interface " & Get_Interface (Request) &
                " not implemented by object " & Get_Path (Request));
      else
         Try_Handler :
         declare
            Handler : constant Handler_Access :=
              OJA.Handlers.Element (Get_Interface (Request));
         begin
            Assert_Valid (OJA.Object.all);
            Handler (OJA.Object.all, Request, Reply);
         exception
            when Unknown_Method =>
               Reply :=
                 New_Error
                   (Reply_To      => Request,
                    Error_Name => "org.freedesktop.DBus.Error.UnknownMethod",
                    Error_Message =>
                      "Method " & Get_Member (Request) &
                      " not implemented by interface " &
                      Get_Interface (Request) & " on object " &
                      Get_Path (Request));

            when X : Unknown_Property =>
               Reply :=
                 New_Error
                   (Reply_To      => Request,
                    Error_Name => "org.freedesktop.DBus.Error.UnknownProperty",
                    Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Invalid_Signature =>
               Reply :=
                 New_Error
                   (Reply_To      => Request,
                    Error_Name    =>
                      "org.freedesktop.DBus.Error.InvalidSignature",
                    Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Property_Read_Only =>
               Reply :=
                 New_Error
                   (Reply_To      => Request,
                    Error_Name    =>
                      "org.freedesktop.DBus.Error.PropertyReadOnly",
                    Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Property_Write_Only =>
               Reply :=
                 New_Error
                   (Reply_To      => Request,
                    Error_Name    => "org.freedesktop.DBus.Error.AccessDenied",
                    Error_Message => Ada.Exceptions.Exception_Message (X));
         end Try_Handler;
      end if;

      --  Send a reply if one is expected
      if not Is_No_Reply_Expected (Request) then
         D_Bus.Connection.Send (OJA.Object.Connection, Reply);
      end if;

      --  Free Reply
      --  Request is provided by D_Bus so no need to free
      D_Bus.Messages.Unref (Reply);

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Message_Function;

   procedure Pad (Arg1 : System.Address) is null;
   pragma Convention (C, Pad);

   --------------
   -- Register --
   --------------
   procedure Register (O : access Server_Object'Class; Handlers : Handler_Map)
   is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO         : Connection_Overlay;
      C_Obj_Path : Interfaces.C.Strings.chars_ptr;

      D_Res : dbus_types_h.dbus_bool_t;

      --!pp off
      VTable : aliased constant dbus_connection_h.DBusObjectPathVTable :=
        (unregister_function => Unregister_Function'Access,
         message_function => Message_Function'Access,
         dbus_internal_pad1 => Pad'Access,
         dbus_internal_pad2 => Pad'Access,
         dbus_internal_pad3 => Pad'Access,
         dbus_internal_pad4 => Pad'Access);
      --!pp on

      OJA : constant Object_Data_Access := new Object_Data_Type;
   begin
      Assert_Valid (O.all);

      if O.Registered then
         raise D_Bus_Error
           with "Object " & To_String (O.Node) & " is already registered";
      end if;

      --  Set up the `Object_Data_Type`
      OJA.Object   := Server_Object_Access (O);
      OJA.Handlers := Handlers;

      CO := Convert (O.Connection);

      C_Obj_Path := Interfaces.C.Strings.New_String (To_String (O.Node));

      D_Res :=
        dbus_connection_h.dbus_connection_register_object_path
          (connection => CO.Thin_Connection, path => C_Obj_Path,
           vtable     => VTable'Access,
           user_data  => Object_Data_Conversions.To_Address (OJA.all'Access));

      Interfaces.C.Strings.Free (C_Obj_Path);

      if D_Res /= 1 then
         raise D_Bus_Error
           with "Failed to register object " & To_String (O.Node);
      end if;

      O.Registered := True;
   end Register;

   ----------------
   -- Unregister --
   ----------------
   procedure Unregister (O : in out Server_Object'Class) is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO         : Connection_Overlay;
      C_Obj_Path : Interfaces.C.Strings.chars_ptr;

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      Assert_Valid (O);

      if not O.Registered then
         raise D_Bus_Error
           with "Object " & To_String (O.Node) & " was not registered";
      end if;

      CO := Convert (O.Connection);

      C_Obj_Path := Interfaces.C.Strings.New_String (To_String (O.Node));

      D_Res :=
        dbus_connection_h.dbus_connection_unregister_object_path
          (connection => CO.Thin_Connection, path => C_Obj_Path);

      Interfaces.C.Strings.Free (C_Obj_Path);

      if D_Res /= 1 then
         raise D_Bus_Error
           with "Failed to unregister object " & To_String (O.Node);
      end if;

      O.Registered := False;
   end Unregister;
   ----------------------------------------------------------------------------
   -------------------
   -- SERVER_OBJECT --
   -------------------
   ---------------
   -- Internals --
   ---------------
   procedure Raise_Unknown_Property
     (O : Server_Object; Iface : String; Name : String);
   procedure Raise_Unknown_Property
     (O : Server_Object; Iface : String; Name : String)
   is
      use D_Bus.Types;
   begin
      raise Unknown_Property
        with "No property " & Iface & "." & Name & " on object " &
        To_String (O.Node);
   end Raise_Unknown_Property;

   -----------------
   -- Send_Signal --
   -----------------
   procedure Send_Signal
     (O    : Server_Object; Iface : String; Name : String;
      Args : D_Bus.Arguments.Argument_List_Type)
   is
   begin
      Assert_Valid (O);

      D_Bus.Connection.Send_Signal
        (Connection => O.Connection, Object_Name => O.Node, Iface => Iface,
         Name       => Name, Args => Args);
   end Send_Signal;

   ------------------
   -- Set_Property --
   ------------------
   procedure Set_Property
     (O         : in out Server_Object; Iface : String; Name : String;
      Value     :        D_Bus.Arguments.Containers.Variant_Type;
      PAccess   :        PAccess_Type        := Unchanged;
      Does_Emit :        Emit_Behaviour_Type := Unchanged)
   is
      use D_Bus.Types;
   begin
      Assert_Valid (O);

      --  Ensure interface exists unless PAccess set
      if not O.Properties.Contains (Iface) then
         case PAccess is
            when Unchanged =>
               Raise_Unknown_Property (O, Iface, Name);
            when others =>
               O.Properties.Insert (Iface, Name_Value_Maps.Empty_Map);
         end case;
      end if;

      --  Ensure property exists unless PAccess set
      if not O.Properties (Iface).Contains (Name) then
         case PAccess is
            when Unchanged =>
               Raise_Unknown_Property (O, Iface, Name);
            when Read | Write | Readwrite =>
               O.Properties (Iface).Insert
                 (Name, Property_Type'(PAccess, Does_Emit, Value));
               goto Try_Emit_Signal;
         end case;
      end if;

      --  Ensure property is writable
      if PAccess = Unchanged then
         case O.Properties (Iface) (Name).PAccess is
            when Read =>
               raise Property_Read_Only
                 with "Property " & Iface & "." & Name & " on object " &
                 To_String (O.Node) & " is read only";
            when Write | Readwrite =>
               null;
         end case;
      end if;

      --  Check the property’s signature and ensure the new value
      --  has the same signature.
      Check_Signature :
      declare
         Original_Signature : constant String :=
           O.Properties (Iface) (Name).Value.Get_Argument.Get_Signature;
         New_Signature : constant String := Value.Get_Argument.Get_Signature;
      begin
         if Original_Signature /= New_Signature then
            raise Invalid_Signature
              with "New value for property " & Iface & "." & Name &
              " on object " & To_String (O.Node) & " has wrong signature: " &
              New_Signature & " != " & Original_Signature;
         end if;
      end Check_Signature;

      O.Properties (Iface) (Name).Value := Value;

      --  Send out the PropertiesChanged signal
      <<Try_Emit_Signal>>

      --  No need to emit a signal in this case
      if O.Properties (Iface) (Name).Emit_Behaviour = False then
         return;
      end if;

      Emit_Signal :
      declare
         use type D_Bus.Arguments.Basic.String_Type;

         Property_Array    : D_Bus.Arguments.Containers.Array_Type;
         Invalidated_Array : D_Bus.Arguments.Containers.Array_Type;
         Args              : D_Bus.Arguments.Argument_List_Type;
      begin
         --  Handle the case where either array could be empty
         --  a{sv} [{name, value}] | changed_properties
         Property_Array.Set_Signature ("{sv}");

         --  as | invalidated_properties
         Invalidated_Array.Set_Signature ("s");

         --  Add a single element to the correct array
         case O.Properties (Iface) (Name).Emit_Behaviour is
            when True =>
               declare
                  use D_Bus.Arguments.Containers;
                  Property_Dict : constant Dict_Entry_Type :=
                    Create (+Name, Value);
               begin
                  Property_Array.Append (Property_Dict);
               end;
            when Invalidates =>
               Invalidated_Array.Append (+Name);

               --  Impossible to get here
            when False =>
               null;
         end case;

         --  sa{sv}as
         --  interface_name changed_properties invalidated_properties
         Args.Append (+Iface);
         Args.Append (Property_Array);
         Args.Append (Invalidated_Array);

         O.Send_Signal
           (Iface => "org.freedesktop.DBus.Properties",
            Name  => "PropertiesChanged", Args => Args);
      end Emit_Signal;
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------
   procedure Get_Property
     (O        :     Server_Object; Iface : String; Name : String;
      Value    : out D_Bus.Arguments.Containers.Variant_Type;
      Internal :     Boolean := False)
   is
      use D_Bus.Types;
   begin
      Assert_Valid (O);

      --  Ensure property exists and is readable
      if O.Properties.Contains (Iface)
        and then O.Properties (Iface).Contains (Name)
      then
         --  If being called locally, bypass access check
         if not Internal and then O.Properties (Iface) (Name).PAccess = Write
         then
            raise Property_Write_Only
              with "Property " & Iface & "." & Name & " on " &
              To_String (O.Node) & " is not readable";
         end if;
      else
         Raise_Unknown_Property (O, Iface, Name);
      end if;

      Value := O.Properties.Element (Iface) (Name).Value;
   end Get_Property;

   ------------------------
   -- Get_All_Properties --
   ------------------------
   procedure Get_All_Properties
     (O          :     Server_Object; Iface : String;
      Properties : out D_Bus.Arguments.Containers.Array_Type)
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Dict_Entry : D_Bus.Arguments.Containers.Dict_Entry_Type;
   begin
      Assert_Valid (O);

      --  Handle the case where no properties are defined on Iface
      --  (this is not erroneous)
      Properties.Set_Signature ("{sv}");
      if not O.Properties.Contains (Iface) then
         return;
      end if;

      --  Add each property to the list
      for Cursor in O.Properties (Iface).Iterate loop
         --  Access check
         if O.Properties (Iface) (Cursor).PAccess in Read | Readwrite then
            Dict_Entry :=
              D_Bus.Arguments.Containers.Create
                (+Name_Value_Maps.Key (Cursor),
                 O.Properties (Iface) (Cursor).Value);

            Properties.Append (Dict_Entry);
         end if;
      end loop;
   end Get_All_Properties;

   ------------
   -- Create --
   ------------
   procedure Create
     (O    : out Server_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path)
   is
   begin
      Assert_Invalid (O);

      O.Connection := Connection;
      O.Node       := Node;
      O.Valid      := True;
   end Create;

   -------------
   -- Destroy --
   -------------
   overriding procedure Destroy (O : in out Server_Object) is
      use D_Bus.Types;
   begin
      Assert_Valid (O);

      if O.Registered then
         raise D_Bus_Error
           with "Unregister object " & To_String (O.Node) &
           " before calling Destroy.";
      end if;

      O.Valid := False;
      O.Properties.Clear;
   end Destroy;
end D_Bus.Support.Server;
