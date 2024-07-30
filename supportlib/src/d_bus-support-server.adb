pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

with D_Bus.Arguments.Basic;
with D_Bus.Connection.G_Main;

with dbus_types_h;
with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;
with dbus_bus_h;
with dbus_errors_h;

package body D_Bus.Support.Server is
   ----------
   -- BASE --
   ----------
   ------------------
   -- Request_Name --
   ------------------
   procedure Request_Name (Name : String) is
   begin
      --  Note: Changing the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Bus.Connection.Request_Name (Connection, Name);
      end Critical_Section;
      D_Bus_Lock.Release;
   end Request_Name;

   ---------------------
   -- Check_Signature --
   ---------------------
   procedure Check_Signature
     (Actual : String;
      Expected : String);
   procedure Check_Signature
     (Actual : String;
      Expected : String)
   is
   begin
      if Actual /= Expected then
         raise Invalid_Signature with
           "Expected signature """ & Expected & """ not found. Found """ &
            Actual & """ instead";
      end if;
   end Check_Signature;

   procedure Check_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type;
      Signature : String)
   is
      Arg_Sig : constant String := Get_Signature (Arguments);
   begin
      Check_Signature (Arg_Sig, Signature);
   end Check_Signature;

   procedure Check_Signature
     (Argument : D_Bus.Arguments.Argument_Type'Class;
      Signature : String)
   is
   begin
      Check_Signature (Argument.Get_Signature, Signature);
   end Check_Signature;

   ------------------
   -- Release_Name --
   ------------------
   procedure Release_Name (Name : String) is
      use type Interfaces.C.int;

      C_Res : Interfaces.C.int;
      D_Err : aliased dbus_errors_h.DBusError;
      C_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Name);
      CO : constant Connection_Overlay := Convert (Connection);
   begin
      --  Note: Changing the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         C_Res := dbus_bus_h.dbus_bus_release_name
           (connection => CO.Thin_Connection,
            name => C_Name,
            error => D_Err'Access);
      end Critical_Section;
      D_Bus_Lock.Release;

      Interfaces.C.Strings.Free (C_Name);

      if C_Res /= dbus_shared_h.DBUS_RELEASE_NAME_REPLY_RELEASED then
         raise D_Bus_Error with
            "Unable to release name " & Name & " (" & C_Res'Image & ")";
      end if;
   end Release_Name;

   -----------------------
   -- Setup_With_G_Main --
   -----------------------
   procedure Setup_With_G_Main is
   begin
      --  Note: Changing the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Bus.Connection.G_Main.Setup_With_G_Main (Connection);
      end Critical_Section;
      D_Bus_Lock.Release;
   end Setup_With_G_Main;

   -------------------
   -- Run_Iteration --
   -------------------
   procedure Run_Iteration is
      function g_main_context_iteration
        (Context : System.Address;
         Blocking : Integer) return Integer;
      pragma Import (C, g_main_context_iteration);

      Discard : Integer;
   begin
      Discard := g_main_context_iteration (System.Null_Address, 1);
   end Run_Iteration;

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
      Object : Server_Object_Access;
      Handlers : Handler_Map;
   end record;

   type Object_Data_Access is access all Object_Data_Type;

   package Object_Data_Conversions is new System.Address_To_Access_Conversions
     (Object_Data_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object_Data_Type, Object_Data_Access);

   procedure Unregister_Function
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data : System.Address);
   pragma Convention (C, Unregister_Function);

   procedure Unregister_Function
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data : System.Address)
   is
      pragma Unreferenced (Connection);

      OJA : Object_Data_Access :=
         Object_Data_Access
           (Object_Data_Conversions.To_Pointer (User_Data));
   begin
      Free (OJA);
   end Unregister_Function;

   function Message_Function
     (Connection : access dbus_connection_h.DBusConnection;
      Message : access dbus_message_h.DBusMessage;
      User_Data : System.Address) return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Message_Function);

   function Message_Function
     (Connection : access dbus_connection_h.DBusConnection;
      Message : access dbus_message_h.DBusMessage;
      User_Data : System.Address) return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (Connection);
      use D_Bus.Messages;

      --  Variables
      OJA : constant Object_Data_Access :=
        Object_Data_Access
          (Object_Data_Conversions.To_Pointer (User_Data));

      --  Messages
      Request : constant Message_Type := Create (Message);
      Reply : Message_Type;
   begin
      --  Try to execute the handler
      if not OJA.Handlers.Contains (Get_Interface (Request)) then
         Reply := New_Error
           (Reply_To => Request,
            Error_Name => "org.freedesktop.DBus.Error.UnknownInterface",
            Error_Message =>
               "Interface " &
                Get_Interface (Request) &
                " not implemented by object " &
                Get_Path (Request));
      else
         Try_Handler :
         declare
            Handler : constant Handler_Access := OJA.Handlers.Element
              (Get_Interface (Request));
         begin
            Assert_Valid (OJA.Object.all);
            Handler (OJA.Object.all, Request, Reply);
         exception
            when Unknown_Method =>
               Reply := New_Error
                 (Reply_To => Request,
                  Error_Name => "org.freedesktop.DBus.Error.UnknownMethod",
                  Error_Message =>
                     "Method " & Get_Member (Request) &
                     " not implemented by interface " &
                     Get_Interface (Request) &
                     " on object " & Get_Path (Request));

            when X : Unknown_Property =>
               Reply := New_Error
                 (Reply_To => Request,
                  Error_Name => "org.freedesktop.DBus.Error.UnknownProperty",
                  Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Invalid_Signature =>
               Reply := New_Error
                 (Reply_To => Request,
                  Error_Name => "org.freedesktop.DBus.Error.InvalidSignature",
                  Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Property_Read_Only =>
               Reply := New_Error
                 (Reply_To => Request,
                  Error_Name => "org.freedesktop.DBus.Error.PropertyReadOnly",
                  Error_Message => Ada.Exceptions.Exception_Message (X));

            when X : Property_Write_Only =>
               Reply := New_Error
                 (Reply_To => Request,
                  Error_Name => "org.freedesktop.DBus.Error.AccessDenied",
                  Error_Message => Ada.Exceptions.Exception_Message (X));
         end Try_Handler;
      end if;

      --  Send a reply
      --  Note: Sending a message is modifying global state
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Bus.Connection.Send (D_Bus.Support.Connection, Reply);
      end Critical_Section;
      D_Bus_Lock.Release;

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Message_Function;

   procedure Pad (Arg1 : System.Address) is null;
   pragma Convention (C, Pad);

   --------------
   -- Register --
   --------------
   procedure Register
     (O : access Server_Object'Class; Handlers : Handler_Map)
   is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO : Connection_Overlay;
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

      --  Set up the `Object_Data_Type`
      OJA.Object := Server_Object_Access (O);
      OJA.Handlers := Handlers;

      CO := Convert (Connection);

      C_Obj_Path := Interfaces.C.Strings.New_String
        (To_String (O.Node));

      --  Note: Registering an object on the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Res := dbus_connection_h.dbus_connection_register_object_path
           (connection => CO.Thin_Connection,
            path => C_Obj_Path,
            vtable => VTable'Access,
            user_data =>
               Object_Data_Conversions.To_Address (OJA.all'Access));
      end Critical_Section;
      D_Bus_Lock.Release;

      Interfaces.C.Strings.Free (C_Obj_Path);

      if D_Res /= 1 then
         raise D_Bus_Error with
            "Failed to register object " & To_String (O.Node);
      end if;
   end Register;

   ----------------
   -- Unregister --
   ----------------
   procedure Unregister (O : Server_Object'Class)
   is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO : Connection_Overlay;
      C_Obj_Path : Interfaces.C.Strings.chars_ptr;

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      Assert_Valid (O);

      CO := Convert (Connection);

      C_Obj_Path := Interfaces.C.Strings.New_String
        (To_String (O.Node));

      --  Note: Unregistering a global object
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Res := dbus_connection_h.dbus_connection_unregister_object_path
           (connection => CO.Thin_Connection,
            path => C_Obj_Path);
      end Critical_Section;
      D_Bus_Lock.Release;

      Interfaces.C.Strings.Free (C_Obj_Path);

      if D_Res /= 1 then
         raise D_Bus_Error with
            "Failed to unregister object " & To_String (O.Node);
      end if;
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
      raise Unknown_Property with
        "No property " & Iface & "." & Name & " on object " &
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

      --  Note: using the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Bus.Connection.Send_Signal
           (Connection => Connection,
            Object_Name => O.Node,
            Iface => Iface,
            Name => Name,
            Args => Args);
      end Critical_Section;
      D_Bus_Lock.Release;
   end Send_Signal;

   ------------------
   -- Set_Property --
   ------------------
   procedure Set_Property
     (O     : in out Server_Object; Iface : String; Name : String;
      Value :        D_Bus.Arguments.Containers.Variant_Type;
      PAccess : Access_Type := Unchanged)
   is
      use type D_Bus.Arguments.Basic.String_Type;
      use D_Bus.Types;

      Property_Array : D_Bus.Arguments.Containers.Array_Type;
      Invalidated_Array : D_Bus.Arguments.Containers.Array_Type;
      Args : D_Bus.Arguments.Argument_List_Type;
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
               O.Properties (Iface).Insert (Name, (PAccess, Value));
               goto Send_Signal;
         end case;
      end if;

      --  Ensure property is writable
      if PAccess = Unchanged then
         case O.Properties (Iface) (Name).PAccess is
            when Read =>
               raise Property_Read_Only with
                  "Property " & Iface & "." & Name & " on object " &
                  To_String (O.Node) & " is read only";
            when Write | Readwrite => null;
         end case;
      end if;

      --  Check the property’s signature and ensure the new value
      --  has the same signature.
      Check_Signature :
      declare
         Original_Signature : constant String :=
           O.Properties (Iface) (Name).Value.Get_Argument.Get_Signature;
         New_Signature : constant String :=
           Value.Get_Argument.Get_Signature;
      begin
         if Original_Signature /= New_Signature then
            raise Invalid_Signature with
               "New value for property " & Iface & "." & Name &
               " on object " & To_String (O.Node) &
               " has wrong signature: " & New_Signature &
               " != " & Original_Signature;
         end if;
      end Check_Signature;

      O.Properties (Iface) (Name).Value := Value;

      --  Send out the PropertiesChanged signal
      <<Send_Signal>>

      --  a{sv} [{name, value}] | changed_properties
      declare
         Property_Dict : D_Bus.Arguments.Containers.Dict_Entry_Type;
      begin
         Property_Dict := D_Bus.Arguments.Containers.Create (+Name, Value);
         Property_Array.Append (Property_Dict);
      end;

      --  as | invalidated_properties
      Invalidated_Array.Set_Signature ("s");
      --  Note: we don’t currently use this

      --  s a{sv} as | interface_name changed_properties invalidated_properties
      Args.Append (+Iface);
      Args.Append (Property_Array);
      Args.Append (Invalidated_Array);

      O.Send_Signal
        (Iface => "org.freedesktop.DBus.Properties",
         Name => "PropertiesChanged",
         Args => Args);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------
   procedure Get_Property
     (O     : Server_Object; Iface : String; Name : String;
      Value :    out D_Bus.Arguments.Containers.Variant_Type;
      Internal : Boolean := False)
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
            raise Property_Write_Only with
               "Property " & Iface & "." & Name & " on " & To_String (O.Node) &
               " is not readable";
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
       (O : Server_Object;
        Iface : String;
        Properties : out D_Bus.Arguments.Containers.Array_Type)
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Dict_Entry : D_Bus.Arguments.Containers.Dict_Entry_Type;
   begin
      Assert_Valid (O);

      --  Handle the case where no properties are defined on Iface
      Properties.Set_Signature ("{sv}");
      if not O.Properties.Contains (Iface) then
         return;
      end if;

      --  Add each property to the list
      for Cursor in O.Properties (Iface).Iterate loop
         --  Access check
         if O.Properties (Iface) (Cursor).PAccess in Read | Readwrite
         then
            Dict_Entry := D_Bus.Arguments.Containers.Create
              (+Name_Value_Maps.Key (Cursor),
               O.Properties (Iface) (Cursor).Value);
         end if;

         Properties.Append (Dict_Entry);
      end loop;
   end Get_All_Properties;

   ------------
   -- Create --
   ------------
   procedure Create (O : out Server_Object; Node : D_Bus.Types.Obj_Path) is
   begin
      Assert_Invalid (O);

      O.Node := Node;
      O.Valid := True;
   end Create;

   -------------
   -- Destroy --
   -------------
   overriding procedure Destroy (O : in out Server_Object) is
   begin
      Assert_Valid (O);

      O.Valid := False;
      O.Properties.Clear;
   end Destroy;
end D_Bus.Support.Server;
