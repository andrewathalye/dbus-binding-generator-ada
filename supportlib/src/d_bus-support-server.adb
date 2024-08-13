pragma Ada_2012;

with Ada.Exceptions;
with D_Bus.Support.Message_Handlers;

with D_Bus.Arguments.Basic;

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

   ----------------------------------------------------------------------------
   ----------------------------
   -- SERVER_OBJECT'CLASS --
   ----------------------------
   -------------------------
   -- Object Registration --
   -------------------------
   function Message_Function
     (O : access Root_Object'Class;
      Connection : D_Bus.Connection.Connection_Type;
      Message : D_Bus.Messages.Message_Type)
      return dbus_shared_h.DBusHandlerResult;

   function Message_Function
     (O : access Root_Object'Class;
      Connection : D_Bus.Connection.Connection_Type;
      Message : D_Bus.Messages.Message_Type)
      return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (Connection);
      use D_Bus.Messages;

      --  Messages
      Request : Message_Type renames Message;
      Reply   : Message_Type;

      --  Variables
      SO : Server_Object'Class renames Server_Object'Class (O.all);
   begin
      Assert_Valid (SO);

      --  Ensure that the message is a Method_Call
      --
      --  This prevents leaking other types of messages to
      --  the method call handler table, where they could theoretically
      --  have the right arguments to pass the signature check.
      if Get_Type (Request) /= Method_Call then
         return dbus_shared_h.DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
      end if;

      --  Try to execute the handler
      if not SO.Handlers.Contains (Get_Interface (Request)) then
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
              SO.Handlers.Element (Get_Interface (Request));
         begin
            Handler (SO, Request, Reply);
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
         D_Bus.Connection.Send (SO.Connection, Reply);
      end if;

      --  Free Reply
      --  Request is provided by D_Bus so no need to free
      D_Bus.Messages.Unref (Reply);

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Message_Function;

   --------------
   -- Register --
   --------------
   procedure Register (O : access Server_Object'Class; Handlers : Handler_Map)
   is
   begin
      Assert_Valid (O.all);

      O.Handlers := Handlers;
      D_Bus.Support.Message_Handlers.Register (O, Message_Function'Access);
   end Register;
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
            when False =>
               --  No need to emit a signal. Return early.
               return;
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
         if O.Properties (Iface) (Name).PAccess = Write and not Internal then
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
      Root_Object (O).Create (Connection, Node);
      O.Valid      := True;
   end Create;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (O : in out Server_Object) is
   begin
      Assert_Valid (O);

      Root_Object (O).Destroy;
      O.Properties.Clear;
      O.Handlers.Clear;
   end Destroy;
end D_Bus.Support.Server;
