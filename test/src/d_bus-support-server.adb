pragma Ada_2005;

with D_Bus.Arguments.Basic;

package body D_Bus.Support.Server is
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

   -----------
   -- Reply --
   -----------
   procedure Reply
     (O    : Server_Object; Request : D_Bus.Messages.Message_Type;
      Args : D_Bus.Arguments.Argument_List_Type)
   is
      Reply : D_Bus.Messages.Message_Type;
   begin
      Assert_Valid (O);

      Reply := D_Bus.Messages.New_Method_Return (Request);
      D_Bus.Messages.Add_Arguments (Reply, Args);

      --  Note: using the global connection
      D_Bus_Lock.Acquire;
      Critical_Section : begin
         D_Bus.Connection.Send (Connection, Reply);
      end Critical_Section;
      D_Bus_Lock.Release;
   end Reply;

   ------------------
   -- Set_Property --
   ------------------
   procedure Set_Property
     (O     : in out Server_Object; Iface : String; Name : String;
      Value :        D_Bus.Arguments.Containers.Variant_Type)
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Property_Dict : D_Bus.Arguments.Containers.Dict_Entry_Type;
      Property_Array : D_Bus.Arguments.Containers.Array_Type;
      Empty_Array : D_Bus.Arguments.Containers.Array_Type;
      Args : D_Bus.Arguments.Argument_List_Type;
   begin
      Assert_Valid (O);

      --  Update the object-local list
      O.Properties.Insert (Iface & ":" & Name, Value);

      --  Send out the PropertiesChanged signal
      --  a{sv} | [{name, value}]
      Property_Dict := D_Bus.Arguments.Containers.Create
        (Key => +Name,
         Value => Value);
      Property_Array.Append (Property_Dict);

      --  as | invalidated_properties
      Empty_Array.Set_Signature ("s");

      --  s a{sv} as | interface_name changed_properties invalidated_properties
      Args.Append (+Iface);
      Args.Append (Property_Array);
      Args.Append (Empty_Array);

      --  Note: Send_Signal is already locked
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
      Value :    out D_Bus.Arguments.Containers.Variant_Type)
   is
      use D_Bus.Types;
   begin
      Assert_Valid (O);

      if not O.Properties.Contains (Iface & ":" & "Name") then
         raise D_Bus_Error with
            "Property " & Iface & "." & Name & " not found for " &
            To_String (O.Node);
      end if;

      Value := O.Properties.Element (Iface & ":" & "Name");
   end Get_Property;

   ------------
   -- Create --
   ------------
   procedure Create (O : out Server_Object; Node : Unbounded_Object_Path) is
      use Ada.Strings.Unbounded;
      use type D_Bus.Types.Obj_Path;
   begin
      Assert_Invalid (O);

      O.Node := +To_String (Node);
      O.Valid := True;
   end Create;

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
