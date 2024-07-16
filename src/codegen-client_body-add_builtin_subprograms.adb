with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

procedure Codegen.Client_Body.Add_Builtin_Subprograms (Pkg : Ada_Package_Type)
is
begin
   Start_Procedure ("Assert_Has_Destination");
   Begin_Code;
   Start_If ("Destination = Ada.Strings.Unbounded.Null_Unbounded_String");
   Raise_Exception
     ("No_Destination", "No destination set for interface " & (+Pkg.Iface));
   End_If;
   End_Procedure ("Assert_Has_Destination");
   New_Line;

   Start_Procedure ("Set_Destination (Dest : String)");
   Begin_Code;
   Assign ("Destination", "Ada.Strings.Unbounded.To_Unbounded_String (Dest)");
   End_Procedure ("Set_Destination");
   New_Line;

   Start_Procedure
     ("Call_Remote (Iface : String; Method : String" &
      "; Request : D_Bus.Arguments.Argument_List_Type" &
      "; Reply : out D_Bus.Arguments.Argument_List_Type)");
   Begin_Code;
   Call ("Assert_Has_Destination");
   Assign
     ("Reply",
      "D_Bus.Connection.Call_Blocking (Connection" &
      ", Ada.Strings.Unbounded.To_String (Destination), Path, Iface" &
      ", Method, D_Bus.Connection.Default_Timeout, Request)");
   End_Procedure ("Call_Remote");
   New_Line;

   Start_Function
     ("Get_Property (Property : String)",
      "D_Bus.Arguments.Argument_Type'Class");
   Use_Entity ("D_Bus.Arguments.Basic");

   Declare_Entity ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
   Declare_Entity ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
   Declare_Entity ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
   Begin_Code;
   Call ("D_Bus.Arguments.Append (Request_Args, +Iface)");
   Call ("D_Bus.Arguments.Append (Request_Args, +Property)");
   Call
     ("Call_Remote (""org.freedesktop.DBus.Properties""" &
      ", ""Get"", Request_Args, Reply_Args)");
   Assign
     ("Variant",
      "D_Bus.Arguments.Containers.Variant_Type (" &
      "D_Bus.Arguments.First_Element (Reply_Args))");
   Return_Entity ("Variant.Get_Argument");
   End_Function ("Get_Property");
   New_Line;

   Start_Procedure
     ("Set_Property (Property : String;" &
      " Value : D_Bus.Arguments.Argument_Type'Class)");
   Use_Entity ("D_Bus.Arguments.Basic");

   Declare_Entity ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
   Declare_Entity ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
   Declare_Entity ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
   Begin_Code;
   Assign ("Variant", "D_Bus.Arguments.Containers.Create (Value)");

   Call ("D_Bus.Arguments.Append (Request_Args, +Iface)");
   Call ("D_Bus.Arguments.Append (Request_Args, +Property)");
   Call ("D_Bus.Arguments.Append (Request_Args, Variant)");
   Call
     ("Call_Remote (""org.freedesktop.DBus.Properties""" &
      ", ""Set"" , Request_Args, Reply_Args)");
   End_Code;
end Codegen.Client_Body.Add_Builtin_Subprograms;
