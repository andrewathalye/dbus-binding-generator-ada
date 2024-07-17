with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

procedure Codegen.Client_Body.Add_Builtins (Pkg : Ada_Package_Type)
is
begin
   if Pkg.Methods.Is_Empty and Pkg.Properties.Is_Empty and Pkg.Signals.Is_Empty
   then
      null;
   else
      --!pp off
      Large_Comment ("Builtins");
      Declare_Entity
        ("Connection",
         "constant D_Bus.Connection.Connection_Type",
         "D_Bus.Connection.Connect");
      Declare_Entity
       ("Destination", "Ada.Strings.Unbounded.Unbounded_String");
      Declare_Entity
        ("Path", "constant D_Bus.Types.Obj_Path",
         "+""" & (+Pkg.Node) & """");
      Declare_Entity
        ("Iface", "constant String",
         """" & (+Pkg.Iface) & """");

      Start_Procedure ("Assert_Has_Destination");
      Begin_Code;
         Start_If
           ("Destination = Ada.Strings.Unbounded.Null_Unbounded_String");
            Raise_Exception
              ("No_Destination", "No destination set for interface " &
               (+Pkg.Iface));
         End_If;
      End_Procedure ("Assert_Has_Destination");

      Start_Procedure ("Set_Destination (Dest : String)");
      Begin_Code;
         Assign
           ("Destination", "Ada.Strings.Unbounded.To_Unbounded_String (Dest)");
      End_Procedure ("Set_Destination");

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
      --!pp on
   end if;

   if not Pkg.Properties.Is_Empty then
      --!pp off
      Start_Function
        ("Get_Property (Property : String) return" &
         " D_Bus.Arguments.Argument_Type'Class");
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
      End_Procedure ("Set_Property");
      --!pp on
   end if;

   if not Pkg.Signals.Is_Empty then
      --!pp off
      Start_Procedure ("Remove_Match (Match : String)");
         Use_Entity ("D_Bus.Arguments.Basic");

         Declare_Entity ("Args", "D_Bus.Arguments.Argument_List_Type");
         Declare_Entity ("Discard", "D_Bus.Arguments.Argument_List_Type");
      Begin_Code;
         Call ("Assert_Has_Destination");

         Call ("D_Bus.Arguments.Append (Args, +Match)");
         Assign
           ("Discard",
            "D_Bus.Connection.Call_Blocking (Connection" &
            ", ""org.freedesktop.DBus"", +""/org/freedesktop/DBus""" &
            ", ""org.freedesktop.DBus"", ""RemoveMatch""" &
            ", D_Bus.Connection.Default_Timeout, Args)");
      End_Procedure ("Remove_Match");

      Declare_Entity ("Signal_Msg", "D_Bus.Messages.Message_Type");
      Declare_Entity ("Signal_Handled", "exception");

      Start_Procedure
        ("Signal_Handler (Msg : D_Bus.Messages.Message_Type)");
      Begin_Code;
         Start_If
           ("D_Bus.Messages.Get_Path (Msg) = D_Bus.Types.To_String (Path)");
            Assign ("Signal_Msg", "Msg");
            Raise_Exception ("Signal_Handled");
         End_If;
      End_Procedure ("Signal_Handler");
      --!pp on
   end if;
end Codegen.Client_Body.Add_Builtins;
