with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
use Codegen.Output;

with Codegen.Binding; use Codegen.Binding;

with Shared;        use Shared;
with Type_Checking; use Type_Checking;

package body Codegen.Client.Iface is
   Properties_Package : constant String := "org_freedesktop_DBus_Properties";

   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec (Pkg : Ada_Package_Type);
   procedure Print_Spec (Pkg : Ada_Package_Type) is
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      With_Entity ("Interfaces");
      --  All basic number types
      New_Line;

      With_Entity ("GNAT.OS_Lib");
      --  `File_Descriptor`
      New_Line;

      With_Entity ("D_Bus.Arguments.Containers");
      --  `Variant_Type`

      With_Entity ("D_Bus.Support");
      --  `Unbounded_Object_Path`, `Unbounded_Signature`
      With_Entity ("D_Bus.Support.Client");
      --  `Client_Interface`

      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  <>

      if not Pkg.Properties.Is_Empty then
         With_Entity (Properties_Package);
      end if;

      New_Line;

      --  Package Spec
      --!pp off
      Start_Package (+Pkg.Name);
         --  The interface type changes if properties are present
         if Pkg.Properties.Is_Empty then
            Declare_Type
              ("Child_Interface",
               "limited interface and D_Bus.Support.Client.Client_Interface");
         else
            Declare_Type
              ("Child_Interface",
               "limited interface and " &
               Properties_Package & ".Child_Interface");
         end if;

         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
            for M of Pkg.Methods loop
               Declare_Procedure (Method_Signature (M));
               New_Line;
            end loop;
         end if;

         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
            for S of Pkg.Signals loop
               Declare_Procedure (Signal_Register_Signature (S));
               Declare_Procedure (Signal_Unregister_Signature (S));
               Declare_Procedure (Signal_Await_Signature (S));
               New_Line;
            end loop;
         end if;

         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
            for P of Pkg.Properties loop
               if P.PAccess in Parsing.Read | Parsing.Readwrite then
                  Declare_Function (Property_Read_Signature (P));
               end if;

               if P.PAccess in Parsing.Write | Parsing.Readwrite then
                  Declare_Procedure (Property_Write_Signature (P));
               end if;
               New_Line;
            end loop;
         end if;
      End_Package (+Pkg.Name);
      --!pp on
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type)
   is
      use type Parsing.DBus_Direction;
   begin
      --  Preamble
      Use_Pragma ("Ada_2012");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Types");
      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Extra");
      With_Entity ("D_Bus.Support");
      With_Entity ("D_Bus.Messages");

      --  Package
      Start_Package_Body (+Pkg.Name);
      begin
         --  Declares
         Declare_Entity
           ("Iface", "constant String", """" & (+Pkg.Real_Name) & """");

         --  Methods
         for M of Pkg.Methods loop
            Start_Procedure (Method_Signature (M));
            begin
               Declare_Entity
                 ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
            end;
            Begin_Code;
            begin
               --  Bind each in argument
               for A of M.Arguments loop
                  if A.Direction = Parsing.DIn then
                     Declare_Code;
                     begin
                        Declare_Entity
                          ("Root_Object",
                           Get_Library_DBus_Type (+A.Type_Code));
                     end;
                     Begin_Code;
                     begin
                        Bind_To_DBus
                          (Types, A.Type_Code, +A.Name, "Root_Object");
                        Call ("Request_Args.Append (Root_Object)");
                     end;
                     End_Code;
                  end if;
               end loop;

               --  The method call itself
               Assign
                 ("Reply_Args",
                  "O.Call_Blocking (Iface, """ & (+M.Name) &
                  """, Request_Args)");

               --  Bind each out argument
               declare
                  Index : Positive := 1;
               begin
                  for A of M.Arguments loop
                     if A.Direction = Parsing.DOut then
                        Codegen.Binding.Bind_To_Ada
                          (Types     => Types, Type_Code => A.Type_Code,
                           DBus_Name =>
                             "Reply_Args.Get_Element (" & Index'Image & ")",
                           Ada_Name  => +A.Name);

                        Index := Index + 1;
                     end if;
                  end loop;
               end;
            end;
            End_Procedure (Method_Name (M));
         end loop;

         --  Signals
         for S of Pkg.Signals loop
            Start_Procedure (Signal_Register_Signature (S));
            Begin_Code;
            begin
               Call ("O.Register_Signal (Iface, """ & (+S.Name) & """)");
            end;
            End_Procedure (Signal_Register_Name (S));

            Start_Procedure (Signal_Unregister_Signature (S));
            Begin_Code;
            begin
               Call ("O.Unregister_Signal (Iface, """ & (+S.Name) & """)");
            end;
            End_Procedure (Signal_Unregister_Name (S));

            Start_Procedure (Signal_Await_Signature (S));
            begin
               Declare_Entity ("Msg", "D_Bus.Messages.Message_Type");
               Declare_Entity ("Args", "D_Bus.Arguments.Argument_List_Type");
            end;
            Begin_Code;
            begin
               Call ("O.Await_Signal (Msg, Iface, """ & (+S.Name) & """)");
               Assign ("Args", "D_Bus.Messages.Get_Arguments (Msg)");

               --  Bind arguments
               declare
                  Index : Positive := 1;
               begin
                  for A of S.Arguments loop
                     Bind_To_Ada
                       (Types     => Types, Type_Code => A.Type_Code,
                        DBus_Name => "Args.Get_Element (" & Index'Image & ")",
                        Ada_Name  => +A.Name);
                     Index := Index + 1;
                  end loop;
               end;
            end;
            End_Procedure (Signal_Await_Name (S));
         end loop;

         --  Properties
         for P of Pkg.Properties loop
            --  Getter
            if P.PAccess in Parsing.Read | Parsing.Readwrite then
               declare
                  Ada_Type  : constant String := Get_Ada_Type (+P.Type_Code);
                  DBus_Type : constant String :=
                    Get_Library_DBus_Type (+P.Type_Code);
               begin
                  Start_Function (Property_Read_Signature (P));
                  begin
                     Use_Entity ("Ada.Strings.Unbounded");
                     Declare_Entity
                       ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
                     Declare_Entity ("Property", DBus_Type);
                     Declare_Entity ("Property_Ada", Ada_Type);
                  end;
                  Begin_Code;
                  begin
                     --  Get the property via a direct D_Bus call
                     Call
                       (Properties_Package & ".Child_Interface'Class (O)" &
                        ".Get (To_Unbounded_String (Iface)," &
                        " To_Unbounded_String (""" & (+P.Name) & """)," &
                        " Variant)");

                     Assign
                       ("Property", DBus_Type & " (Variant.Get_Argument)");

                     Codegen.Binding.Bind_To_Ada
                       (Types     => Types, Type_Code => P.Type_Code,
                        DBus_Name => "Property", Ada_Name => "Property_Ada");
                     Return_Entity ("Property_Ada");
                  end;
                  End_Procedure (Property_Read_Name (P));
               end;
            end if;

            --  Setter
            if P.PAccess in Parsing.Write | Parsing.Readwrite then
               Start_Procedure (Property_Write_Signature (P));
               begin
                  Use_Entity ("Ada.Strings.Unbounded");
                  Declare_Entity
                    ("Property", Get_Library_DBus_Type (+P.Type_Code));
                  Declare_Entity
                    ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
               end;
               Begin_Code;
               begin
                  Codegen.Binding.Bind_To_DBus
                    (Types    => Types, Type_Code => P.Type_Code,
                     Ada_Name => "Value", DBus_Name => "Property");

                  Assign
                    ("Variant",
                     "D_Bus.Arguments.Containers.Create (Property)");

                  --  Set the property (direct D_Bus call)
                  Call
                    (Properties_Package & ".Child_Interface'Class (O)" &
                     ".Set (To_Unbounded_String (Iface), To_Unbounded_String" &
                     " (""" & (+P.Name) & """), Variant)");
               end;
               End_Procedure (Property_Write_Name (P));
            end if;
         end loop;
      end;
      End_Package (+Pkg.Name);
   end Print_Body;

   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type)
   is
   begin
      Print_Spec (Pkg);
      Print_Body (Types, Pkg);
   end Print;
end Codegen.Client.Iface;
