with Codegen.Output.Client; use Codegen.Output.Client;
use Codegen.Output;

with Codegen.Binding; use Codegen.Binding;

with Shared;        use Shared;
with Type_Checking; use Type_Checking;

package body Codegen.Client.Iface is
   ----------------
   -- Print_Spec --
   ----------------
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

      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  <>
      New_Line;

      --  Package Spec
      --!pp off
      Start_Package (+Pkg.Name);
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
   procedure Print_Body (Pkg : Ada_Package_Type) is
      use type Parsing.DBus_Direction;

      --  Applies `A` to `Request_Args`
      procedure Apply_In_Argument (A : Parsing.Argument_Type);
      procedure Apply_In_Argument (A : Parsing.Argument_Type) is
         TD : constant Ada_Type_Declaration :=
           Pkg.Type_Declarations (A.Type_Code);
      begin
         case TD.Kind is
            when Basic_Kind | Variant_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_Obj", Get_Library_DBus_Type (+A.Type_Code));
               Begin_Code;
                  Codegen.Binding.Bind_To_DBus (Pkg, TD, +A.Name, "Root_Obj");
                  Call ("Request_Args.Append (Root_Obj)");
               End_Code;
               --!pp on
            when Array_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_List", "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Codegen.Binding.Bind_To_DBus (Pkg, TD, +A.Name, "Root_List");
                  Call ("Request_Args.Append (Root_List)");
               End_Code;
               --!pp on
            when Struct_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_Struct", "D_Bus.Arguments.Containers.Struct_Type");
               Begin_Code;
                  Codegen.Binding.Bind_To_DBus
                    (Pkg, TD, +A.Name, "Root_Struct");
                  Call ("Request_Args.Append (Root_Struct)");
               End_Code;
               --!pp on
            when Ordered_Dict_Kind | Hashed_Dict_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_Dict", "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Codegen.Binding.Bind_To_DBus (Pkg, TD, +A.Name, "Root_Dict");
                  Call ("Request_Args.Append (Root_Dict)");
               End_Code;
               --!pp on
         end case;
      end Apply_In_Argument;
   begin
      --!pp off
      --  Preamble
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Types");
      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Extra");
      With_Entity ("D_Bus.Support");

      --  Package
      Start_Package_Body (+Pkg.Name);
         --  Declares
         Declare_Entity
           ("Iface", "constant String", """" & (+Pkg.Iface) & """");

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
         end if;
         for M of Pkg.Methods loop
            Start_Procedure (Method_Signature (M));
               Declare_Entity
                 ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
            Begin_Code;
               Call ("Assert_Has_Destination");

               --  Bind each in argument
               for A of M.Arguments loop
                  if A.Direction = Parsing.DIn then
                     Apply_In_Argument (A);
                  end if;
               end loop;

               --  The method call itself
               Assign
                 ("Reply_Args",
                  "D_Bus.Support.Call_Blocking (Get_Destination, Get_Node," &
                  " Iface, """ & (+M.Name) & """, Request_Args)");

               --  Bind each out argument
               declare
                  Index : Positive := 1;
               begin
                  for A of M.Arguments loop
                     if A.Direction = Parsing.DOut then
                        Codegen.Binding.Bind_To_Ada
                          (Pkg => Pkg,
                           TD => Pkg.Type_Declarations (A.Type_Code),
                           DBus_Name =>
                              "Reply_Args.Get_Element (" &
                               Index'Image & ")",
                           Ada_Name  => +A.Name);

                        Index := Index + 1;
                     end if;
                  end loop;
               end;
            End_Procedure (Method_Name (M));
         end loop;

         --  Signals
         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
         end if;
         for S of Pkg.Signals loop
            Declare_Entity (Signal_Id_Name (S), "D_Bus.Support.Signal_Id");

            --  (Node : String)
            Start_Procedure (Signal_Register_Signature (S));
               Use_Type ("D_Bus.Support.Signal_Id");
               Use_Type ("D_Bus.Support.Unbounded_Object_Path");
               Use_Type ("D_Bus.Types.Obj_Path");

               Declare_Entity ("Target_Node", "D_Bus.Types.Obj_Path");
            Begin_Code;
               --  Prevent duplicate registration
               Start_If
                 (Signal_Id_Name (S) & " /= D_Bus.Support.Null_Signal_Id");
                  Raise_Exception
                    ("D_Bus.D_Bus_Error", "Signal already registered.");
               End_If;

               --  Select node
               Start_If ("Node = D_Bus.Support.Null_Unbounded_Object_Path");
                  Assign ("Target_Node", "Get_Node");
               Start_Else;
                  Assign
                    ("Target_Node",
                     "+Ada.Strings.Unbounded.To_String (Node)");
               End_If;

               --  Assign the Signal_Id
               Assign
                 (Signal_Id_Name (S),
                  "D_Bus.Support.Register_Signal (Get_Node, Iface," &
                  " """ & (+S.Name) & """)");
            End_Procedure (Signal_Register_Name (S));

            Start_Procedure (Signal_Unregister_Signature (S));
            Begin_Code;
               Call
                 ("D_Bus.Support.Unregister_Signal (" &
                  Signal_Id_Name (S) & ")");
            End_Procedure (Signal_Unregister_Name (S));

            Start_Procedure (Signal_Await_Signature (S));
               Declare_Entity ("Args", "D_Bus.Arguments.Argument_List_Type");
            Begin_Code;
               Call ("Assert_Has_Destination");

               Assign
                 ("Args",
                  "D_Bus.Messages.Get_Arguments" &
                  " (D_Bus.Support.Await_Signal (" &
                  Signal_Id_Name (S) & "))");

               --  Bind arguments
               declare
                  Index : Positive := 1;
               begin
                  for A of S.Arguments loop
                     Bind_To_Ada
                       (Pkg       => Pkg,
                        TD        =>
                          Pkg.Type_Declarations (A.Type_Code),
                        DBus_Name =>
                           "Args.Get_Element (" & Index'Image & ")",
                        Ada_Name  => +A.Name);
                     Index := Index + 1;
                  end loop;
               end;
            End_Procedure (Signal_Await_Name (S));
         end loop;

         --  Properties
         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
         end if;
         for P of Pkg.Properties loop
            --  Getter
            if P.PAccess in Parsing.Read | Parsing.Readwrite then
               declare
                  Ada_Type : constant String :=
                    +Pkg.Type_Declarations (P.Type_Code).Name;
                  DBus_Type : constant String :=
                    Get_Library_DBus_Type (+P.Type_Code);
               begin
                  Start_Function (Property_Read_Signature (P));
                     Declare_Entity ("Property", DBus_Type);
                     Declare_Entity ("Property_Ada", Ada_Type);
                  Begin_Code;
                     Call ("Assert_Has_Destination");

                     Assign
                       ("Property",
                        DBus_Type & " (D_Bus.Support.Get_Property" &
                        " (Get_Destination, Get_Node, Iface, """ &
                        (+P.Name) & """))");
                     Codegen.Binding.Bind_To_Ada
                       (Pkg => Pkg,
                        TD => Pkg.Type_Declarations (P.Type_Code),
                        DBus_Name => "Property",
                        Ada_Name => "Property_Ada");
                     Return_Entity ("Property_Ada");
                  End_Procedure (Property_Read_Name (P));
               end;
            end if;

            --  Setter
            if P.PAccess in Parsing.Write | Parsing.Readwrite then
               Start_Procedure (Property_Write_Signature (P));
                  Declare_Entity
                    ("Property", Get_Library_DBus_Type (+P.Type_Code));
               Begin_Code;
                  Call ("Assert_Has_Destination");

                  Codegen.Binding.Bind_To_DBus
                    (Pkg => Pkg,
                     TD => Pkg.Type_Declarations (P.Type_Code),
                     Ada_Name => "Value",
                     DBus_Name => "Property");
                  Call
                    ("D_Bus.Support.Set_Property (Get_Destination, Get_Node," &
                     " Iface, """ & (+P.Name) & """, Property)");
               End_Procedure (Property_Write_Name (P));
            end if;
         end loop;
      End_Package (+Pkg.Name);
      --!pp on
   end Print_Body;
end Codegen.Client.Iface;
