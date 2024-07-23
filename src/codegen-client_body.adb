with Codegen.Output.Client;
use Codegen.Output.Client;
use Codegen.Output;

with Codegen.Binding; use Codegen.Binding;
with Codegen.Client_Body.Add_Builtins;

with Parsing;
use type Parsing.DBus_Direction;

with Type_Checking; use Type_Checking;

with Shared; use Shared;

package body Codegen.Client_Body is
   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type) is
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
      With_Entity ("D_Bus.Connection");
      With_Entity ("D_Bus.Connection.Dispatch");
      With_Entity ("D_Bus.Types");
      Use_Type ("D_Bus.Types.Obj_Path");
      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Messages");
      With_Entity ("D_Bus.Extra");

      --  Package
      Start_Package_Body (+Pkg.Name);
         --  Builtin
         Add_Builtins (Pkg);

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
               --  Bind each in argument
               for A of M.Arguments loop
                  if A.Direction = Parsing.DIn then
                     Apply_In_Argument (A);
                  end if;
               end loop;

               --  The method call itself
               Call
                 ("Call_Remote (Iface, """ & (+M.Name) & """, Request_Args" &
                  ", Reply_Args)");

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
            Start_Procedure (Signal_Signature (S));
               Declare_Entity ("Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Match_Rule",
                  "constant String",
                  """path="" & D_Bus.Types.To_String (Path) & " &
                  """, interface="" & Iface & "", member=" &
                  (+S.Name) & """");
            Begin_Code;
               --  Add match rule
               Call ("D_Bus.Connection.Add_Match (Connection, Match_Rule)");

               --  Execute handler and interrupt (binding doesn’t offer a
               --  one-time dispatch (yet)
               Begin_Code;
                  Call
                    ("D_Bus.Connection.Dispatch (Connection," &
                     " Signal_Handler'Access)");
               Exception_Code;
                  When_Exception ("Signal_Handled");
                     Call ("null");
               End_Code;

               --  Remove match rule
               Call ("Remove_Match (Match_Rule)");

               --  Bind arguments
               Assign ("Args", "D_Bus.Messages.Get_Arguments (Signal_Msg)");
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
            End_Procedure (Signal_Name (S));
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
                     Assign
                       ("Property",
                        DBus_Type & " (Get_Property (""" &
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
                  Codegen.Binding.Bind_To_DBus
                    (Pkg => Pkg,
                     TD => Pkg.Type_Declarations (P.Type_Code),
                     Ada_Name => "Value",
                     DBus_Name => "Property");
                  Call ("Set_Property (""" & (+P.Name) & """, Property)");
               End_Procedure (Property_Write_Name (P));
            end if;
         end loop;
      End_Package (+Pkg.Name);
      --!pp on
   end Print;
end Codegen.Client_Body;
