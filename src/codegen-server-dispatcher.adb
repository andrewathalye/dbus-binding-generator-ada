pragma Ada_2012;

with Codegen.Output;             use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
with Codegen.Binding;            use Codegen.Binding;

with Type_Checking; use Type_Checking;
with Parsing;
with Shared;        use Shared;

package body Codegen.Server.Dispatcher is
   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec;
   procedure Print_Spec is
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      New_Line;

      With_Entity ("D_Bus.Support.Server");

      Start_Package ("D_Bus.Generated_Dispatcher");
      begin
         Declare_Procedure
           ("Add_Object (O : D_Bus.Support.Server.Server_Object'Class;" &
            " Path : D_Bus.Support.Unbounded_Object_Path)");
         Comment
           ("Register and initialise object `O` with path `Path`.");
         Declare_Procedure
           ("Remove_Object (Path : D_Bus.Support.Unbounded_Object_Path)");
         Comment ("Remove the object with path `Path` and destroy it.");
         New_Line;

         Declare_Procedure ("Blocking_Dispatch");
         Comment ("Wait for the next message to arrive and dispatch it.");
      end;
      End_Package ("D_Bus.Generated_Dispatcher");
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   --  TODO: registration and thread safety
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map;
      Pkgs  : Codegen.Maps.Ada_Package_Map);
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map;
      Pkgs  : Codegen.Maps.Ada_Package_Map)
   is
      use all type Parsing.DBus_Direction;
   begin
      --  Preamble
      Use_Pragma ("Ada_2012");

      With_Entity ("Ada.Containers.Indefinite_Hashed_Maps");
      With_Entity ("Ada.Strings.Unbounded.Hash");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");

      --  With all interfaces
      for Pkg of Pkgs loop
         With_Entity (+Pkg.Name);
      end loop;

      Start_Package_Body ("D_Bus.Generated_Dispatcher");
      begin
         --  Types
         Declare_Package
           ("Object_Maps",
            "new Ada.Containers.Indefinite_Hashed_Maps" &
            " (Unbounded_Object_Path," &
            " D_Bus.Support.Server.Server_Object'Class," &
            " Ada.Strings.Unbounded.Hash, ""="")");

         Declare_Entity ("Objects", "Object_Maps.Map");

         --  Generated Mappings
         --  TODO check signature
         for Pkg of Pkgs loop
            for M of Pkg.Methods loop
               --  Generate each dispatcher handler
               Start_Procedure (Method_Dispatcher_Signature (Pkg, M));
               begin
                  --  Use clauses
                  Use_Entity ("Ada.Strings.Unbounded");
                  Use_Entity ("D_Bus.Types");

                  --  Node path
                  Declare_Entity
                    (Entity => "Node",
                     EType  => "constant Unbounded_Object_Path",
                     Value  =>
                       "To_Unbounded_String" &
                       " (To_String (D_Bus.Messages.Get_Path (In_Msg)))");

                  --  In args and out args
                  Declare_Entity
                    ("In_Args", "constant D_Bus.Arguments.Argument_List_Type",
                     "D_Bus.Messages.Get_Arguments (In_Msg)");
                  Declare_Entity
                    ("Out_Args", "D_Bus.Arguments.Argument_List_Type");

                  --  Declare all arguments
                  for A of M.Arguments loop
                     Declare_Entity
                       (Sanitise_Name (+A.Name), Get_Ada_Type (+A.Type_Code));
                  end loop;
               end;
               Begin_Code;
               begin
                  --  Ensure the object path exists
                  Start_If ("not Objects.Contains (Node)");
                  begin
                     Raise_Exception
                       ("D_Bus_Error",
                        """No object found with path"" &" &
                        " To_String (Node)");
                  end;
                  End_If;

                  --  Bind all in arguments
                  for I in 1 .. M.Arguments.Last_Index loop
                     if M.Arguments (I).Direction = DIn then
                        Bind_To_Ada
                          (Types     => Types,
                           Type_Code => M.Arguments (I).Type_Code,
                           DBus_Name =>
                             Get_Library_DBus_Type
                               (+M.Arguments (I).Type_Code) &
                             " (" & "In_Args.Element (" & I'Image & "))",
                           Ada_Name  => +M.Arguments (I).Name);
                     end if;
                  end loop;

                  --  Call method
                  Declare_Code;
                  begin
                     Renames_Entity
                       (L => "O", T => (+Pkg.Name) & ".Child_Interface'Class",
                        R =>
                          (+Pkg.Name) &
                          ".Child_Interface'Class (Objects (Node))");
                  end;
                  Begin_Code;
                  begin
                     Call (Method_Call_Expression (M));
                  end;
                  End_Code;

                  --  Bind all out arguments
                  for A of M.Arguments loop
                     if A.Direction = DOut then
                        Declare_Code;
                        begin
                           Declare_Entity
                             ("DBus_Object",
                              Get_Library_DBus_Type (+A.Type_Code));
                        end;
                        Begin_Code;
                        begin
                           Bind_To_DBus
                             (Types    => Types, Type_Code => A.Type_Code,
                              Ada_Name => +A.Name, DBus_Name => "DBus_Object");

                           Call ("Out_Args.Append (DBus_Object)");
                        end;
                        End_Code;
                     end if;
                  end loop;

                  --  Send Reply
                  Call ("Objects (Node).Reply (In_Msg, Out_Args)");
               end;
               End_Procedure (Method_Dispatcher_Name (Pkg, M));
            end loop;
         end loop;

         --  Predefined Subprograms
         Start_Procedure
           ("Add_Object (O : D_Bus.Support.Server.Server_Object'Class;" &
            " Path : D_Bus.Support.Unbounded_Object_Path)");
         begin
            Use_Entity ("Ada.Strings.Unbounded");
         end;
         Begin_Code;
         begin
            Start_If ("Objects.Contains (Path)");
            begin
               Raise_Exception
                 ("D_Bus_Error",
                  """An object is already registered with path "" &" &
                  " To_String (Path)");
            end;
            End_If;

            Call ("O.Create (Path)");
            Call ("Objects.Insert (Path, O)");
         end;
         End_Procedure ("Add_Object");

         Start_Procedure
           ("Remove_Object (Path : D_Bus.Support.Unbounded_Object_Path)");
         Begin_Code;
         begin
            Start_If ("not Objects.Contains (Path)");
            begin
               Raise_Exception
                 ("D_Bus_Error",
                  """No object is registered with path "" &" &
                  " To_String (Path)");
            end;
            End_If;

            Call ("Objects (Path).Destroy");
            Call ("Objects.Delete (Path)");
         end;
         End_Procedure ("Remove_Object");
      end;
      Begin_Code;
      begin
         --  Register each method of each package at elaboration time
         for Pkg of Pkgs loop
            for M of Pkg.Methods loop
               Call
                 ("Register_Method_Handler (Dispatcher, """ &
                  (+Pkg.Real_Name) & """, """ & (+M.Name) & """, " &
                  (+Pkg.Name) & "_" & Sanitise_Name (+M.Name) & "'Access)");
            end loop;
         end loop;
      end;
      End_Package ("D_Bus.Generated_Dispatcher");
   end Print_Body;

   -----------
   -- Print --
   -----------
   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map;
      Pkgs  : Codegen.Maps.Ada_Package_Map)
   is
   begin
      Print_Spec;
      Print_Body (Types, Pkgs);
   end Print;

end Codegen.Server.Dispatcher;
