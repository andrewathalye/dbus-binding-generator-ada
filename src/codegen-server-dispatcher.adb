pragma Ada_2012;

with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

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

      Start_Package ("D_Bus.Generated_Dispatcher");
      begin
         Declare_Procedure
           ("Add_Object (O : D_Bus.Support.Server_Object'Class)");
         Declare_Procedure
           ("Remove_Object (Path : Unbounded_Object_Path)");
         Declare_Procedure ("Blocking_Dispatch");
      end;
      End_Package ("D_Bus.Generated_Dispatcher");
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   --  TODO: registration and thread safety
   procedure Print_Body (Pkgs : Codegen.Lists.Ada_Package_List);
   procedure Print_Body (Pkgs : Codegen.Lists.Ada_Package_List) is
   begin
      --  Preamble
      Use_Pragma ("Ada_2012");

      With_Entity ("Ada.Containers.Hashed_Maps");
      With_Entity ("Ada.Strings.Unbounded.Hash");

      Start_Package_Body ("D_Bus.Generated_Dispatcher");
      begin
         --  Types
         Declare_Type ("Server_Object_Access", "access Server_Object'Class");

         Declare_Package
           ("Object_Maps",
            "new Ada.Containers.Hashed_Maps (Unbounded_Object_Path," &
            " Server_Object_Access, Ada.Strings.Unbounded.Hash, ""="")");

         Declare_Entity ("Objects", "Object_Maps.Map");

         --  Predefined Subprograms
         Start_Procedure
           ("Add_Object (O : D_Bus.Support.Server_Object'Class)");
         Begin_Code;
         begin
            Start_If ("Objects.Contains (O.Node)");
            begin
               Raise_Exception
                 ("D_Bus_Error",
                  """An object is already registered with path "" &" &
                  " To_String (O.Node)");
            end;
            End_If;

            Call
              ("Objects.Insert (O.Node, new" &
               " D_Bus.Support.Server_Object'Class'(O))");
         end;
         End_Procedure ("Add_Object");

         Start_Procedure ("Remove_Object (Path : Unbounded_Object_Path)");
         Begin_Code;
         begin
            Start_If ("not Objects.Contains (Path)");
            begin
               Raise_Exception
                 ("D_Bus_Error", """No object is registered with path "" &" &
                  " To_String (Path)");
            end;
            End_If;

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
   procedure Print (Pkgs : Codegen.Lists.Ada_Package_List) is
   begin
      Print_Spec;
      Print_Body (Pkgs);
   end Print;

end Codegen.Server.Dispatcher;
