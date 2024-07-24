with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

package body Codegen.Types is

   ------------------
   -- Append_Types --
   ------------------
   procedure Append_Types
     (Types_Pkg : in out Ada_Types_Package_Type; Pkg : Ada_Package_Type)
   is
   begin
      for TD of Pkg.Type_Declarations loop
         if not Types_Pkg.Type_Declarations.Contains (TD.Type_Code) then
            Types_Pkg.Type_Declarations.Insert (TD.Type_Code, TD);
         end if;
      end loop;
   end Append_Types;

   ---------------------------
   -- Declare_Types_Package --
   ---------------------------
   procedure Print (Types_Pkg : Ada_Types_Package_Type) is
      --  Produce an acceptable order for dependency resolution
      --  'Elaborate' the type declaration order
      package ATDL is new Ada.Containers.Vectors
        (Positive, Ada_Type_Declaration);
      subtype Ada_Type_Declaration_List is ATDL.Vector;

      function Resolve_Dependencies return Ada_Type_Declaration_List;
      function Resolve_Dependencies return Ada_Type_Declaration_List is
         package USL is new Ada.Containers.Vectors
           (Positive, Ada.Strings.Unbounded.Unbounded_String);
         subtype Unbounded_String_List is USL.Vector;

         Result      : Ada_Type_Declaration_List;
         Bookkeeping : Unbounded_String_List;

         procedure Resolve_Dependency (TD : Ada_Type_Declaration);
         procedure Resolve_Dependency (TD : Ada_Type_Declaration) is
         begin
            case TD.Kind is
               when Basic_Kind | Variant_Kind =>
                  null;
               when Array_Kind =>
                  if not Bookkeeping.Contains (TD.Array_Element_Type_Code) then
                     Resolve_Dependency
                       (Types_Pkg.Type_Declarations
                          (TD.Array_Element_Type_Code));
                  end if;
               when Ordered_Dict_Kind | Hashed_Dict_Kind =>
                  if not Bookkeeping.Contains (TD.Dict_Key_Type_Code) then
                     Resolve_Dependency
                       (Types_Pkg.Type_Declarations (TD.Dict_Key_Type_Code));
                  end if;

                  if not Bookkeeping.Contains (TD.Dict_Element_Type_Code) then
                     Resolve_Dependency
                       (Types_Pkg.Type_Declarations
                          (TD.Dict_Element_Type_Code));
                  end if;
               when Struct_Kind =>
                  for SM of TD.Struct_Members loop
                     if not Bookkeeping.Contains (SM.Type_Code) then
                        Resolve_Dependency
                          (Types_Pkg.Type_Declarations (SM.Type_Code));
                     end if;
                  end loop;
            end case;

            Bookkeeping.Append (TD.Type_Code);
            Result.Append (TD);
         end Resolve_Dependency;
      begin
         for TD of Types_Pkg.Type_Declarations loop
            if not Bookkeeping.Contains (TD.Type_Code) then
               Resolve_Dependency (TD);
            end if;
         end loop;
         return Result;
      end Resolve_Dependencies;
   begin
      --!pp off
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");
      With_Entity ("Ada.Strings.Unbounded.Hash");
      With_Entity ("Ada.Containers.Vectors");
      With_Entity ("Ada.Containers.Ordered_Maps");
      With_Entity ("Ada.Containers.Hashed_Maps");
      With_Entity ("Interfaces");
      Use_Entity ("Interfaces");
      New_Line;

      With_Entity ("GNAT.OS_Lib");
      New_Line;

      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Arguments.Containers");
      Use_Type ("D_Bus.Arguments.Containers.Variant_Type");
      With_Entity ("D_Bus.Extra");
      With_Entity ("D_Bus.Support");
      New_Line;

      Start_Package ("D_Bus.Generated_Types");
         --  Generated Types
         for TD of Resolve_Dependencies loop
            case TD.Kind is
               when Basic_Kind | Variant_Kind =>
                  null;
               when Array_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Vectors (Positive, " &
                     (+Types_Pkg.Type_Declarations
                        (TD.Array_Element_Type_Code).Name) &
                     ")");

                  Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Vector");
                  Use_Type (+TD.Name);
                  New_Line;
               when Struct_Kind =>
                  Start_Record (+TD.Name);
                  for SM of TD.Struct_Members loop
                     Declare_Entity
                       (+SM.Name,
                        (+Types_Pkg.Type_Declarations (SM.Type_Code).Name));
                  end loop;
                  End_Record;
                  New_Line;
               when Ordered_Dict_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Ordered_Maps (" &
                     (+Types_Pkg.Type_Declarations
                       (TD.Dict_Key_Type_Code).Name) &
                     ", " &
                     (+Types_Pkg.Type_Declarations
                        (TD.Dict_Element_Type_Code).Name) &
                     ")");

                  Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
                  Use_Type (+TD.Name);
                  New_Line;
               when Hashed_Dict_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Hashed_Maps (" &
                     (+Types_Pkg.Type_Declarations
                       (TD.Dict_Key_Type_Code).Name) &
                     ", " &
                     (+Types_Pkg.Type_Declarations
                        (TD.Dict_Element_Type_Code).Name) &
                     ", Ada.Strings.Unbounded.Hash, ""="")");

                  Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
                  Use_Type (+TD.Name);
                  New_Line;
            end case;
         end loop;
      End_Package ("D_Bus.Generated_Types");
      --!pp on
   end Print;
end Codegen.Types;
