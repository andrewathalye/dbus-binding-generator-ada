package Codegen.Types is
   type Ada_Types_Package_Type is private;
   --  A package containing only a list of Ada type declarations.

   procedure Append_Types
     (Types_Pkg : in out Ada_Types_Package_Type; Pkg : Ada_Package_Type);
   --  Add any types only in `Pkg` to `Types_Pkg`

   procedure Print (Types_Pkg : Ada_Types_Package_Type);
   --  Declare a package containing type definitions for `Types_Pkg`
private
   type Ada_Types_Package_Type is record
      Type_Declarations : Ada_Type_Declaration_Map;
   end record;
end Codegen.Types;
