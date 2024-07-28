pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

package Codegen.Types is
   package Ada_Type_Declaration_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Ada_Type_Declaration,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => "=");
   subtype Ada_Type_Declaration_Map is Ada_Type_Declaration_Maps.Map;
   --  A package containing only a list of Ada type declarations.

   procedure Add_Types
     (Types : in out Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   --  Generate Ada types for all of the members of `Pkg` and
   --  add them to `Map` if it did not already contain them.

   procedure Print (Types : Ada_Type_Declaration_Map);
   --  Declare a package containing type definitions for `Types`
end Codegen.Types;
