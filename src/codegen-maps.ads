pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

package Codegen.Maps is
   package Ada_Package_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Ada_Package_Type,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   subtype Ada_Package_Map is Ada_Package_Maps.Map;
end Codegen.Maps;
