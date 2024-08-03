pragma Ada_2012;

with Ada.Strings.Unbounded;

package Shared is
   function "+" (L : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (L : Ada.Strings.Unbounded.Unbounded_String) return String
      renames Ada.Strings.Unbounded.To_String;
end Shared;
