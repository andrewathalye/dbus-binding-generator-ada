with Ada.Strings.Unbounded;

package Shared is
   pragma Preelaborate (Shared);

   function "+" (Source : Ada.Strings.Unbounded.Unbounded_String) return String
      renames Ada.Strings.Unbounded.To_String;
   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;
end Shared;

