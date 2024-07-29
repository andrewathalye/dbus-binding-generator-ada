pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

with Parsing;

package Codegen.Types is
   package Ada_Type_Declaration_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Ada_Type_Declaration,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => "=");
   subtype Ada_Type_Declaration_Map is Ada_Type_Declaration_Maps.Map;
   --  A package containing only a list of Ada type declarations.

   function Calculate_Request_Signature
     (Arguments : Parsing.Argument_List) return String;
   --  Return the D_Bus signature of the in arguments in `Arguments`

   function Calculate_Reply_Signature
     (Arguments : Parsing.Argument_List) return String;
   --  Analagous, but only for out arguments

   procedure Add_Types
     (Types : in out Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   --  Generate Ada types for all of the members of `Pkg` and
   --  add them to `Map` if it did not already contain them.

   procedure Print (Types : Ada_Type_Declaration_Map);
   --  Declare a package containing type definitions for `Types`
end Codegen.Types;
