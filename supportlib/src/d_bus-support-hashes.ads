pragma Ada_2012;

with Ada.Containers;

with D_Bus.Types;

package D_Bus.Support.Hashes is
   function Hash (Key : D_Bus.Types.Obj_Path) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);

   function Hash (Key : D_Bus.Types.Signature) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);
end D_Bus.Support.Hashes;
