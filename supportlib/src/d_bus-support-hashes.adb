pragma Ada_2012;

with Ada.Strings.Hash;

package body D_Bus.Support.Hashes is
   ----------
   -- Hash --
   ----------
   function Hash
     (Key : D_Bus.Types.Obj_Path) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (D_Bus.Types.To_String (Key)));

   ----------
   -- Hash --
   ----------
   function Hash
     (Key : D_Bus.Types.Signature) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (D_Bus.Types.To_String (Key)));
end D_Bus.Support.Hashes;
