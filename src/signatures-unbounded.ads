pragma Ada_2012;

with Ada.Containers;

private with Ada.Containers.Indefinite_Holders;

package Signatures.Unbounded is
   --------------------------
   -- Unbounded Signatures --
   --------------------------
   type Unbounded_Signature is private;

   function "+" (L : Unbounded_Signature) return Signature;
   function "+" (L : Signature) return Unbounded_Signature;
   function Hash (Key : Unbounded_Signature) return Ada.Containers.Hash_Type;

private
   package S_IDH is new Ada.Containers.Indefinite_Holders (Signature);

   type Unbounded_Signature is record
      Value : S_IDH.Holder;
   end record;
end Signatures.Unbounded;
