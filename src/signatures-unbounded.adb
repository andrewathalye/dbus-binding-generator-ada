with GNAT.CRC32;

package body Signatures.Unbounded is
   -------------------------
   -- Unbounded_Signature --
   -------------------------
   function "+" (L : Unbounded_Signature) return Signature is
     (L.Value.Element);

   function "+" (L : Signature) return Unbounded_Signature is
   begin
      return Result : Unbounded_Signature do
         Result.Value.Replace_Element (L);
      end return;
   end "+";

   function Hash (Key : Unbounded_Signature) return Ada.Containers.Hash_Type is
      Result : GNAT.CRC32.CRC32;
   begin
      GNAT.CRC32.Initialize (Result);
      GNAT.CRC32.Update (Result, As_String (+Key));

      return Ada.Containers.Hash_Type (GNAT.CRC32.Get_Value (Result));
   end Hash;
end Signatures.Unbounded;
