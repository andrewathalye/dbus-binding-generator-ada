pragma Ada_2012;

package Signatures is
   pragma Pure;

   ----------------
   -- Signatures --
   ----------------
   type Signature_Element is
     ('y', 'b', 'n', 'q', 'i', 'u', 'x', 't', 'd', 's', 'o', 'g', 'h', 'v',
      'a', '(', ')', '{', '}');

   for Signature_Element'Size use Character'Size;

   type Signature is array (Positive range <>) of Signature_Element;

   subtype Basic_Type is Signature_Element range 'y' .. 'h';
   function Is_Basic (Sig : Signature) return Boolean;
   pragma Inline (Is_Basic);
   --  Basic type, per the D_Bus specification

   subtype Complex_Starting_Type is Signature_Element range 'a' .. '(';

   subtype Starting_Type is Signature_Element range 'y' .. '(';
   function Is_Starting (Sig : Signature) return Boolean;
   pragma Inline (Is_Starting);
   --  Type which can start a complete signature

   subtype Stringlike_Type is Signature_Element range 's' .. 'g';
   function Is_Stringlike (Sig : Signature) return Boolean;
   pragma Inline (Is_Stringlike);
   --  Stringlike type, pre the D_Bus specification

   -----------------
   -- Conversions --
   -----------------
   function As_String (Sig : Signature) return String;
   pragma Inline (As_String);
   --  Get a string representation for `Sig`.

   function As_Signature (Sig : String) return Signature;
   pragma Inline (As_Signature);
   --  Get `Sig` as a `Signature`.
   --  Note: An exception will be raised if any element of
   --  `Sig` was not a valid `Signature_Element`

   ---------------------
   -- Type Management --
   ---------------------
   function Get_Interior (DType : Signature) return Signature;
   --  Get the type(s) contained within a container
   --  It is erroneous to call this on a basic type or variant.

   No_More_Complete_Types : exception;
   function Get_Complete_Type
     (T : Signature; Index : Positive := 1) return Signature;
   --  Returns the complete type at index `Index` within T

   function Get_Ada_Type (T : Signature) return String;
   --  Generate a consistent Ada name for a DBus type

   function Get_Library_DBus_Type (T : Signature) return String;
   --  Get the D_Bus-Ada type name for a DBus type on the wire

   function Get_Library_Ada_Type (T : Signature) return String;
   --  Get the D_Bus-Ada type name for a DBus basic type
end Signatures;
