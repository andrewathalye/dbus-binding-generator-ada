pragma Ada_2005;

with dbus_message_h;
with Interfaces;
with GNAT.OS_Lib;

with D_Bus.Arguments;

private with Ada.Strings.Unbounded;

package D_Bus.Extra is
   ------------
   -- DOUBLE --
   ------------
   type Double is new Interfaces.IEEE_Float_64;
   type Double_Type is new D_Bus.Arguments.Basic_Type with private;

   function To_String (Arg : Double_Type) return String;
   function To_Ada (Arg : Double_Type) return Double;

   function "+" (L : Double) return Double_Type;

   overriding procedure Serialize
     (Arg : Double_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
     return Double_Type;

   ---------------------
   -- FILE_DESCRIPTOR --
   ---------------------
   type File_Descriptor is new GNAT.OS_Lib.File_Descriptor;
   type File_Descriptor_Type is new D_Bus.Arguments.Basic_Type with private;

   function To_String (Arg : File_Descriptor_Type) return String;
   function To_Ada (Arg : File_Descriptor_Type) return File_Descriptor;

   function "+" (L : File_Descriptor) return File_Descriptor_Type;

   overriding procedure Serialize
     (Arg : File_Descriptor_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
     return File_Descriptor_Type;

   ---------------
   -- SIGNATURE --
   ---------------
   Invalid_Signature : exception;
   type Signature is new String;
   --  Note: Signatures are validated when serialised!
   --  If you pass an invalid signature as a DBus parameter,
   --  Invalid_Signature will be raised.

   type Signature_Type is new D_Bus.Arguments.Basic_Type with private;

   function To_String (Arg : Signature_Type) return String;
   function To_Ada (Arg : Signature_Type) return Signature;

   function "+" (L : Signature) return Signature_Type;

   overriding procedure Serialize
     (Arg : Signature_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
     return Signature_Type;

private
   type Double_Type is new D_Bus.Arguments.Basic_Type with record
      Value : Double := 0.0;
   end record;

   type File_Descriptor_Type is new D_Bus.Arguments.Basic_Type with record
      Value : File_Descriptor := 0;
   end record;

   type Signature_Type is new D_Bus.Arguments.Basic_Type with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end D_Bus.Extra;
