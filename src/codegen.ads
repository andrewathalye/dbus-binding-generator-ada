pragma Ada_2012;

with Ada.Strings.Unbounded;

with Parsing;

private with Ada.Containers.Vectors;
private with Signatures.Unbounded;

package Codegen is
   type Ada_Package_Type is private;
   --  A generic Ada package (with info about D_Bus mappings)

   type Ada_Type_Declaration is private;
   --  See `Codegen.Types` for more information.

   function Create_Package
     (I : Parsing.Interface_Type) return Ada_Package_Type;
   --  Return an Ada Package object based on the DBus interface
private
   --  Packages
   type Ada_Package_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Real_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Methods     : Parsing.Method_List;
      Signals     : Parsing.Signal_List;
      Properties  : Parsing.Property_List;
      Annotations : Parsing.Annotations_List;
   end record;

   -----------------------
   -- Type Declarations --
   -----------------------
   type Ada_Record_Member_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Signatures.Unbounded.Unbounded_Signature;
   end record;

   package ARML is new Ada.Containers.Vectors
     (Positive, Ada_Record_Member_Type);
   subtype Ada_Record_Member_List is ARML.Vector;

   type ATDK is
     (Basic_Kind, Array_Kind, Struct_Kind, Ordered_Dict_Kind, Hashed_Dict_Kind,
      Variant_Kind);
   type Ada_Type_Declaration (Kind : ATDK := Basic_Kind) is record
      Type_Code : Signatures.Unbounded.Unbounded_Signature;

      case Kind is
         when Basic_Kind | Variant_Kind =>
            null;
         when Array_Kind =>
            Array_Element_Type_Code : Signatures.Unbounded.Unbounded_Signature;
         when Struct_Kind =>
            Struct_Members : Ada_Record_Member_List;
         when Ordered_Dict_Kind | Hashed_Dict_Kind =>
            Dict_Key_Type_Code     : Signatures.Unbounded.Unbounded_Signature;
            Dict_Element_Type_Code : Signatures.Unbounded.Unbounded_Signature;

            case Kind is
               when Hashed_Dict_Kind =>
                  Dict_Key_Hash_Function : Ada.Strings.Unbounded
                    .Unbounded_String;
               when others =>
                  null;
            end case;
      end case;
   end record;
end Codegen;
