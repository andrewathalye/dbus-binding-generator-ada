with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded;

with Parsing;

private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package Codegen is
   -----------
   -- TYPES --
   -----------
   type Ada_Types_Package_Type is private;
   --  A package containing only a list of Ada type declarations.

   type Ada_Package_Type is private;
   --  A generic Ada package (with info about D_Bus mappings)

   type Ada_Type_Declaration is private;
   --  An Ada type declaration (with info about D_Bus mappings)

   -----------------
   -- SUBPROGRAMS --
   -----------------
   function Create_Package
     (Node : Ada.Strings.Unbounded.Unbounded_String;
      I    : Parsing.Interface_Type) return Ada_Package_Type;
   --  Return an Ada Package object based on the DBus interface

   procedure Append_Types
     (Types_Pkg : in out Ada_Types_Package_Type; Pkg : Ada_Package_Type);
   --  Add any types only in `Pkg` to `Types_Pkg`
   --  You may then call `Codegen.Output.Print_Types_Package (...)`
private
   --  Type Declarations
   type Ada_Record_Member_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package ARML is new Ada.Containers.Vectors
     (Positive, Ada_Record_Member_Type);
   subtype Ada_Record_Member_List is ARML.Vector;

   type ATDK is
     (Basic_Kind, Array_Kind, Struct_Kind, Ordered_Dict_Kind, Hashed_Dict_Kind,
      Variant_Kind);
   type Ada_Type_Declaration (Kind : ATDK := Basic_Kind) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Ada name of the DBus type

      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
      --  DBus type code

      case Kind is
         when Basic_Kind | Variant_Kind =>
            null;
         when Array_Kind =>
            Array_Element_Type_Code : Ada.Strings.Unbounded.Unbounded_String;
            --  DBus Type Code of an Array Element
         when Struct_Kind =>
            Struct_Members : Ada_Record_Member_List;
         when Ordered_Dict_Kind | Hashed_Dict_Kind =>
            Dict_Key_Type_Code     : Ada.Strings.Unbounded.Unbounded_String;
            --  DBus Type Code of a Dict Key
            Dict_Element_Type_Code : Ada.Strings.Unbounded.Unbounded_String;
            --  DBus Type Code of a Dict Element
      end case;
   end record;

   use type Ada.Strings.Unbounded.Unbounded_String;
   package ATDM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Ada_Type_Declaration, Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   subtype Ada_Type_Declaration_Map is ATDM.Map;

   --  Packages
   type Ada_Package_Type is record
      Name              : Ada.Strings.Unbounded.Unbounded_String;
      Node              : Ada.Strings.Unbounded.Unbounded_String;
      Iface             : Ada.Strings.Unbounded.Unbounded_String;
      Type_Declarations : Ada_Type_Declaration_Map;
      Methods           : Parsing.Method_List;
      Signals           : Parsing.Signal_List;
      Properties        : Parsing.Property_List;
   end record;

   type Ada_Types_Package_Type is record
      Type_Declarations : Ada_Type_Declaration_Map;
   end record;
end Codegen;
