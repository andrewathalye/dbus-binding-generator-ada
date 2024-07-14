with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Parsing;

private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package Codegen is
   type Ada_Subprogram_Type is private;
   type Ada_Package_Type is private;

   procedure Print_Signature
     (SP : Ada_Subprogram_Type;
      File : Ada.Text_IO.File_Type);
   --  Print a function signature

   function Create_Package
     (Node : Ada.Strings.Unbounded.Unbounded_String;
      I : Parsing.Interface_Type) return Ada_Package_Type;
   --  Return an Ada Package object based on the DBus interface
private
   --  Type Declarations
   type Ada_Record_Member_Type is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package ARML is new Ada.Containers.Vectors
     (Positive, Ada_Record_Member_Type);
   subtype Ada_Record_Member_List is ARML.Vector;

   type ATDK is (Builtin_Kind, Array_Kind, Struct_Kind, Dict_Kind);
   type Ada_Type_Declaration (Kind : ATDK := Builtin_Kind) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Ada name of the DBus type

      case Kind is
         when Builtin_Kind =>
            null;
         when Array_Kind =>
            Array_Element_Type_Code : Ada.Strings.Unbounded.Unbounded_String;
         when Struct_Kind =>
            Struct_Members : Ada_Record_Member_List;
         when Dict_Kind =>
            Dict_Key_Type_Code : Ada.Strings.Unbounded.Unbounded_String;
            Dict_Element_Type_Code : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   use type Ada.Strings.Unbounded.Unbounded_String;
   package ATDM is new Ada.Containers.Hashed_Maps
     (Key_Type => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Ada_Type_Declaration,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   subtype Ada_Type_Declaration_Map is ATDM.Map;

   --  Packages, Subprograms, Arguments
   type Ada_Argument_Type is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
      Direction : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package AATL is new Ada.Containers.Vectors (Positive, Ada_Argument_Type);
   subtype Ada_Argument_Type_List is AATL.Vector;

   type Ada_Subprogram_Type is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Arguments : Ada_Argument_Type_List;
   end record;

   package ASTL is new Ada.Containers.Vectors (Positive, Ada_Subprogram_Type);
   subtype Ada_Subprogram_Type_List is ASTL.Vector;

   type Ada_Package_Type is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Node : Ada.Strings.Unbounded.Unbounded_String;
      Iface : Ada.Strings.Unbounded.Unbounded_String;
      Type_Declarations : Ada_Type_Declaration_Map;
      Subprograms : Ada_Subprogram_Type_List;
   end record;
end Codegen;
