package Codegen.Output is
   --  Loops and Control Flow
   procedure Start_Container_For_Loop (Component : String; List : String);
   procedure Start_Map_For_Loop (Cursor : String; Map : String);
   procedure Start_Index_For_Loop
     (I : String; Range_1 : String; Range_2 : String);
   procedure End_For_Loop;
   procedure Start_If (Expression : String);
   procedure Start_Else;
   procedure End_If;

   --  Pragmas
   procedure Use_Pragma (Expression : String);

   --  Code Blocks
   procedure Declare_Code;
   procedure Begin_Code;
   procedure Exception_Code;
   procedure When_Exception (Name : String);
   procedure End_Code;

   --  Subprograms
   --  `Signature` means "<Name> (<Parameters>) [return <Type>]"
   procedure Declare_Procedure (Signature : String);
   procedure Declare_Function (Signature : String);
   procedure Start_Procedure (Signature : String);
   procedure Start_Function (Signature : String);
   procedure End_Procedure (Name : String);
   procedure End_Function (Name : String) renames End_Procedure;
   procedure Return_Entity (Name : String);

   --  Packages
   procedure Declare_Package (Name : String; Extension : String);
   procedure Start_Package_Body (Name : String);
   procedure Start_Package (Name : String);
   procedure Private_Package;
   procedure End_Package (Name : String);

   --  Entity Clauses
   procedure Renames_Entity (L, T, R : String);
   procedure Use_Entity (Entity : String);
   procedure Use_Type (T : String);
   procedure Declare_Entity
     (Entity : String; EType : String; Value : String := "");
   procedure With_Entity (Entity : String);
   procedure Private_With_Entity (Entity : String);

   --  Types
   procedure Declare_Types_Package (Types_Pkg : Ada_Types_Package_Type);
   procedure Declare_Type (Name : String; Extension : String);
   procedure Declare_Subtype (Name : String; Extension : String);
   procedure Start_Record (Name : String);
   procedure End_Record;

   --  Assignment and Invocation
   procedure Assign (Entity : String; Expression : String);
   procedure Call (Expression : String);
   procedure Raise_Exception (Name : String; Message : String := "");

   --  Comments and Formatting
   procedure Large_Comment (Message : String);
   procedure Comment (Message : String);
   procedure New_Line;

   --  Sanitising
   function Sanitise_Name (Name : String) return String;
   pragma Pure_Function (Sanitise_Name);
   --  Sanitise a name so that it is legal for use as an Ada identifier.
   --  The actual result of this process is opaque, but a consistent result
   --  will be produced for any given `Name`.

private
   function Get_Arguments
     (AL : Parsing.Argument_List; Client : Boolean) return String;
   --  Get the Ada arguments for an argument list.
   --  This should be accessed via the `Client` and `Server` child packages
end Codegen.Output;
