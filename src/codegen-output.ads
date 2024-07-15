package Codegen.Output is
   procedure Start_Container_For_Loop (Component : String; List : String);
   procedure Start_Map_For_Loop (Cursor : String; Map : String);
   procedure Start_Index_For_Loop
     (I : String; Range_1 : String; Range_2 : String);
   procedure End_For_Loop;

   procedure Use_Pragma (Expression : String);

   procedure Declare_Code;
   procedure Begin_Code;
   procedure End_Code;

   function Function_Signature (SP : Ada_Subprogram_Type) return String;
   procedure Declare_Procedure (Signature : String);
   procedure Start_Procedure (Signature : String);
   procedure End_Procedure (Name : String);

   procedure Declare_Package (Name : String; Extension : String);
   procedure Start_Package_Body (Name : String);
   procedure Start_Package (Name : String);
   procedure End_Package (Name : String);

   procedure Renames_Entity (L, T, R : String);
   procedure Use_Entity (Entity : String);
   procedure Use_Type (T : String);
   procedure Declare_Entity
     (Entity : String; EType : String; Value : String := "");
   procedure With_Entity (Entity : String);

   procedure Declare_Type (Name : String; Extension : String);
   procedure Declare_Subtype (Name : String; Extension : String);
   procedure Start_Record (Name : String);
   procedure End_Record;

   procedure Assign (Entity : String; Expression : String);
   procedure Call (Expression : String);
   procedure Start_If (Expression : String);
   procedure Start_Else;
   procedure End_If;
   procedure Raise_Exception (Name : String; Message : String := "");

   procedure New_Line;

   procedure Large_Comment (Message : String);
   procedure Comment (Message : String);
end Codegen.Output;
