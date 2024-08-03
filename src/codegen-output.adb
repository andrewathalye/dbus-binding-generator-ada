with Ada.Text_IO; use Ada.Text_IO;

package body Codegen.Output is
   File : constant Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;

   procedure Start_Container_For_Loop (Component : String; List : String) is
   begin
      Put_Line (File, "for " & Component & " of " & List & " loop");
   end Start_Container_For_Loop;

   procedure Start_Map_For_Loop (Cursor : String; Map : String) is
   begin
      Put_Line (File, "for " & Cursor & " in " & Map & ".Iterate loop");
   end Start_Map_For_Loop;

   procedure Start_Index_For_Loop
     (I : String; Range_1 : String; Range_2 : String)
   is
   begin
      Put_Line
        (File, "for " & I & " in " & Range_1 & " .. " & Range_2 & " loop");
   end Start_Index_For_Loop;

   procedure End_For_Loop is
   begin
      Put_Line (File, "end loop;");
   end End_For_Loop;

   procedure Declare_Code is
   begin
      Put_Line (File, "declare");
   end Declare_Code;

   procedure Begin_Code is
   begin
      Put_Line (File, "begin");
   end Begin_Code;

   procedure End_Code is
   begin
      Put_Line (File, "end;");
   end End_Code;

   procedure Use_Entity (Entity : String) is
   begin
      Put_Line (File, "use " & Entity & ";");
   end Use_Entity;

   procedure Renames_Entity (L, T, R : String) is
   begin
      Put_Line (File, L & " : " & T & " renames " & R & ";");
   end Renames_Entity;

   procedure Use_Type (T : String) is
   begin
      Put_Line (File, "use type " & T & ";");
   end Use_Type;

   procedure Declare_Entity
     (Entity : String; EType : String; Value : String := "")
   is
   begin
      Put (File, Entity & " : " & EType);

      if Value'Length /= 0 then
         Put (File, " := " & Value);
      end if;

      Put_Line (File, ";");
   end Declare_Entity;

   procedure Assign (Entity : String; Expression : String) is
   begin
      Put_Line (Entity & " := " & Expression & ";");
   end Assign;

   procedure Call (Expression : String) is
   begin
      Put_Line (File, Expression & ";");
   end Call;

   procedure Start_If (Expression : String) is
   begin
      Put_Line (File, "if " & Expression & " then");
   end Start_If;

   procedure End_If is
   begin
      Put_Line (File, "end if;");
   end End_If;

   procedure Raise_Exception (Name : String) is
   begin
      Put_Line (File, "raise " & Name & ";");
   end Raise_Exception;

   procedure Declare_Procedure (Signature : String) is
   begin
      Put_Line (File, "procedure " & Signature & ";");
   end Declare_Procedure;

   procedure Declare_Abstract_Procedure (Signature : String) is
   begin
      Put_Line (File, "procedure " & Signature & " is abstract;");
   end Declare_Abstract_Procedure;

   procedure Declare_Null_Procedure (Signature : String) is
   begin
      Put_Line (File, "procedure " & Signature & " is null;");
   end Declare_Null_Procedure;

   procedure Declare_Function (Signature : String) is
   begin
      Put_Line (File, "function " & Signature & ";");
   end Declare_Function;

   procedure Start_Function (Signature : String) is
   begin
      Put_Line (File, "function " & Signature & " is");
   end Start_Function;

   procedure Start_Procedure (Signature : String) is
   begin
      Put_Line (File, "procedure " & Signature & " is");
   end Start_Procedure;

   procedure End_Procedure (Name : String) is
   begin
      Put_Line (File, "end " & Name & ";");
   end End_Procedure;

   procedure Start_Package_Body (Name : String) is
   begin
      Put_Line (File, "package body " & Name & " is");
   end Start_Package_Body;

   procedure Start_Package (Name : String) is
   begin
      Put_Line (File, "package " & Name & " is");
   end Start_Package;

   procedure End_Package (Name : String) is
   begin
      Put_Line (File, "end " & Name & ";");
   end End_Package;

   procedure With_Entity (Entity : String) is
   begin
      Put_Line (File, "with " & Entity & ";");
   end With_Entity;

   procedure New_Line is
   begin
      New_Line (File);
   end New_Line;

   procedure Use_Pragma (Expression : String) is
   begin
      Put_Line (File, "pragma " & Expression & ";");
   end Use_Pragma;

   procedure Large_Comment (Message : String) is
      Comment          : constant String := "-- " & Message & " --";
      Comment_Ornament : constant String (Comment'Range) := (others => '-');
   begin
      Put_Line (File, Comment_Ornament);
      Put_Line (File, Comment);
      Put_Line (File, Comment_Ornament);
   end Large_Comment;

   procedure Comment (Message : String) is
   begin
      Put_Line (File, "--  " & Message);
   end Comment;

   procedure Declare_Type (Name : String; Extension : String) is
   begin
      Put_Line (File, "type " & Name & " is " & Extension & ";");
   end Declare_Type;

   procedure Declare_Subtype (Name : String; Extension : String) is
   begin
      Put_Line (File, "subtype " & Name & " is " & Extension & ";");
   end Declare_Subtype;

   procedure Declare_Package (Name : String; Extension : String) is
   begin
      Put_Line (File, "package " & Name & " is " & Extension & ";");
   end Declare_Package;

   procedure Start_Record (Name : String) is
   begin
      Put_Line (File, "type " & Name & " is record");
   end Start_Record;

   procedure End_Record is
   begin
      Put_Line (File, "end record;");
   end End_Record;

   procedure Return_Null is
   begin
      Put_Line (File, "return;");
   end Return_Null;

   procedure Return_Entity (Name : String) is
   begin
      Put_Line (File, "return " & Name & ";");
   end Return_Entity;

   -------------------
   -- Sanitise_Name --
   -------------------
   --  See the spec for documentation
   --  See `codegen-output-sanitise_name.adb` for justification.
   function Sanitise_Name (Name : String) return String is separate;
end Codegen.Output;
