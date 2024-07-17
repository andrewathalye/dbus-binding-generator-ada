pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;

with Ada.Containers.Indefinite_Ordered_Maps;

with Type_Checking; use Type_Checking;
with Shared;        use Shared;

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

   procedure Start_Else is
   begin
      Put_Line (File, "else");
   end Start_Else;

   procedure End_If is
   begin
      Put_Line (File, "end if;");
   end End_If;

   procedure Raise_Exception (Name : String; Message : String := "") is
   begin
      Put_Line
        (File,
         "raise " & Name & " with " & ASCII.Quotation & Message &
         ASCII.Quotation & ";");
   end Raise_Exception;

   procedure Declare_Procedure (Signature : String) is
   begin
      Put_Line (File, "procedure " & Signature & ";");
   end Declare_Procedure;

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

   -------------------
   -- Get_Arguments --
   -------------------
   function Get_Arguments
     (AL : Parsing.Argument_List; Client : Boolean) return String
   is
      use Ada.Strings.Unbounded;
      Buf : Unbounded_String;

      function To_Ada_Direction (D : Parsing.DBus_Direction) return String is
        (case D is
            when Parsing.DIn =>
               (if Client then "in" else "out"),
            when Parsing.DOut =>
               (if Client then "out" else "in"));
   begin
      if not AL.Is_Empty then
         Append (Buf, " (");

         declare
            FI : constant Positive := AL.First_Index;
            LI : constant Positive := AL.Last_Index;
         begin
            for I in FI .. LI loop
               Append
                 (Buf,
                  (+AL (I).Name) & " : " &
                  To_Ada_Direction (AL (I).Direction) & " " &
                  (Get_Ada_Type (+AL (I).Type_Code)));

               if I /= LI then
                  Append (Buf, "; ");
               end if;
            end loop;
         end;

         Append (Buf, ")");
      end if;

      return To_String (Buf);
   end Get_Arguments;

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

   procedure Return_Entity (Name : String) is
   begin
      Put_Line (File, "return " & Name & ";");
   end Return_Entity;

   --  Declare all Ada Types contained in TDM in the correct order
   procedure Declare_Types (Pkg : Ada_Package_Type) is
      --  Produce an acceptable order for dependency resolution
      --  'Elaborate' the type declaration order
      package ATDL is new Ada.Containers.Vectors
        (Positive, Ada_Type_Declaration);
      subtype Ada_Type_Declaration_List is ATDL.Vector;

      function Resolve_Dependencies return Ada_Type_Declaration_List;
      function Resolve_Dependencies return Ada_Type_Declaration_List
      is
         package USL is new Ada.Containers.Vectors
           (Positive, Ada.Strings.Unbounded.Unbounded_String);
         subtype Unbounded_String_List is USL.Vector;

         Result      : Ada_Type_Declaration_List;
         Bookkeeping : Unbounded_String_List;

         procedure Resolve_Dependency (TD : Ada_Type_Declaration);
         procedure Resolve_Dependency (TD : Ada_Type_Declaration) is
         begin
            case TD.Kind is
               when Basic_Kind | Variant_Kind =>
                  null;
               when Array_Kind =>
                  if not Bookkeeping.Contains (TD.Array_Element_Type_Code) then
                     Resolve_Dependency
                       (Pkg.Type_Declarations (TD.Array_Element_Type_Code));
                  end if;
               when Ordered_Dict_Kind | Hashed_Dict_Kind =>
                  if not Bookkeeping.Contains (TD.Dict_Key_Type_Code) then
                     Resolve_Dependency
                       (Pkg.Type_Declarations (TD.Dict_Key_Type_Code));
                  end if;

                  if not Bookkeeping.Contains (TD.Dict_Element_Type_Code) then
                     Resolve_Dependency
                       (Pkg.Type_Declarations (TD.Dict_Element_Type_Code));
                  end if;
               when Struct_Kind =>
                  for SM of TD.Struct_Members loop
                     if not Bookkeeping.Contains (SM.Type_Code) then
                        Resolve_Dependency
                          (Pkg.Type_Declarations (SM.Type_Code));
                     end if;
                  end loop;
            end case;

            Bookkeeping.Append (TD.Type_Code);
            Result.Append (TD);
         end Resolve_Dependency;
      begin
         for TD of Pkg.Type_Declarations loop
            if not Bookkeeping.Contains (TD.Type_Code) then
               Resolve_Dependency (TD);
            end if;
         end loop;
         return Result;
      end Resolve_Dependencies;
   begin
      for TD of Resolve_Dependencies loop
         case TD.Kind is
            when Basic_Kind | Variant_Kind =>
               null;
            when Array_Kind =>
               Declare_Package
                 ("Pkg_" & (+TD.Name),
                  "new Ada.Containers.Vectors (Positive, " &
                  (+Pkg.Type_Declarations
                     (TD.Array_Element_Type_Code).Name) &
                  ")");

               Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Vector");
               Use_Type (+TD.Name);
               Codegen.Output.New_Line;
            when Struct_Kind =>
               Start_Record (+TD.Name);
               for SM of TD.Struct_Members loop
                  Declare_Entity
                    (+SM.Name,
                     (+Pkg.Type_Declarations (SM.Type_Code).Name));
               end loop;
               End_Record;
               Codegen.Output.New_Line;
            when Ordered_Dict_Kind =>
               Declare_Package
                 ("Pkg_" & (+TD.Name),
                  "new Ada.Containers.Ordered_Maps (" &
                  (+Pkg.Type_Declarations (TD.Dict_Key_Type_Code).Name) &
                  ", " &
                  (+Pkg.Type_Declarations
                     (TD.Dict_Element_Type_Code).Name) &
                  ")");

               Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
               Use_Type (+TD.Name);
               Codegen.Output.New_Line;
            when Hashed_Dict_Kind =>
               Declare_Package
                 ("Pkg_" & (+TD.Name),
                  "new Ada.Containers.Hashed_Maps (" &
                  (+Pkg.Type_Declarations (TD.Dict_Key_Type_Code).Name) &
                  ", " &
                  (+Pkg.Type_Declarations
                     (TD.Dict_Element_Type_Code).Name) &
                  ", Ada.Strings.Unbounded.Hash, " & ASCII.Quotation & "=" &
                  ASCII.Quotation & ")");

               Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
               Use_Type (+TD.Name);
               Codegen.Output.New_Line;
         end case;
      end loop;
   end Declare_Types;

   -------------------
   -- Sanitise_Name --
   -------------------
   --  See the spec for documentation
   function Sanitise_Name (Name : String) return String is
      --  Words that the Ada standard forbids in names
      type Reserved is
        (R_Abort, R_Abs, R_Abstract, R_Accept, R_Access, R_Aliased, R_All,
         R_And, R_Array, R_At, R_Begin, R_Body, R_Case, R_Constant, R_Declare,
         R_Delay, R_Delta, R_Digits, R_Do, R_Else, R_Elsif, R_End, R_Entry,
         R_Exception, R_Exit, R_For, R_Function, R_Generic, R_Goto, R_If, R_In,
         R_Interface, R_Is, R_Limited, R_Loop, R_Mod, R_New, R_Not, R_Null,
         R_Of, R_Or, R_Others, R_Out, R_Overriding, R_Package, R_Pragma,
         R_Private, R_Procedure, R_Protected, R_Raise, R_Range, R_Record,
         R_Rem, R_Renames, R_Requeue, R_Return, R_Reverse, R_Select,
         R_Separate, R_Subtype, R_Synchronized, R_Tagged, R_Task, R_Terminate,
         R_Then, R_Type, R_Until, R_Use, R_When, R_While, R_With, R_Xor);

      --  Words that we use non-negociably as binding dependencies
      type Dependent is (Interfaces);

      package Reserved_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String, Reserved);

      package Dependent_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String, Dependent);

      Reserved_Map : constant Reserved_Maps.Map :=
        ["abort" => R_Abort, "abs" => R_Abs, "abstract" => R_Abstract,
         "accept" => R_Accept, "access" => R_Access, "aliased" => R_Aliased,
         "all" => R_All, "and" => R_And, "array" => R_Array, "at" => R_At,
         "Begin" => R_Begin, "body" => R_Body, "case" => R_Case,
         "constant" => R_Constant, "declare" => R_Declare, "delay" => R_Delay,
         "delta" => R_Delta, "digits" => R_Digits, "do" => R_Do,
         "else" => R_Else, "elsif" => R_Elsif, "end" => R_End,
         "entry" => R_Entry, "exception" => R_Exception, "exit" => R_Exit,
         "for" => R_For, "function" => R_Function, "generic" => R_Generic,
         "goto" => R_Goto, "if" => R_If, "in" => R_In,
         "interface" => R_Interface, "is" => R_Is, "limited" => R_Limited,
         "loop" => R_Loop, "mod" => R_Mod, "new" => R_New, "not" => R_Not,
         "null" => R_Null, "of" => R_Of, "or" => R_Or, "others" => R_Others,
         "out" => R_Out, "overriding" => R_Overriding, "package" => R_Package,
         "pragma" => R_Pragma, "private" => R_Private,
         "procedure" => R_Procedure, "protected" => R_Protected,
         "raise" => R_Raise, "range" => R_Range, "record" => R_Record,
         "rem" => R_Rem, "renames" => R_Renames, "requeue" => R_Requeue,
         "return" => R_Return, "reverse" => R_Reverse, "select" => R_Select,
         "separate" => R_Separate, "subtype" => R_Subtype,
         "synchronized" => R_Synchronized, "tagged" => R_Tagged,
         "task" => R_Task, "terminate" => R_Terminate,
         "then" => R_Then, "type" => R_Type, "until" => R_Until,
         "use" => R_Use, "when" => R_When, "while" => R_While,
         "with" => R_With, "xor" => R_Xor];

      Dependent_Map : constant Dependent_Maps.Map :=
        ["interfaces" => Interfaces];

      Lower_Name : constant String := Ada.Characters.Handling.To_Lower (Name);
   begin
      --  Sanitise the name or continue if not matched
      begin
         case Reserved_Map (Lower_Name) is
            when others => return "R_" & Name;
         end case;
      exception
         when Constraint_Error => null;
      end;

      --  Sanitise the name or return if not matched
      begin
         case Dependent_Map (Lower_Name) is
            when others => return "R_" & Name;
         end case;
      exception
         when Constraint_Error => return Name;
      end;
   end Sanitise_Name;

   procedure Exception_Code is
   begin
      Put_Line (File, "exception");
   end Exception_Code;
   procedure When_Exception (Name : String) is
   begin
      Put_Line (File, "When X : " & Name & " =>");
   end When_Exception;
end Codegen.Output;
