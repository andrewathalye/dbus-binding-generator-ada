--  Note: Crashes gnatpp (23/7/2024) so separate for now.
pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers.Indefinite_Ordered_Maps;

separate (Codegen.Output)
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

   --  Words that we use as binding dependencies
   type Dependent is (Interfaces, D_Bus, R_Ada, O);
   --  O is the name used for the Child_Interface controlling parameter

   package Reserved_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Reserved);

   package Dependent_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Dependent);

   --  Disallowed names
   Reserved_Map : constant Reserved_Maps.Map :=
     ["abort" => R_Abort, "abs" => R_Abs, "abstract" => R_Abstract,
      "accept" => R_Accept, "access" => R_Access, "aliased" => R_Aliased,
      "all" => R_All, "and" => R_And, "array" => R_Array, "at" => R_At,
      "begin" => R_Begin, "body" => R_Body, "case" => R_Case,
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
     ["interfaces" => Interfaces, "d_bus" => D_Bus, "ada" => R_Ada, "o" => O];

   --  Disallowed characters
   Underline_Set : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ('_');

   --  Local variables
   Lower_Name : constant String := Ada.Characters.Handling.To_Lower (Name);
   Buf : String := Name;
begin
   --  Prefix if reserved names were found
   begin
      case Reserved_Map (Lower_Name) is
         when others => return "R_" & Name;
      end case;
   exception
      when Constraint_Error => null;
   end;

   begin
      case Dependent_Map (Lower_Name) is
         when others => return "R_" & Name;
      end case;
   exception
      when Constraint_Error => null;
   end;

   --  Otherwise replace disallowed characters
   for C of Buf loop
      case C is
         when '/' | '-' | '.' =>
            C := '_';
         when others => null;
      end case;
   end loop;

   --  Remove any underlines surrounding the name
   return Ada.Strings.Fixed.Trim (Buf, Underline_Set, Underline_Set);
end Sanitise_Name;

