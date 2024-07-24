with Ada.Text_IO; use Ada.Text_IO;

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

   procedure Private_With_Entity (Entity : String) is
   begin
      Put_Line (File, "private with " & Entity & ";");
   end Private_With_Entity;

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
        (case D is when Parsing.DIn => (if Client then "in" else "out"),
           when Parsing.DOut => (if Client then "out" else "in"));
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

   procedure Private_Package is
   begin
      Put_Line (File, "private");
   end Private_Package;

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

   ---------------------------
   -- Declare_Types_Package --
   ---------------------------
   --  Declare a package containing the types in `Types_Pkg`
   procedure Declare_Types_Package (Types_Pkg : Ada_Types_Package_Type) is
      --  Produce an acceptable order for dependency resolution
      --  'Elaborate' the type declaration order
      package ATDL is new Ada.Containers.Vectors
        (Positive, Ada_Type_Declaration);
      subtype Ada_Type_Declaration_List is ATDL.Vector;

      function Resolve_Dependencies return Ada_Type_Declaration_List;
      function Resolve_Dependencies return Ada_Type_Declaration_List is
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
                       (Types_Pkg.Type_Declarations
                          (TD.Array_Element_Type_Code));
                  end if;
               when Ordered_Dict_Kind | Hashed_Dict_Kind =>
                  if not Bookkeeping.Contains (TD.Dict_Key_Type_Code) then
                     Resolve_Dependency
                       (Types_Pkg.Type_Declarations (TD.Dict_Key_Type_Code));
                  end if;

                  if not Bookkeeping.Contains (TD.Dict_Element_Type_Code) then
                     Resolve_Dependency
                       (Types_Pkg.Type_Declarations
                          (TD.Dict_Element_Type_Code));
                  end if;
               when Struct_Kind =>
                  for SM of TD.Struct_Members loop
                     if not Bookkeeping.Contains (SM.Type_Code) then
                        Resolve_Dependency
                          (Types_Pkg.Type_Declarations (SM.Type_Code));
                     end if;
                  end loop;
            end case;

            Bookkeeping.Append (TD.Type_Code);
            Result.Append (TD);
         end Resolve_Dependency;
      begin
         for TD of Types_Pkg.Type_Declarations loop
            if not Bookkeeping.Contains (TD.Type_Code) then
               Resolve_Dependency (TD);
            end if;
         end loop;
         return Result;
      end Resolve_Dependencies;
   begin
      --!pp off
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Codegen.Output.New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");
      With_Entity ("Ada.Strings.Unbounded.Hash");
      With_Entity ("Ada.Containers.Vectors");
      With_Entity ("Ada.Containers.Ordered_Maps");
      With_Entity ("Ada.Containers.Hashed_Maps");
      With_Entity ("Interfaces");
      Use_Entity ("Interfaces");
      Codegen.Output.New_Line;

      With_Entity ("GNAT.OS_Lib");
      Codegen.Output.New_Line;

      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Arguments.Containers");
      Use_Type ("D_Bus.Arguments.Containers.Variant_Type");
      With_Entity ("D_Bus.Extra");
      With_Entity ("D_Bus.Support");
      Codegen.Output.New_Line;

      Start_Package ("D_Bus.Generated_Types");
         --  Generated Types
         for TD of Resolve_Dependencies loop
            case TD.Kind is
               when Basic_Kind | Variant_Kind =>
                  null;
               when Array_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Vectors (Positive, " &
                     (+Types_Pkg.Type_Declarations
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
                        (+Types_Pkg.Type_Declarations (SM.Type_Code).Name));
                  end loop;
                  End_Record;
                  Codegen.Output.New_Line;
               when Ordered_Dict_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Ordered_Maps (" &
                     (+Types_Pkg.Type_Declarations
                       (TD.Dict_Key_Type_Code).Name) &
                     ", " &
                     (+Types_Pkg.Type_Declarations
                        (TD.Dict_Element_Type_Code).Name) &
                     ")");

                  Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
                  Use_Type (+TD.Name);
                  Codegen.Output.New_Line;
               when Hashed_Dict_Kind =>
                  Declare_Package
                    ("Pkg_" & (+TD.Name),
                     "new Ada.Containers.Hashed_Maps (" &
                     (+Types_Pkg.Type_Declarations
                       (TD.Dict_Key_Type_Code).Name) &
                     ", " &
                     (+Types_Pkg.Type_Declarations
                        (TD.Dict_Element_Type_Code).Name) &
                     ", Ada.Strings.Unbounded.Hash, " & ASCII.Quotation & "=" &
                     ASCII.Quotation & ")");

                  Declare_Subtype (+TD.Name, "Pkg_" & (+TD.Name) & ".Map");
                  Use_Type (+TD.Name);
                  Codegen.Output.New_Line;
            end case;
         end loop;
      End_Package ("D_Bus.Generated_Types");
      --!pp on
   end Declare_Types_Package;

   -------------------
   -- Sanitise_Name --
   -------------------
   --  See the spec for documentation
   --  See `codegen-output-sanitise_name.adb` for justification.
   function Sanitise_Name (Name : String) return String is separate;

   procedure Exception_Code is
   begin
      Put_Line (File, "exception");
   end Exception_Code;
   procedure When_Exception (Name : String) is
   begin
      Put_Line (File, "when X : " & Name & " =>");
   end When_Exception;
end Codegen.Output;
