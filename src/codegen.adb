with Parsing; use Parsing;

with Type_Checking; use Type_Checking;

with Shared; use Shared;

package body Codegen is
   --  Generate Ada type declarations for an argument
   procedure Generate_Ada_Types
     (Map : in out Ada_Type_Declaration_Map;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String);
   procedure Generate_Ada_Types
     (Map : in out Ada_Type_Declaration_Map;
     Type_Code : Ada.Strings.Unbounded.Unbounded_String)
   is
      type Type_Category is (DBus_Array, DBus_Struct, DBus_Dict);

      Type_First : constant Character := String'(+Type_Code) (1);

      --  Produce declarations for complex types
      procedure Internal (T : Type_Category);
      procedure Internal (T : Type_Category) is
         use Ada.Strings.Unbounded;

         Interior_S : constant String := Get_Interior (+Type_Code);
         Interior_UB : constant Unbounded_String := +Interior_S;

         Ada_Type_Name : constant Ada.Strings.Unbounded.Unbounded_String :=
            +Get_Ada_Type (+Type_Code);
      begin
         case T is
            when DBus_Array =>
               Generate_Ada_Types (Map, Interior_UB);

               declare
                  Array_Decl : Ada_Type_Declaration (Array_Kind);
               begin
                  Array_Decl.Name := Ada_Type_Name;
                  Array_Decl.Array_Element_Type_Code := Interior_UB;

                  Map.Insert (Type_Code, Array_Decl);
               end;
            when DBus_Struct =>
               declare
                  I : Positive;
                  Struct_Decl : Ada_Type_Declaration (Struct_Kind);
               begin
                  Struct_Decl.Name := Ada_Type_Name;

                  --  Ensure all types are declared
                  I := 1;
                  loop
                     begin
                        Generate_Ada_Types
                          (Map, +Get_Complete_Type (Interior_S, I));
                     exception
                        when No_More_Complete_Types => exit;
                     end;

                     I := I + 1;
                  end loop;

                  --  Declare the record itself
                  I := 1;
                  loop
                     begin
                        Struct_Decl.Struct_Members.Append
                          (Ada_Record_Member_Type'
                           (Name =>
                              +("Member_" &
                              I'Image (I'Image'First + 1 .. I'Image'Last)),
                           Type_Code => +Get_Complete_Type (Interior_S, I)));
                     exception
                        when No_More_Complete_Types => exit;
                     end;

                     I := I + 1;
                  end loop;

                  Map.Insert (Type_Code, Struct_Decl);
               end;
            when DBus_Dict =>
               declare
                  Key_Type : constant Unbounded_String :=
                     +Get_Complete_Type (Interior_S, 1);
                  Element_Type : constant Unbounded_String :=
                     +Get_Complete_Type (Interior_S, 2);

                  Dict_Decl : Ada_Type_Declaration (Dict_Kind);
               begin
                  --  A dict key must be a basic type
                  if not Is_Basic (+Key_Type) then
                     raise Program_Error with
                        "Key " & (+Key_Type) & " is a complex type";
                  end if;

                  Generate_Ada_Types (Map, Key_Type);
                  Generate_Ada_Types (Map, Element_Type);

                  Dict_Decl.Name := Ada_Type_Name;
                  Dict_Decl.Dict_Key_Type_Code := Key_Type;
                  Dict_Decl.Dict_Element_Type_Code := Element_Type;

                  Map.Insert (Type_Code, Dict_Decl);
               end;
         end case;
      end Internal;
   begin
      --  Avoid duplicate type declarations
      if Map.Contains (Type_Code) then
         return;
      end if;

      --  Complex types need type definitions
      --  Basic types get Builtin_Kind stubs
      case Type_First is
         --  Check for dicts
         when 'a' =>
            if String'(+Type_Code) (2) = '{' then
               Internal (DBus_Dict);
            else
               Internal (DBus_Array);
            end if;
         when '(' =>
            Internal (DBus_Struct);
         when others =>
            Map.Insert
              (Type_Code,
               (Kind => Builtin_Kind,
                Name => +Get_Ada_Type (+Type_Code)));
      end case;
   end Generate_Ada_Types;

   --  Create an Ada argument for a DBus argument
   function Create_Argument (A : Argument_Type) return Ada_Argument_Type
   is (Name => A.Name,
       Type_Code => A.AType,
       Direction => A.Direction);

   --  Create a subprogram for a method
   function Create_Subprogram (M : Method_Type) return Ada_Subprogram_Type;
   function Create_Subprogram (M : Method_Type) return Ada_Subprogram_Type
   is
      use Ada.Strings.Unbounded;

      FI : constant Positive := M.Arguments.First_Index;
      LI : constant Positive := M.Arguments.Last_Index;

      Sp : Ada_Subprogram_Type;
   begin
      Sp.Name := M.Name;

      for I in FI .. LI loop
         declare
            Arg : Ada_Argument_Type;
         begin
            Arg := Create_Argument (M.Arguments (I));

            --  Give a unique name to unnamed parameters
            if Arg.Name = Null_Unbounded_String then
               Arg.Name := +("Parameter_" &
                  I'Image (I'Image'First + 1 .. I'Image'Last));
            end if;

            Sp.Arguments.Append (Arg);
         end;
      end loop;

      return Sp;
   end Create_Subprogram;

   ---------------------
   -- Print_Signature --
   ---------------------
   procedure Print_Signature
     (SP : Ada_Subprogram_Type;
      File : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (File, "procedure " & (+SP.Name));

      if not SP.Arguments.Is_Empty then
         Put (File, " (");

         declare
            SPA : Ada_Argument_Type_List renames SP.Arguments;
            FI : constant Positive := SPA.First_Index;
            LI : constant Positive := SPA.Last_Index;
         begin
            for I in FI .. LI loop
               Put
                 (File,
                  (+SPA (I).Name) &
                  " : " &
                  (+SPA (I).Direction) &
                  " " &
                  (Get_Ada_Type (+SPA (I).Type_Code)));

               if I /= LI then
                  Put ("; ");
               end if;
            end loop;
         end;

         Put (File, ")");
      end if;
   end Print_Signature;

   --------------------
   -- Create_Package --
   --------------------
   function Create_Package
     (Node : Ada.Strings.Unbounded.Unbounded_String;
      I : Interface_Type) return Ada_Package_Type
   is
      Name : String := +I.Name;

      Pkg : Ada_Package_Type;
   begin
      ------------
      -- Fixups --
      ------------
      for C of Name loop
         if C = '.' then
            C := '_';
         end if;
      end loop;

      Pkg.Name := +Name;
      Pkg.Node := Node;
      Pkg.Iface := I.Name;

      -----------
      -- Types --
      -----------
      for M of I.Methods loop
         for A of M.Arguments loop
            Generate_Ada_Types (Pkg.Type_Declarations, A.AType);
         end loop;
      end loop;

      -----------------
      -- Subprograms --
      -----------------
      for M of I.Methods loop
         Pkg.Subprograms.Append (Create_Subprogram (M));
      end loop;

      return Pkg;
   end Create_Package;
end Codegen;
