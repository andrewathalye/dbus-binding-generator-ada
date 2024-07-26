pragma Ada_2022;

with Codegen.Output;

with Parsing; use Parsing;

with Type_Checking; use Type_Checking;

with Shared; use Shared;
with Debug;  use Debug;

package body Codegen is
   --  Generate Ada type declarations for an argument
   procedure Generate_Ada_Types
     (Map       : in out Ada_Type_Declaration_Map;
      Type_Code :        Ada.Strings.Unbounded.Unbounded_String);
   procedure Generate_Ada_Types
     (Map       : in out Ada_Type_Declaration_Map;
      Type_Code :        Ada.Strings.Unbounded.Unbounded_String)
   is
      type Type_Category is (DBus_Array, DBus_Struct, DBus_Dict);

      Type_First : constant Character := String'(+Type_Code) (1);

      --  Produce declarations for complex types
      procedure Internal (T : Type_Category);
      procedure Internal (T : Type_Category) is
         use Ada.Strings.Unbounded;

         Interior_S  : constant String           := Get_Interior (+Type_Code);
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
                  Array_Decl.Name                    := Ada_Type_Name;
                  Array_Decl.Type_Code               := Type_Code;
                  Array_Decl.Array_Element_Type_Code := Interior_UB;

                  Map.Insert (Type_Code, Array_Decl);
               end;
            when DBus_Struct =>
               declare
                  I           : Positive;
                  Struct_Decl : Ada_Type_Declaration (Struct_Kind);
               begin
                  Struct_Decl.Name      := Ada_Type_Name;
                  Struct_Decl.Type_Code := Type_Code;

                  --  Declare and add types
                  I := 1;
                  loop
                     declare
                        Type_Code : Ada.Strings.Unbounded.Unbounded_String;
                     begin
                        Type_Code := +Get_Complete_Type (Interior_S, I);

                        --  Generate types recursively
                        Generate_Ada_Types (Map, Type_Code);

                        --  Add the struct declaration
                        Struct_Decl.Struct_Members.Append
                          (Ada_Record_Member_Type'
                             (Name      =>
                                +("Member_" &
                                 I'Image (I'Image'First + 1 .. I'Image'Last)),
                              Type_Code =>
                                +Get_Complete_Type (Interior_S, I)));
                     exception
                        when No_More_Complete_Types =>
                           exit;
                     end;

                     I := I + 1;
                  end loop;

                  Map.Insert (Type_Code, Struct_Decl);
               end;
            when DBus_Dict =>
               declare
                  Key_Type     : constant Unbounded_String :=
                    +Get_Complete_Type (Interior_S, 1);
                  Element_Type : constant Unbounded_String :=
                    +Get_Complete_Type (Interior_S, 2);

                  Dict_Decl : Ada_Type_Declaration;
               begin
                  --  A dict key must be a basic type
                  if not Is_Basic (+Key_Type) then
                     raise Program_Error
                       with "Key " & (+Key_Type) & " is a complex type";
                  end if;

                  Generate_Ada_Types (Map, Key_Type);
                  Generate_Ada_Types (Map, Element_Type);

                  --  Update the dict kind based upon the key type
                  if Is_Stringlike (+Key_Type) then
                     Dict_Decl := (Kind => Hashed_Dict_Kind, others => <>);
                  else
                     Dict_Decl := (Kind => Ordered_Dict_Kind, others => <>);
                  end if;

                  --  Fill in the record
                  Dict_Decl.Name                   := Ada_Type_Name;
                  Dict_Decl.Type_Code              := Type_Code;
                  Dict_Decl.Dict_Key_Type_Code     := Key_Type;
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
         when 'v' =>
            Map.Insert
              (Type_Code,
              (Kind       => Variant_Kind, Name => +Get_Ada_Type (+Type_Code),
                Type_Code => Type_Code));
         when others =>
            Map.Insert
              (Type_Code,
              (Kind       => Basic_Kind, Name => +Get_Ada_Type (+Type_Code),
                Type_Code => Type_Code));
      end case;
   end Generate_Ada_Types;

   --  Ensure that all Method parameters are named
   procedure Name_Parameters (M : in out Method_Type);
   procedure Name_Parameters (M : in out Method_Type) is
      use Ada.Strings.Unbounded;

      FI : constant Positive := M.Arguments.First_Index;
      LI : constant Natural  := M.Arguments.Last_Index;
   begin
      for I in FI .. LI loop
         M.Arguments (I).Name :=
           +Codegen.Output.Sanitise_Name (+M.Arguments (I).Name);

         --  Give a unique name to unnamed parameters
         if M.Arguments (I).Name = Null_Unbounded_String then
            M.Arguments (I).Name :=
              +("Parameter_" & I'Image (I'Image'First + 1 .. I'Image'Last));
         end if;
      end loop;
   end Name_Parameters;

   --------------------
   -- Create_Package --
   --------------------
   function Create_Package (I : Interface_Type) return Ada_Package_Type is
      use Ada.Strings.Unbounded;

      Pkg : Ada_Package_Type;
   begin
      Append (Pkg.Name, Codegen.Output.Sanitise_Name (+I.Name));

      Pkg.Real_Name := I.Name;

      -----------
      -- Types --
      -----------
      for M of I.Methods loop
         for A of M.Arguments loop
            Put_Debug ("Generate Type: " & (+A.Type_Code));
            Generate_Ada_Types (Pkg.Type_Declarations, A.Type_Code);
         end loop;
      end loop;

      for S of I.Signals loop
         for A of S.Arguments loop
            Put_Debug ("Generate Type: " & (+A.Type_Code));
            Generate_Ada_Types (Pkg.Type_Declarations, A.Type_Code);
         end loop;
      end loop;

      for P of I.Properties loop
         Put_Debug ("Generate Type: " & (+P.Type_Code));
         Generate_Ada_Types (Pkg.Type_Declarations, P.Type_Code);
      end loop;

      -----------------
      -- Subprograms --
      -----------------
      for M of I.Methods loop
         declare
            M2 : Method_Type := M;
         begin
            Name_Parameters (M2);
            Pkg.Methods.Append (M2);
         end;
      end loop;

      for S of I.Signals loop
         declare
            S2 : Method_Type := S;
         begin
            Name_Parameters (S2);
            Pkg.Signals.Append (S2);
         end;
      end loop;

      Pkg.Properties := I.Properties;

      return Pkg;
   end Create_Package;
end Codegen;
