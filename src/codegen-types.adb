pragma Ada_2012;

with Codegen.Output;             use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
with Codegen.Binding;            use Codegen.Binding;

with Shared;               use Shared;
with Signatures.Unbounded; use Signatures.Unbounded;
with Signatures;           use Signatures;
with Debug;                use Debug;

package body Codegen.Types is
   ------------------------
   -- Generate_Ada_Types --
   ------------------------
   procedure Generate_Ada_Types
     (Map : in out Ada_Type_Declaration_Map; Type_Code : Unbounded_Signature);
   procedure Generate_Ada_Types
     (Map : in out Ada_Type_Declaration_Map; Type_Code : Unbounded_Signature)
   is
      type Type_Category is (DBus_Array, DBus_Struct, DBus_Dict);

      --  Produce declarations for complex types
      procedure Internal (T : Type_Category);
      procedure Internal (T : Type_Category) is
         use Ada.Strings.Unbounded;

         Interior_S  : constant Signature := Get_Interior (+Type_Code);
         Interior_UB : constant Unbounded_Signature := +Interior_S;
      begin
         case T is
            when DBus_Array =>
               Generate_Ada_Types (Map, Interior_UB);

               declare
                  Array_Decl : Ada_Type_Declaration (Array_Kind);
               begin
                  Array_Decl.Type_Code               := Type_Code;
                  Array_Decl.Array_Element_Type_Code := Interior_UB;

                  Map.Insert (Type_Code, Array_Decl);
               end;
            when DBus_Struct =>
               declare
                  I           : Positive;
                  Struct_Decl : Ada_Type_Declaration (Struct_Kind);
               begin
                  Struct_Decl.Type_Code := Type_Code;

                  --  Declare and add types
                  I := 1;
                  loop
                     declare
                        SM_Type_Code : Unbounded_Signature;
                     begin
                        SM_Type_Code := +Get_Complete_Type (Interior_S, I);

                        --  Generate types recursively
                        Generate_Ada_Types (Map, SM_Type_Code);

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
                  Key_Type     : constant Unbounded_Signature :=
                    +Get_Complete_Type (Interior_S, 1);
                  Element_Type : constant Unbounded_Signature :=
                    +Get_Complete_Type (Interior_S, 2);

                  Dict_Decl : Ada_Type_Declaration;
               begin
                  --  A dict key must be a basic type
                  if not Is_Basic (+Key_Type) then
                     raise Program_Error
                       with "Key " & As_String (+Key_Type) &
                       " is a complex type";
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
                  Dict_Decl.Type_Code              := Type_Code;
                  Dict_Decl.Dict_Key_Type_Code     := Key_Type;
                  Dict_Decl.Dict_Element_Type_Code := Element_Type;

                  Map.Insert (Type_Code, Dict_Decl);
               end;
         end case;
      end Internal;

      TC : constant Signature := +Type_Code;
   begin
      --  Avoid duplicate type declarations
      if Map.Contains (Type_Code) then
         return;
      end if;

      Put_Debug (As_String (TC));

      --  Complex types need type definitions
      --  Basic types get Builtin_Kind stubs
      case Starting_Type (TC (TC'First)) is
         --  Check for dicts
         when 'a' =>
            if TC (TC'First + 1) = '{' then
               Internal (DBus_Dict);
            else
               Internal (DBus_Array);
            end if;
         when '(' =>
            Internal (DBus_Struct);
         when 'v' =>
            Map.Insert
              (Type_Code, (Kind => Variant_Kind, Type_Code => Type_Code));
         when others =>
            Map.Insert
              (Type_Code, (Kind => Basic_Kind, Type_Code => Type_Code));
      end case;
   end Generate_Ada_Types;

   ---------------------------------
   -- Calculate_Request_Signature --
   ---------------------------------
   function Calculate_Request_Signature
     (Arguments : Parsing.Argument_List) return String
   is
      use Ada.Strings.Unbounded;
      use type Parsing.DBus_Direction;

      Buf : Unbounded_String;
   begin
      for A of Arguments loop
         if A.Direction = Parsing.DIn then
            Append (Buf, As_String (+A.Type_Code));
         end if;
      end loop;

      return To_String (Buf);
   end Calculate_Request_Signature;

   -------------------------------
   -- Calculate_Reply_Signature --
   -------------------------------
   function Calculate_Reply_Signature
     (Arguments : Parsing.Argument_List) return String
   is
      use Ada.Strings.Unbounded;
      use type Parsing.DBus_Direction;

      Buf : Unbounded_String;
   begin
      for A of Arguments loop
         if A.Direction = Parsing.DOut then
            Append (Buf, As_String (+A.Type_Code));
         end if;
      end loop;

      return To_String (Buf);
   end Calculate_Reply_Signature;

   ---------------
   -- Add_Types --
   ---------------
   procedure Add_Types
     (Types : in out Ada_Type_Declaration_Map; Pkg : Ada_Package_Type)
   is
   begin
      for M of Pkg.Methods loop
         for A of M.Arguments loop
            Generate_Ada_Types (Types, A.Type_Code);
         end loop;
      end loop;

      for S of Pkg.Signals loop
         for A of S.Arguments loop
            Generate_Ada_Types (Types, A.Type_Code);
         end loop;
      end loop;

      for P of Pkg.Properties loop
         Generate_Ada_Types (Types, P.Type_Code);
      end loop;
   end Add_Types;

   -----------
   -- Print --
   -----------
   procedure Print (Types : Ada_Type_Declaration_Map) is
      --  Produce an acceptable order for dependency resolution
      --  'Elaborate' the type declaration order
      package ATDL is new Ada.Containers.Vectors
        (Positive, Ada_Type_Declaration);
      subtype Ada_Type_Declaration_List is ATDL.Vector;

      function Resolve_Dependencies return Ada_Type_Declaration_List;
      function Resolve_Dependencies return Ada_Type_Declaration_List is
         package USL is new Ada.Containers.Vectors
           (Positive, Signatures.Unbounded.Unbounded_Signature);
         subtype Unbounded_Signature_List is USL.Vector;

         Result      : Ada_Type_Declaration_List;
         Bookkeeping : Unbounded_Signature_List;

         procedure Resolve_Dependency (TD : Ada_Type_Declaration);
         procedure Resolve_Dependency (TD : Ada_Type_Declaration) is
         begin
            case TD.Kind is
               when Basic_Kind | Variant_Kind =>
                  null;
               when Array_Kind =>
                  if not Bookkeeping.Contains (TD.Array_Element_Type_Code) then
                     Resolve_Dependency (Types (TD.Array_Element_Type_Code));
                  end if;
               when Ordered_Dict_Kind | Hashed_Dict_Kind =>
                  if not Bookkeeping.Contains (TD.Dict_Key_Type_Code) then
                     Resolve_Dependency (Types (TD.Dict_Key_Type_Code));
                  end if;

                  if not Bookkeeping.Contains (TD.Dict_Element_Type_Code) then
                     Resolve_Dependency (Types (TD.Dict_Element_Type_Code));
                  end if;
               when Struct_Kind =>
                  for SM of TD.Struct_Members loop
                     if not Bookkeeping.Contains (SM.Type_Code) then
                        Resolve_Dependency (Types (SM.Type_Code));
                     end if;
                  end loop;
            end case;

            Bookkeeping.Append (TD.Type_Code);
            Result.Append (TD);
         end Resolve_Dependency;
      begin
         for TD of Types loop
            if not Bookkeeping.Contains (TD.Type_Code) then
               Resolve_Dependency (TD);
            end if;
         end loop;
         return Result;
      end Resolve_Dependencies;

      Dependencies : constant Ada_Type_Declaration_List :=
        Resolve_Dependencies;
   begin
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");
      With_Entity ("Ada.Strings.Unbounded.Hash");
      With_Entity ("Ada.Containers.Vectors");
      With_Entity ("Ada.Containers.Ordered_Maps");
      With_Entity ("Ada.Containers.Hashed_Maps");
      With_Entity ("Interfaces");
      Use_Entity ("Interfaces");
      New_Line;

      With_Entity ("GNAT.OS_Lib");
      New_Line;

      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Arguments.Containers");
      Use_Type ("D_Bus.Arguments.Containers.Variant_Type");
      With_Entity ("D_Bus.Types");
      Use_Type ("D_Bus.Types.Obj_Path");
      Use_Type ("D_Bus.Types.Signature");
      New_Line;

      Start_Package ("D_Bus.Generated_Types");
      begin
         Large_Comment ("Generated Types");
         for TD of Dependencies loop
            declare
               Name : constant String := Get_Ada_Type (+TD.Type_Code);
            begin
               case TD.Kind is
                  when Basic_Kind | Variant_Kind =>
                     null;
                  when Array_Kind =>
                     Declare_Package
                       ("Pkg_" & Name,
                        "new Ada.Containers.Vectors (Positive, " &
                        (Get_Ada_Type (+TD.Array_Element_Type_Code) & ")"));

                     Declare_Subtype (Name, "Pkg_" & Name & ".Vector");
                     Use_Type (Name);
                     New_Line;
                  when Struct_Kind =>
                     Start_Record (Name);
                     for SM of TD.Struct_Members loop
                        Declare_Entity
                          (+SM.Name, Get_Ada_Type (+SM.Type_Code));
                     end loop;
                     End_Record;
                     New_Line;
                  when Ordered_Dict_Kind =>
                     Declare_Package
                       ("Pkg_" & Name,
                        "new Ada.Containers.Ordered_Maps (" &
                        Get_Ada_Type (+TD.Dict_Key_Type_Code) & ", " &
                        Get_Ada_Type (+TD.Dict_Element_Type_Code) & ")");

                     Declare_Subtype (Name, "Pkg_" & Name & ".Map");
                     Use_Type (Name);
                     New_Line;
                  when Hashed_Dict_Kind =>
                     Declare_Package
                       ("Pkg_" & Name,
                        "new Ada.Containers.Hashed_Maps (" &
                        Get_Ada_Type (+TD.Dict_Key_Type_Code) & ", " &
                        Get_Ada_Type (+TD.Dict_Element_Type_Code) &
                        ", Ada.Strings.Unbounded.Hash, ""="")");

                     Declare_Subtype (Name, "Pkg_" & Name & ".Map");
                     Use_Type (Name);
                     New_Line;
               end case;
            end;
         end loop;
         New_Line;

         Large_Comment ("Binding");
         for TD of Dependencies loop
            Declare_Procedure (Bind_To_Ada_Signature (TD));
            Declare_Procedure (Bind_To_DBus_Signature (TD));
         end loop;
      end;
      End_Package ("D_Bus.Generated_Types");

      --  Preamble
      Use_Pragma ("Ada_2012");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Types");

      Start_Package_Body ("D_Bus.Generated_Types");
      begin
         --  Bindings
         for TD of Dependencies loop
            Start_Procedure (Bind_To_Ada_Signature (TD));
            Begin_Code;
            begin
               Bind_To_Ada
                 (Types     => Types, Type_Code => TD.Type_Code,
                  DBus_Name => "DBus_Entity", Ada_Name => "Ada_Entity");
            end;
            End_Procedure (Bind_To_Ada_Name);

            Start_Procedure (Bind_To_DBus_Signature (TD));
            Begin_Code;
            begin
               Bind_To_DBus
                 (Types    => Types, Type_Code => TD.Type_Code,
                  Ada_Name => "Ada_Entity", DBus_Name => "DBus_Entity");
            end;
            End_Procedure (Bind_To_DBus_Name);
         end loop;
      end;
      End_Package ("D_Bus.Generated_Types");
   end Print;
end Codegen.Types;
