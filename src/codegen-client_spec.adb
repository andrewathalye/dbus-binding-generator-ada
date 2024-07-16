with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

package body Codegen.Client_Spec is
   --  Produce an acceptable order for dependency resolution
   --  'Elaborate' the type declaration order
   package ATDL is new Ada.Containers.Vectors (Positive, Ada_Type_Declaration);
   subtype Ada_Type_Declaration_List is ATDL.Vector;

   function Resolve_Dependencies
     (TDM : Ada_Type_Declaration_Map) return Ada_Type_Declaration_List;
   function Resolve_Dependencies
     (TDM : Ada_Type_Declaration_Map) return Ada_Type_Declaration_List
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
                  Resolve_Dependency (TDM (TD.Array_Element_Type_Code));
               end if;
            when Ordered_Dict_Kind | Hashed_Dict_Kind =>
               if not Bookkeeping.Contains (TD.Dict_Key_Type_Code) then
                  Resolve_Dependency (TDM (TD.Dict_Key_Type_Code));
               end if;

               if not Bookkeeping.Contains (TD.Dict_Element_Type_Code) then
                  Resolve_Dependency (TDM (TD.Dict_Element_Type_Code));
               end if;
            when Struct_Kind =>
               for SM of TD.Struct_Members loop
                  if not Bookkeeping.Contains (SM.Type_Code) then
                     Resolve_Dependency (TDM (SM.Type_Code));
                  end if;
               end loop;
         end case;

         Bookkeeping.Append (TD.Type_Code);
         Result.Append (TD);
      end Resolve_Dependency;
   begin
      for TD of TDM loop
         if not Bookkeeping.Contains (TD.Type_Code) then
            Resolve_Dependency (TD);
         end if;
      end loop;
      return Result;
   end Resolve_Dependencies;

   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type) is
   begin
      --  Preamble
      Use_Pragma ("Ada_2012");
      With_Entity ("Ada.Strings.Unbounded");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");
      With_Entity ("Ada.Strings.Unbounded.Hash");
      With_Entity ("Ada.Containers.Vectors");
      With_Entity ("Ada.Containers.Ordered_Maps");
      With_Entity ("Ada.Containers.Hashed_Maps");
      With_Entity ("Interfaces");
      Use_Entity ("Interfaces");
      With_Entity ("GNAT.OS_Lib");
      With_Entity ("D_Bus.Arguments.Basic");
      Use_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Arguments.Containers");
      Use_Entity ("D_Bus.Arguments.Containers");
      New_Line;

      --  Package Spec
      --!pp off
      Start_Package (+Pkg.Name);
         Large_Comment ("Builtin D_Bus Types");
         Declare_Subtype
           ("Object_Path", "Ada.Strings.Unbounded.Unbounded_String");
         Declare_Subtype
           ("Signature_Type", "Ada.Strings.Unbounded.Unbounded_String");
         New_Line;

         Large_Comment ("Generated D_Bus Types");
         for TD of Resolve_Dependencies (Pkg.Type_Declarations) loop
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
                  New_Line;
               when Struct_Kind =>
                  Start_Record (+TD.Name);
                  for SM of TD.Struct_Members loop
                     Declare_Entity
                       (+SM.Name,
                        (+Pkg.Type_Declarations (SM.Type_Code).Name));
                  end loop;
                  End_Record;
                  New_Line;
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
                  New_Line;
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
                  New_Line;

            end case;
         end loop;

         --  Print subprogram specs
         Large_Comment ("Builtin");
         Declare_Entity ("No_Destination", "exception");
         Declare_Procedure ("Set_Destination (Dest : String)");
         Comment ("Connect and prepare to send messages to `Dest`");
         Comment ("This must be called before any other method.");
         New_Line;

         Large_Comment ("DBus Methods");
         for SP of Pkg.Methods loop
            Declare_Procedure (Function_Signature (SP));
            New_Line;
         end loop;

         --  TODO is this correct handling for signals?
         Large_Comment ("DBus Signals");
         for S of Pkg.Signals loop
            Declare_Procedure (Function_Signature (S));
            New_Line;
         end loop;

         Large_Comment ("DBus Properties");
         for P of Pkg.Properties loop
            declare
               Ada_Type : constant String := +Pkg.Type_Declarations
                 (P.Type_Code).Name;
            begin
               case P.PAccess is
                  when Parsing.Read =>
                     Declare_Function ("Get_" & (+P.Name), Ada_Type);
                  when Parsing.Write =>
                     Declare_Procedure
                       ("Set_" & (+P.Name) & " (Value : " & Ada_Type & ")");
                  when Parsing.Readwrite =>
                     Declare_Function ("Get_" & (+P.Name), Ada_Type);
                     Declare_Procedure
                       ("Set_" & (+P.Name) & " (Value : " & Ada_Type & ")");
               end case;
            end;
         end loop;
      End_Package (+Pkg.Name);
      --!pp on
   end Print;
end Codegen.Client_Spec;
