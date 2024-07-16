with Codegen.Output; use Codegen.Output;

with Type_Checking; use Type_Checking;

with Shared; use Shared;

package body Codegen.Binding is

   ------------------
   -- Bind_To_DBus --
   ------------------
   procedure Bind_To_DBus
     (Pkg : Ada_Package_Type; TD : Ada_Type_Declaration; Ada_Name : String;
      DBus_Name : String)
   is
      procedure Bind_To_DBus_Inner
        (TD : Ada_Type_Declaration; Ada_Name : String; DBus_Name : String);
      procedure Bind_To_DBus_Inner
        (TD : Ada_Type_Declaration; Ada_Name : String; DBus_Name : String)
      is
      begin
         case TD.Kind is
            --  DBus_Name := +Ada_Name
            when Basic_Kind =>
               --!pp off
               Declare_Code;
                  Use_Entity ("D_Bus.Arguments.Basic");
                  Use_Entity ("D_Bus.Types");
               Begin_Code;
                  if Is_Stringlike (+TD.Type_Code) then
                     --  Unbounded_String -> String
                     --  -> Obj_Path -> Object_Path_Type
                     if +TD.Type_Code = Object_Path then
                        Assign
                          (DBus_Name,
                           "+(+Ada.Strings.Unbounded.To_String (" & Ada_Name &
                           "))");
                     else --  Unbounded_String -> String -> String_Type
                        Assign
                          (DBus_Name,
                           "+Ada.Strings.Unbounded.To_String (" &
                           Ada_Name & ")");
                     end if;
                  else
                     Assign
                       (DBus_Name,
                        "+" & Get_Library_Ada_Type (+TD.Type_Code) & " (" &
                        Ada_Name & ")");
                  end if;
               End_Code;
               --!pp on

               --  for C of Ada_Name => DBus_Name.Append (Bind (C))
            when Array_Kind =>
               --!pp off
               Start_Container_For_Loop ("C", Ada_Name);
                  Declare_Code;
                     Declare_Entity
                       ("Arr_Obj",
                        Get_Library_DBus_Type (+TD.Array_Element_Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD =>
                           Pkg.Type_Declarations (TD.Array_Element_Type_Code),
                        Ada_Name => "C", DBus_Name => "Arr_Obj");
                     Call (DBus_Name & ".Append (Arr_Obj)");
                  End_Code;
               End_For_Loop;
               --!pp on

               --  for_struct M of Ada_Name => D_Bus_Name.Append (Bind (M))
            when Struct_Kind =>
               --!pp off
               for SM of TD.Struct_Members loop
                  Declare_Code;
                     Declare_Entity
                       ("Struct_Obj", Get_Library_DBus_Type (+SM.Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD        => Pkg.Type_Declarations (SM.Type_Code),
                        Ada_Name  => Ada_Name & "." & (+SM.Name),
                        DBus_Name => "Struct_Obj");

                     Call (DBus_Name & ".Append (Struct_Obj)");
                  End_Code;
               end loop;
               --!pp on

               --  for <K,V> of Ada_Name =>
               --  D_Bus_Name.Append (<Bind(K),Bind(V)>)
            when Ordered_Dict_Kind | Hashed_Dict_Kind =>
               --!pp off
               Start_Map_For_Loop ("Cursor", Ada_Name);
                  Declare_Code;
                     Declare_Entity
                       ("Dict_Key",
                        Get_Library_DBus_Type (+TD.Dict_Key_Type_Code));
                     Declare_Entity
                       ("Dict_Element",
                        Get_Library_DBus_Type (+TD.Dict_Element_Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD        =>
                           Pkg.Type_Declarations (TD.Dict_Key_Type_Code),
                        Ada_Name  => "Pkg_" & (+TD.Name) & ".Key (Cursor)",
                        DBus_Name => "Dict_Key");
                     Bind_To_DBus_Inner
                       (TD =>
                           Pkg.Type_Declarations (TD.Dict_Element_Type_Code),
                        Ada_Name  => "Pkg_" & (+TD.Name) & ".Element (Cursor)",
                        DBus_Name => "Dict_Element");
                     Call
                       (DBus_Name &
                        ".Append (D_Bus.Arguments.Containers.Create" &
                        " (Dict_Key, Dict_Element))");
                  End_Code;
               End_For_Loop;
               --!pp on
            when Variant_Kind =>
               Assign (DBus_Name, Ada_Name);
         end case;
      end Bind_To_DBus_Inner;
   begin
      Bind_To_DBus_Inner (TD, Ada_Name, DBus_Name);
   end Bind_To_DBus;

   -----------------
   -- Bind_To_Ada --
   -----------------
   procedure Bind_To_Ada
     (Pkg : Ada_Package_Type; TD : Ada_Type_Declaration; DBus_Name : String;
      Ada_Name : String)
   is
      procedure Bind_To_Ada_Inner
        (TD : Ada_Type_Declaration; DBus_Name : String; Ada_Name : String);

      procedure Bind_To_Ada_Inner
        (TD : Ada_Type_Declaration; DBus_Name : String; Ada_Name : String)
      is
      begin
         case TD.Kind is
            --  Ada_Name := +DBus_Name
            when Basic_Kind =>
               --  Ada_Name := To_Unbounded_String
               --    (To_String (String_Type (D_Bus_Name)))
               if Is_Stringlike (+TD.Type_Code) then
                  Assign
                    (Ada_Name,
                     "Ada.Strings.Unbounded.To_Unbounded_String (" &
                     DBus_Name & ".To_String)");

                  --  Ada_Name := <Ada_Type> (D_Bus.Arguments.Basic.To_Ada
                  --    (<DBus_Ada_Type> (DBus_Name)))
               else
                  Assign
                    (Ada_Name,
                     (+TD.Name) & " (" & "D_Bus.Arguments.Basic.To_Ada (" &
                     Get_Library_DBus_Type (+TD.Type_Code) & "(" & DBus_Name &
                     ")))");
               end if;

               --  for C of DBus_Name => Ada_Name.Append (Bind (C))
            when Array_Kind =>
               --!pp off
               Declare_Code;
                  Renames_Entity
                    ("DBus_Array",
                     "D_Bus.Arguments.Containers.Array_Type",
                     "D_Bus.Arguments.Containers.Array_Type (" &
                      DBus_Name & ")");
               Begin_Code;
                  Start_Index_For_Loop ("I", "1", "DBus_Array.Get_Count");
                     Declare_Code;
                        --  DBus Element
                        Declare_Entity
                          ("Arr_Obj",
                           Get_Library_DBus_Type
                             (+TD.Array_Element_Type_Code));

                        --  Ada Element
                        Declare_Entity
                          ("Arr_Obj_Ada",
                           +Pkg.Type_Declarations
                             (TD.Array_Element_Type_Code).Name);
                     Begin_Code;
                        Assign
                          ("Arr_Obj",
                           Get_Library_DBus_Type
                             (+TD.Array_Element_Type_Code) & " (" &
                           "DBus_Array.Get_Element (I))");
                        Bind_To_Ada_Inner
                          (TD =>
                              Pkg.Type_Declarations
                                (TD.Array_Element_Type_Code),
                           DBus_Name => "Arr_Obj", Ada_Name => "Arr_Obj_Ada");
                        Call (Ada_Name & ".Append (Arr_Obj_Ada)");
                     End_Code;
                  End_For_Loop;
               End_Code;
               --!pp on

               --  for M of DBus_Name => Ada_Name.<Member_I> := Bind (M)
            when Struct_Kind =>
               --!pp off
               for I in 1 .. Positive (TD.Struct_Members.Length) loop
                  Declare_Code;
                     Declare_Entity
                       ("Element",
                        +Pkg.Type_Declarations
                          (TD.Struct_Members (I).Type_Code).Name);
                  Begin_Code;
                     Bind_To_Ada_Inner
                       (TD        =>
                          Pkg.Type_Declarations
                            (TD.Struct_Members (I).Type_Code),
                        DBus_Name =>
                           DBus_Name & ".Get_Element (" & I'Image & ")",
                        Ada_Name  => "Element");
                     Assign
                       (Ada_Name & "." & (+TD.Struct_Members (I).Name),
                        "Element");
                  End_Code;
               end loop;
               --!pp on

               --  for <K,V> of DBus_Name =>
               --  Ada_Name.Insert (Bind (K), Bind (V))
            when Ordered_Dict_Kind | Hashed_Dict_Kind =>
               --!pp off
               Declare_Code;
                  Renames_Entity
                    ("DBus_Dict",
                     "D_Bus.Arguments.Containers.Array_Type",
                     "D_Bus.Arguments.Containers.Array_Type (" &
                     DBus_Name & ")");
               Begin_Code;
                  Start_Index_For_Loop ("I", "1", "DBus_Dict.Get_Count");
                     Declare_Code;
                        Declare_Entity
                          ("Dict_Entry",
                           "D_Bus.Arguments.Containers.Dict_Entry_Type");
                        Declare_Entity
                          ("Dict_Key",
                           +Pkg.Type_Declarations
                             (TD.Dict_Key_Type_Code).Name);
                        Declare_Entity
                          ("Dict_Element",
                           +Pkg.Type_Declarations
                             (TD.Dict_Element_Type_Code).Name);
                     Begin_Code;
                        Assign
                          ("Dict_Entry",
                           "D_Bus.Arguments.Containers.Dict_Entry_Type (" &
                           "DBus_Dict.Get_Element (I))");
                        Bind_To_Ada_Inner
                          (Pkg.Type_Declarations (TD.Dict_Key_Type_Code),
                           "Dict_Entry.Get_Key", "Dict_Key");
                        Bind_To_Ada_Inner
                          (Pkg.Type_Declarations (TD.Dict_Element_Type_Code),
                           Get_Library_DBus_Type (+TD.Dict_Element_Type_Code) &
                           " (" & "Dict_Entry.Get_Value)",
                           "Dict_Element");
                        Call (Ada_Name & ".Insert (Dict_Key, Dict_Element)");
                     End_Code;
                  End_For_Loop;
               End_Code;
               --!pp on

            when Variant_Kind =>
               Assign
                 (Ada_Name,
                  Get_Library_DBus_Type (+TD.Type_Code) & "(" & DBus_Name &
                  ")");
         end case;
      end Bind_To_Ada_Inner;
   begin
      Bind_To_Ada_Inner (TD, DBus_Name, Ada_Name);
   end Bind_To_Ada;
end Codegen.Binding;
