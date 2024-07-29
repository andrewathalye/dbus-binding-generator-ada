pragma Ada_2012;

with Codegen.Output; use Codegen.Output;

with Type_Checking; use Type_Checking;
with Shared;        use Shared;

package body Codegen.Binding is

   ------------------
   -- Bind_To_DBus --
   ------------------
   procedure Bind_To_DBus
     (Types     : Codegen.Types.Ada_Type_Declaration_Map;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String; Ada_Name : String;
      DBus_Name : String)
   is
      procedure Bind_To_DBus_Inner
        (TD : Ada_Type_Declaration; Ada_Name : String; DBus_Name : String);
      procedure Bind_To_DBus_Inner
        (TD : Ada_Type_Declaration; Ada_Name : String; DBus_Name : String)
      is
         Name : constant String := Get_Ada_Type (+TD.Type_Code);
      begin
         case TD.Kind is
            --  DBus_Name := +Ada_Name
            when Basic_Kind =>
               --!pp off
               Declare_Code;
                  Use_Entity ("D_Bus.Arguments.Basic");
                  Use_Entity ("D_Bus.Types");
                  Use_Entity ("D_Bus.Extra");
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
                           "+" & Get_Library_Ada_Type (+TD.Type_Code) & " (" &
                           "Ada.Strings.Unbounded.To_String (" & Ada_Name &
                           "))");
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
                       ("Arr_Obj_" & Name,
                        Get_Library_DBus_Type (+TD.Array_Element_Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD =>
                           Types (TD.Array_Element_Type_Code),
                        Ada_Name => "C",
                        DBus_Name => "Arr_Obj_" & Name);
                     Call (DBus_Name & ".Append (Arr_Obj_" & Name & ")");
                  End_Code;
               End_For_Loop;

               --  Handle the case of an empty array
               Start_If (DBus_Name & ".Is_Empty");
                  Call
                    (DBus_Name & ".Set_Signature (""" &
                     (+TD.Array_Element_Type_Code) & """)");
               End_If;
               --!pp on

               --  for_struct M of Ada_Name => D_Bus_Name.Append (Bind (M))
            when Struct_Kind =>
               --!pp off
               for SM of TD.Struct_Members loop
                  Declare_Code;
                     Declare_Entity
                       ("Struct_Obj_" & Name,
                        Get_Library_DBus_Type (+SM.Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD        => Types (SM.Type_Code),
                        Ada_Name  => Ada_Name & "." & (+SM.Name),
                        DBus_Name => "Struct_Obj_" & Name);

                     Call (DBus_Name & ".Append (Struct_Obj_" & Name & ")");
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
                       ("Dict_Element_" & Name,
                        Get_Library_DBus_Type (+TD.Dict_Element_Type_Code));
                  Begin_Code;
                     Bind_To_DBus_Inner
                       (TD        =>
                           Types (TD.Dict_Key_Type_Code),
                        Ada_Name  => "Pkg_" & Name & ".Key (Cursor)",
                        DBus_Name => "Dict_Key");
                     Bind_To_DBus_Inner
                       (TD =>
                           Types (TD.Dict_Element_Type_Code),
                        Ada_Name  => "Pkg_" & Name & ".Element (Cursor)",
                        DBus_Name => "Dict_Element_" & Name);
                     Call
                       (DBus_Name &
                        ".Append (D_Bus.Arguments.Containers.Create" &
                        " (Dict_Key, Dict_Element_" & Name & "))");
                  End_Code;
               End_For_Loop;

               --  Handle the case of an empty dict
               Start_If (DBus_Name & ".Is_Empty");
                  Call
                    (DBus_Name & ".Set_Signature (""{" &
                     (+TD.Dict_Key_Type_Code) &
                     (+TD.Dict_Element_Type_Code) & "}"")");
               End_If;

               --!pp on
            when Variant_Kind =>
               Assign (DBus_Name, Ada_Name);
         end case;
      end Bind_To_DBus_Inner;
   begin
      Bind_To_DBus_Inner (Types (Type_Code), Ada_Name, DBus_Name);
   end Bind_To_DBus;

   -----------------
   -- Bind_To_Ada --
   -----------------
   procedure Bind_To_Ada
     (Types     : Codegen.Types.Ada_Type_Declaration_Map;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String; DBus_Name : String;
      Ada_Name  : String)
   is
      procedure Bind_To_Ada_Inner
        (TD : Ada_Type_Declaration; DBus_Name : String; Ada_Name : String);

      procedure Bind_To_Ada_Inner
        (TD : Ada_Type_Declaration; DBus_Name : String; Ada_Name : String)
      is
         Name : constant String := Get_Ada_Type (+TD.Type_Code);
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

                  --  Ada_Name :=
                  --     <Ada_Type> (<DBus_Ada_Type> (DBus_Name).To_Ada)
               else
                  Assign
                    (Ada_Name,
                     Name & " (" & Get_Library_DBus_Type (+TD.Type_Code) &
                     " (" & DBus_Name & ").To_Ada)");
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
                          ("Arr_Obj_Ada_" & Name,
                           Get_Ada_Type (+TD.Array_Element_Type_Code));
                     Begin_Code;
                        Assign
                          ("Arr_Obj",
                           Get_Library_DBus_Type
                             (+TD.Array_Element_Type_Code) & " (" &
                           "DBus_Array.Get_Element (I))");
                        Bind_To_Ada_Inner
                          (TD =>
                              Types
                                (TD.Array_Element_Type_Code),
                           DBus_Name => "Arr_Obj",
                           Ada_Name => "Arr_Obj_Ada_" & Name);
                        Call
                          (Ada_Name &
                           ".Append (Arr_Obj_Ada_" &
                           Name & ")");
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
                       ("Element_" & Name,
                        Get_Ada_Type (+TD.Struct_Members (I).Type_Code));
                  Begin_Code;
                     Bind_To_Ada_Inner
                       (TD        =>
                          Types
                            (TD.Struct_Members (I).Type_Code),
                        DBus_Name =>
                           "D_Bus.Arguments.Containers.Struct_Type (" &
                           DBus_Name & ")" & ".Get_Element (" & I'Image & ")",
                        Ada_Name  => "Element_" & Name);
                     Assign
                       (Ada_Name & "." & (+TD.Struct_Members (I).Name),
                        "Element_" & Name);
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
                           Get_Ada_Type (+TD.Dict_Key_Type_Code));
                        Declare_Entity
                          ("Dict_Element_" & Name,
                           Get_Ada_Type (+TD.Dict_Element_Type_Code));
                     Begin_Code;
                        Assign
                          ("Dict_Entry",
                           "D_Bus.Arguments.Containers.Dict_Entry_Type (" &
                           "DBus_Dict.Get_Element (I))");
                        Bind_To_Ada_Inner
                          (Types (TD.Dict_Key_Type_Code),
                           "Dict_Entry.Get_Key", "Dict_Key");
                        Bind_To_Ada_Inner
                          (Types (TD.Dict_Element_Type_Code),
                           Get_Library_DBus_Type (+TD.Dict_Element_Type_Code) &
                           " (" & "Dict_Entry.Get_Value)",
                           "Dict_Element_" & Name);
                        Call
                          (Ada_Name &
                           ".Insert (Dict_Key, Dict_Element_" &
                           Name & ")");
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
      Bind_To_Ada_Inner (Types (Type_Code), DBus_Name, Ada_Name);
   end Bind_To_Ada;
end Codegen.Binding;
