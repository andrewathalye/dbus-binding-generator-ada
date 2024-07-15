with Type_Checking; use Type_Checking;

with Shared; use Shared;

with Codegen.Output; use Codegen.Output;

package body Codegen.The_Body is
   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type) is
      --  Produce a value `DBus_Name` by binding `Ada_Name`
      --  This doesn’t declare either name (that is for the
      --  caller to do)
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

      --  Bind Ada Argument to DBus
      procedure Bind_To_DBus (A : Ada_Argument_Type);
      procedure Bind_To_DBus (A : Ada_Argument_Type) is
         TD : constant Ada_Type_Declaration :=
           Pkg.Type_Declarations (A.Type_Code);
      begin
         case TD.Kind is
            when Basic_Kind | Variant_Kind =>
               --!pp off
               Declare_Code;
                  Use_Entity ("D_Bus.Arguments.Basic");
                  Declare_Entity
                    ("Root_Obj", Get_Library_DBus_Type (+A.Type_Code));
               Begin_Code;
                  Bind_To_DBus_Inner (TD, +A.Name, "Root_Obj");
                  Call ("Request_Args.Append (Root_Obj)");
               End_Code;
               --!pp on
            when Array_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_List", "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Bind_To_DBus_Inner (TD, +A.Name, "Root_List");
                  Call ("Request_Args.Append (Root_List)");
               End_Code;
               --!pp on
            when Struct_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_Struct", "D_Bus.Arguments.Containers.Struct_Type");
               Begin_Code;
                  Bind_To_DBus_Inner (TD, +A.Name, "Root_Struct");
                  Call ("Request_Args.Append (Root_Struct)");
               End_Code;
               --!pp on
            when Ordered_Dict_Kind | Hashed_Dict_Kind =>
               --!pp off
               Declare_Code;
                  Declare_Entity
                    ("Root_Dict", "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Bind_To_DBus_Inner (TD, +A.Name, "Root_Dict");
                  Call ("Request_Args.Append (Root_Dict)");
               End_Code;
               --!pp on
         end case;
      end Bind_To_DBus;

      --  Binds a DBus Argument to Ada but provides no support code
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
                     Get_Library_DBus_Type (+TD.Type_Code) & " (" & DBus_Name &
                     ")))");
               end if;

               --  for C of DBus_Name => Ada_Name.Append (Bind (C))
            when Array_Kind =>
               --!pp off
               Start_Index_For_Loop ("I", "1", DBus_Name & ".Get_Count");
                  Declare_Code;
                     --  DBus Element
                     Declare_Entity
                       ("Arr_Obj",
                        Get_Library_DBus_Type (+TD.Array_Element_Type_Code));

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
                        DBus_Name & ".Get_Element (I))");
                     Bind_To_Ada_Inner
                       (TD =>
                           Pkg.Type_Declarations (TD.Array_Element_Type_Code),
                        DBus_Name => "Arr_Obj", Ada_Name => "Arr_Obj_Ada");
                     Call (Ada_Name & ".Append (Arr_Obj_Ada)");
                  End_Code;
               End_For_Loop;
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
               Start_Index_For_Loop ("I", "1", DBus_Name & ".Get_Count");
                  Declare_Code;
                     Declare_Entity
                       ("Dict_Entry",
                        "D_Bus.Arguments.Containers.Dict_Entry_Type");
                     Declare_Entity
                       ("Dict_Key",
                        +Pkg.Type_Declarations (TD.Dict_Key_Type_Code).Name);
                     Declare_Entity
                       ("Dict_Element",
                        +Pkg.Type_Declarations
                          (TD.Dict_Element_Type_Code).Name);
                  Begin_Code;
                     Assign
                       ("Dict_Entry",
                        "D_Bus.Arguments.Containers.Dict_Entry_Type ("
                        & DBus_Name & ".Get_Element (I))");
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
               --!pp on

            when Variant_Kind =>
               Assign
                 (Ada_Name,
                  Get_Library_DBus_Type (+TD.Type_Code) & "(" & DBus_Name &
                  ")");
         end case;
      end Bind_To_Ada_Inner;

      --  Bind DBus Argument to Ada
      procedure Bind_To_Ada (A : Ada_Argument_Type; Arg_Index : Positive);
      procedure Bind_To_Ada (A : Ada_Argument_Type; Arg_Index : Positive) is
      begin
         Bind_To_Ada_Inner
           (Pkg.Type_Declarations (A.Type_Code),
            DBus_Name =>
              Get_Library_DBus_Type (+A.Type_Code) &
              " (Reply_Args.Get_Element (" & Arg_Index'Image & "))",
            Ada_Name  => +A.Name);
      end Bind_To_Ada;
   begin
      --!pp off
      --  Preamble
      With_Entity ("D_Bus.Connection");
      With_Entity ("D_Bus.Types");
      Use_Type ("D_Bus.Types.Obj_Path");
      With_Entity ("D_Bus.Arguments.Basic");
      New_Line;

      --  Package
      Start_Package_Body (+Pkg.Name);
         --  Globals
         Declare_Entity ("Connection", "D_Bus.Connection.Connection_Type");
         Declare_Entity ("Connected", "Boolean", "False");
         Declare_Entity
           ("Destination", "Ada.Strings.Unbounded.Unbounded_String");
         Declare_Entity
           ("Path", "constant D_Bus.Types.Obj_Path",
            "+" & ASCII.Quotation & (+Pkg.Node) & ASCII.Quotation);
         Declare_Entity
           ("Iface", "constant String",
            ASCII.Quotation & (+Pkg.Iface) & ASCII.Quotation);
         New_Line;

         --  Predefined
         Start_Procedure ("Connect (Dest : String)");
         Begin_Code;
            Start_If ("not Connected");
               Assign ("Connection", "D_Bus.Connection.Connect");
               Assign
                 ("Destination",
                  "Ada.Strings.Unbounded.To_Unbounded_String (Dest)");
               Assign ("Connected", "True");
            Start_Else;
               Raise_Exception ("Already_Connected");
            End_If;
         End_Procedure ("Connect");

         --  Subprograms
         for SP of Pkg.Subprograms loop
            New_Line;
            Start_Procedure (Function_Signature (SP));
               Declare_Entity
                 ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
            Begin_Code;
               Start_If ("not Connected");
                  Raise_Exception ("Not_Connected", (+SP.Name));
               End_If;
               New_Line;

               --  Bind each in argument
               for A of SP.Arguments loop
                  if +A.Direction = "in" then
                     Bind_To_DBus (A);
                     New_Line;
                  end if;
               end loop;

               --  The method call itself
               Assign
                 (Entity     => "Reply_Args",
                  Expression =>
                    "D_Bus.Connection.Call_Blocking " &
                    "(Connection, Ada.Strings.Unbounded.To_String " &
                    "(Destination), Path, Iface, "
                    & ASCII.Quotation & (+SP.Name) & ASCII.Quotation &
                    ", D_Bus.Connection.Default_Timeout, Request_Args)");

               --  Bind each out argument
               declare
                  Index : Positive := 1;
               begin
                  for A of SP.Arguments loop
                     if +A.Direction = "out" then
                        New_Line;
                        Bind_To_Ada (A, Index);

                        Index := Index + 1;
                     end if;
                  end loop;
               end;
            End_Procedure (+SP.Name);
         end loop;
      End_Package (+Pkg.Name);
      --!pp on
   end Print;
end Codegen.The_Body;
