with Type_Checking; use Type_Checking;

with Shared; use Shared;

package body Codegen.The_Body is
   --  Bind DBus Argument to Ada
   procedure Bind_To_Ada (A : Ada_Argument_Type) is null;

   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type; File : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;

      --  Codegen tools
      procedure Start_For_Loop (Component : String; List : String);
      procedure Start_For_Loop (Component : String; List : String)
      is
      begin
         Put_Line (File, "for " & Component & " of " & List & " loop");
      end Start_For_Loop;

      procedure Start_Map_For_Loop (Cursor : String; Map : String);
      procedure Start_Map_For_Loop (Cursor : String; Map : String)
      is
      begin
         Put_Line (File, "for " & Cursor & " in " & Map & ".Iterate loop");
      end Start_Map_For_Loop;

      procedure End_For_Loop;
      procedure End_For_Loop
      is
      begin
         Put_Line (File, "end loop;");
      end End_For_Loop;

      procedure Declare_Code;
      procedure Declare_Code
      is
      begin
         Put_Line (File, "declare");
      end Declare_Code;

      procedure Begin_Code;
      procedure Begin_Code
      is
      begin
         Put_Line (File, "begin");
      end Begin_Code;

      procedure End_Code;
      procedure End_Code
      is
      begin
         Put_Line (File, "end;");
      end End_Code;

      procedure Use_Entity (Entity : String);
      procedure Use_Entity (Entity : String)
      is
      begin
         Put_Line (File, "use " & Entity & ";");
      end Use_Entity;

      procedure Declare_Entity
        (Entity : String; EType : String; Value : String := "");
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

      procedure Assign (Entity : String; Expression : String);
      procedure Assign (Entity : String; Expression : String)
      is
      begin
         Put_Line (Entity & " := " & Expression & ";");
      end Assign;

      procedure Call (Expression : String);
      procedure Call (Expression : String)
      is
      begin
         Put_Line (File, Expression & ";");
      end Call;

      procedure Start_Procedure (Signature : String);
      procedure Start_Procedure (Signature : String)
      is
      begin
         Put_Line (File, "procedure " & Signature & ";");
         Put_Line (File, "procedure " & Signature & " is");
      end Start_Procedure;

      procedure End_Procedure (Name : String);
      procedure End_Procedure (Name : String)
      is
      begin
         Put_Line (File, "end " & Name & ";");
      end End_Procedure;

      procedure Start_Package_Body (Name : String);
      procedure Start_Package_Body (Name : String)
      is
      begin
         Put_Line (File, "package body " & Name & " is");
      end Start_Package_Body;

      procedure End_Package_Body (Name : String);
      procedure End_Package_Body (Name : String)
      is
      begin
         Put_Line (File, "end " & Name & ";");
      end End_Package_Body;

      --  Produce a value `DBus_Name` by binding `Ada_Name`
      --  This doesn’t declare either name (that is for the
      --  caller to do)
      procedure Bind_Inner
        (TD : Ada_Type_Declaration;
         Ada_Name : String;
         DBus_Name : String);
      procedure Bind_Inner
        (TD : Ada_Type_Declaration;
         Ada_Name : String;
         DBus_Name : String)
      is
      begin
         case TD.Kind is
            --  !pp off
            when Builtin_Kind =>
               Assign (DBus_Name, "+" & Ada_Name);

            --  Bind Ada_Name[] => DBus_Name[]
            --  for C of Ada_Name => DBus_Name.Append (Bind (C))
            when Array_Kind =>
               Start_For_Loop ("C", Ada_Name);
                  Declare_Code;
                     Declare_Entity
                       ("Obj",
                        Get_DBus_Ada_Type (+TD.Array_Element_Type_Code));
                  Begin_Code;
                     Bind_Inner
                       (TD        =>
                          Pkg.Type_Declarations
                            (TD.Array_Element_Type_Code),
                        Ada_Name  => "C",
                        DBus_Name => "Obj");
                     Call (DBus_Name & ".Append (Obj)");
                  End_Code;
               End_For_Loop;

            --  Bind Ada_Name{} => DBus_Name[]
            --  Meta_For SM of Ada_Name => D_Bus_Name.Append (Bind (Sm))
            when Struct_Kind =>
               for SM of TD.Struct_Members loop
                  Declare_Code;
                     Declare_Entity
                       ("Obj",
                        Get_DBus_Ada_Type (+SM.Type_Code));
                  Begin_Code;
                     Bind_Inner
                       (TD =>
                           Pkg.Type_Declarations (SM.Type_Code),
                        Ada_Name => Ada_Name & "." & (+SM.Name),
                        DBus_Name => "Obj");

                     Call (DBus_Name & ".Append (Obj)");
                  End_Code;
               end loop;
            when Dict_Kind =>
               Start_Map_For_Loop ("Cursor", Ada_Name);
                  Declare_Code;
                     Declare_Entity
                       ("Dict_Key",
                        Get_DBus_Ada_Type (+TD.Dict_Key_Type_Code));
                     Declare_Entity
                       ("Dict_Element",
                        Get_DBus_Ada_Type (+TD.Dict_Element_Type_Code));
                  Begin_Code;
                     Bind_Inner
                       (TD =>
                           Pkg.Type_Declarations (TD.Dict_Key_Type_Code),
                        Ada_Name => "Cursor.Key",
                        DBus_Name => "Dict_Key");
                     Bind_Inner
                       (TD =>
                           Pkg.Type_Declarations (TD.Dict_Element_Type_Code),
                        Ada_Name => "Cursor.Element",
                        DBus_Name => "Dict_Element");
                     Call
                       (DBus_Name &
                        ".Append (D_Bus.Arguments.Containers.Create" &
                        " (Dict_Key, Dict_Element))");
                  End_Code;
               End_For_Loop;
            --  !pp on
         end case;
      end Bind_Inner;

      --  Bind Ada Argument to DBus
      procedure Bind_To_DBus (A : Ada_Argument_Type);
      procedure Bind_To_DBus (A : Ada_Argument_Type)
      is
         TD : constant Ada_Type_Declaration :=
            Pkg.Type_Declarations (A.Type_Code);
      begin
         case TD.Kind is
            --  !pp off
            when Builtin_Kind =>
               Declare_Code;
                  Use_Entity ("D_Bus.Arguments.Basic");
                  Declare_Entity
                    ("Obj", Get_DBus_Ada_Type (+A.Type_Code));
               Begin_Code;
                  Bind_Inner (TD, +A.Name, "Obj");
                  Call ("Request_Args.Append (Obj)");
               End_Code;
            when Array_Kind =>
               Declare_Code;
                  Declare_Entity
                    ("List",
                     "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Bind_Inner (TD, +A.Name, "List");
                  Call ("Request_Args.Append (List)");
               End_Code;
            when Struct_Kind =>
               Declare_Code;
                  Declare_Entity
                    ("Struct", "D_Bus.Arguments.Containers.Struct_Type");
               Begin_Code;
                  Bind_Inner (TD, +A.Name, "Struct");
                  Call ("Request_Args.Append (Struct)");
               End_Code;
            when Dict_Kind =>
               Declare_Code;
                  Declare_Entity
                    ("Dict", "D_Bus.Arguments.Containers.Array_Type");
               Begin_Code;
                  Bind_Inner (TD, +A.Name, "Dict");
                  Call ("Request_Args.Append (Dict)");
               End_Code;
            --  !pp on
         end case;
      end Bind_To_DBus;
   begin
      --  Preamble
      Put_Line (File, "with D_Bus.Connection;");
      Put_Line (File, "with D_Bus.Arguments;");
      New_Line (File);

      --  Package
      --  !pp off
      Start_Package_Body (+Pkg.Name);
         --  Globals
         Declare_Entity ("Connection", "D_Bus.Connection.Connection_Type");
         Declare_Entity
           ("Destination",
            "constant String",
            ASCII.Quotation & ASCII.Quotation);
         Declare_Entity
           ("Path",
            "constant String",
            ASCII.Quotation & (+Pkg.Node) & ASCII.Quotation);
         Declare_Entity
           ("Iface",
            "constant String",
            ASCII.Quotation & (+Pkg.Iface) & ASCII.Quotation);
         New_Line (File);

         --  Private
         Start_Procedure ("Connect");
         Begin_Code;
            Assign ("Connection", "D_Bus.Connection.Connect");
         End_Procedure ("Connect");
         New_Line (File);

         --  Subprograms
         for SP of Pkg.Subprograms loop
            Print_Signature (SP, File);
            Put_Line (File, "is");
               Declare_Entity
                 ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
            Begin_Code;
               --  Bind each in argument
               for A of SP.Arguments loop
                  if +A.Direction = "in" then
                     Bind_To_DBus (A);
                     New_Line (File);
                  end if;
               end loop;

               --  The method call itself
               Assign (
                  Entity => "Reply_Args",
                  Expression => "D_Bus.Connection.Call_Blocking " &
                  "(Connection, Destination, Path, Iface, " &
                  ASCII.Quotation & (+SP.Name) & ASCII.Quotation &
                  ", D_Bus.Connection.Default_Timeout, Request_Args)");
               New_Line (File);

               --  Bind each out argument
               for A of SP.Arguments loop
                  if +A.Direction = "out" then
                     Bind_To_Ada (A);
                     New_Line (File);
                  end if;
               end loop;
            End_Procedure (+SP.Name);
            New_Line (File);
         end loop;
      End_Package_Body (+Pkg.Name);
      --  !pp on
   end Print;
end Codegen.The_Body;
