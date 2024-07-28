pragma Ada_2012;

with Codegen.Output;             use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
with Codegen.Binding;            use Codegen.Binding;

with Type_Checking; use Type_Checking;

with Shared; use Shared;

package body Codegen.Server.Iface is
   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec (Pkg : Ada_Package_Type);
   procedure Print_Spec (Pkg : Ada_Package_Type) is
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      --  `Unbounded_String`
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");
      --  ^^

      With_Entity ("Interfaces");
      --  All basic types
      Use_Entity ("Interfaces");
      --  ^^

      With_Entity ("D_Bus.Arguments.Containers");
      --  `Variant_Type`
      Use_Type ("D_Bus.Arguments.Containers.Variant_Type");
      --  ^^

      With_Entity ("D_Bus.Support");
      --  `Unbounded_Object_Path`, `Unbounded_Signature`
      Use_Entity ("D_Bus.Support");
      --  ^^

      With_Entity ("D_Bus.Support.Server");
      --  `Server_Interface` and associated methods
      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  All generated types
      New_Line;

      Start_Package (+Pkg.Name);
      begin
         --  Interface Type
         --  Note: This _should_ extend `org.freedesktop.DBus.Properties` but
         --  we choose not to to reduce the size of the closure. This is
         --  most likely irrelevant as Server_Interface provides the same
         --  methods as `org.freedesktop.DBus.Properties`
         Declare_Type
           ("Child_Interface",
            "limited interface and D_Bus.Support.Server.Server_Interface");
         New_Line;

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
         end if;
         for M of Pkg.Methods loop
            Declare_Procedure (Method_Signature (M));
            New_Line;
         end loop;

         --  Signals
         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
         end if;
         for S of Pkg.Signals loop
            Declare_Procedure (Signal_Signature (S));
            New_Line;
         end loop;

         --  Properties
         --  The server can read and write any property
         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
         end if;
         for P of Pkg.Properties loop
            Declare_Function (Property_Read_Signature (P));
            Declare_Procedure (Property_Write_Signature (P));
            New_Line;
         end loop;
      end;
      End_Package (+Pkg.Name);
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   procedure Print_Body
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type)
   is
   begin
      --  Preamble
      Use_Pragma ("Ada_2012");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Arguments.Containers");
      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Extra");
      With_Entity ("D_Bus.Types");

      Start_Package_Body (+Pkg.Name);
      begin
         Declare_Entity
           ("Iface", "constant String", """" & (+Pkg.Real_Name) & """");

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
         end if;
         for M of Pkg.Methods loop
            Declare_Null_Procedure (Method_Signature (M));
         end loop;

         --  Signals
         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
         end if;
         for S of Pkg.Signals loop
            Start_Procedure (Signal_Signature (S));
            begin
               Declare_Entity ("Args", "D_Bus.Arguments.Argument_List_Type");
            end;
            Begin_Code;
            begin
               --  Bind each argument
               for A of S.Arguments loop
                  Declare_Code;
                  begin
                     Declare_Entity
                       ("Argument", Get_Library_DBus_Type (+A.Type_Code));
                  end;
                  Begin_Code;
                  begin
                     Bind_To_DBus
                       (Types    => Types, Type_Code => A.Type_Code,
                        Ada_Name => +A.Name, DBus_Name => "Argument");

                     Call ("D_Bus.Arguments.Append (Args, Argument)");
                  end;
                  End_Code;
               end loop;

               --  Send the signal
               Call ("O.Send_Signal (Iface, """ & (+S.Name) & """, Args)");
            end;
            End_Procedure (Signal_Name (S));
         end loop;

         --  Properties
         --  The server can read and write to any property
         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
         end if;
         for P of Pkg.Properties loop
            --  <PropertyName> (...) return <Type>
            Start_Function (Property_Read_Signature (P));
            begin
               Declare_Entity
                 ("Value", "D_Bus.Arguments.Containers.Variant_Type");
               Declare_Entity ("Ada_Value", Get_Ada_Type (+P.Type_Code));
            end;
            Begin_Code;
            begin
               --  Get the raw property Variant
               Call ("O.Get_Property (Iface, """ & (+P.Name) & """, Value)");

               Bind_To_Ada
                 (Types     => Types, Type_Code => P.Type_Code,
                  DBus_Name =>
                    Get_Library_DBus_Type (+P.Type_Code) &
                    " (Value.Get_Argument)",
                  Ada_Name  => "Ada_Value");

               Return_Entity ("Ada_Value");
            end;
            End_Function (Property_Read_Name (P));

            --  Set_<PropertyName> (...Value : <Type>)
            Start_Procedure (Property_Write_Signature (P));
            begin
               Declare_Entity
                 ("DBus_Value",
                  Get_Library_DBus_Type (+P.Type_Code));
            end;
            Begin_Code;
            begin
               Bind_To_DBus
                 (Types    => Types, Type_Code => P.Type_Code,
                  Ada_Name => "Value", DBus_Name => "DBus_Value");

               --  Assign the property after creating a Variant
               Call
                 ("O.Set_Property (Iface, """ & (+P.Name) &
                  """, D_Bus.Arguments.Containers.Create (DBus_Value))");
            end;
            End_Procedure (Property_Write_Name (P));
         end loop;
      end;
      End_Package (+Pkg.Name);
   end Print_Body;

   -----------
   -- Print --
   -----------
   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type)
   is
   begin
      Print_Spec (Pkg);
      Print_Body (Types, Pkg);
   end Print;
end Codegen.Server.Iface;
