pragma Ada_2012;

with Codegen.Output;             use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;

with Signatures.Unbounded; use Signatures.Unbounded;
with Signatures;           use Signatures;
with Parsing;

with Shared; use Shared;

package body Codegen.Server.Iface is
   ----------------------
   -- Print_Properties --
   ----------------------
   procedure Print_Properties (Pkg : Ada_Package_Type);
   procedure Print_Properties (Pkg : Ada_Package_Type) is
      Set_Signature : constant String :=
        "Set (O : in out Child_Interface'Class;" &
        " Iface : Ada.Strings.Unbounded.Unbounded_String;" &
        " Name : Ada.Strings.Unbounded.Unbounded_String;" &
        " Value : D_Bus.Arguments.Containers.Variant_Type)";

      Get_Signature : constant String :=
        "Get (O : in out Child_Interface'Class;" &
        " Iface : Ada.Strings.Unbounded.Unbounded_String;" &
        " Name : Ada.Strings.Unbounded.Unbounded_String;" &
        " Value : out D_Bus.Arguments.Containers.Variant_Type)";

      GetAll_Signature : constant String :=
        "GetAll (O : in out Child_Interface'Class;" &
        " Iface : Ada.Strings.Unbounded.Unbounded_String;" &
        " Properties : out Dict_sv)";
   begin

      --  Spec
      Use_Pragma ("Ada_2005");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      --  `Unbounded_String`
      With_Entity ("D_Bus.Arguments.Containers");
      --  `Variant_Type`
      With_Entity ("D_Bus.Support.Server");
      --  `Server_Interface`
      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  All types
      New_Line;

      Start_Package (+Pkg.Name);
      begin
         Declare_Type
           ("Child_Interface",
            "limited interface and D_Bus.Support.Server.Server_Interface");
         New_Line;

         Large_Comment ("Methods");
         Declare_Procedure (Set_Signature);
         Declare_Procedure (Get_Signature);
         Declare_Procedure (GetAll_Signature);

         --  Note: we don’t support manually emitting
         --  signals, including PropertiesChanged
      end;
      End_Package (+Pkg.Name);

      --  Body
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Style_Checks (Off)");

      With_Entity ("Ada.Strings.Unbounded");
      Use_Entity ("Ada.Strings.Unbounded");

      Start_Package_Body (+Pkg.Name);
      begin
         Start_Procedure (Set_Signature);
         Begin_Code;
         begin
            Call
              ("O.Set_Property (To_String (Iface)," &
               " To_String (Name), Value)");
         end;
         End_Procedure ("Set");

         Start_Procedure (Get_Signature);
         Begin_Code;
         begin
            Call
              ("O.Get_Property (To_String (Iface)," &
               " To_String (Name), Value)");
         end;
         End_Procedure ("Get");

         Start_Procedure (GetAll_Signature);
         begin
            Declare_Entity
              ("DBus_Dict", "D_Bus.Arguments.Containers.Array_Type");
         end;
         Begin_Code;
         begin
            Call ("O.Get_All_Properties (To_String (Iface)," & " DBus_Dict)");

            Call ("Bind_To_Ada (DBus_Dict, Properties)");
         end;
         End_Procedure ("GetAll");
      end;
      End_Package (+Pkg.Name);
   end Print_Properties;

   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec (Pkg : Ada_Package_Type);
   procedure Print_Spec (Pkg : Ada_Package_Type) is
      use type Parsing.DBus_Direction;
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      New_Line;

      With_Entity ("Ada.Strings.Unbounded");
      --  `Unbounded_String`

      With_Entity ("Interfaces");
      --  All basic types

      With_Entity ("D_Bus.Arguments.Containers");
      --  `Variant_Type`
      Use_Type ("D_Bus.Arguments.Containers.Variant_Type");
      --  ^^

      With_Entity ("D_Bus.Types");
      --  `Obj_Path`, `Signature`

      With_Entity ("D_Bus.Support.Server");
      --  `Server_Interface` and associated methods
      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  All generated types
      New_Line;

      if not Pkg.Properties.Is_Empty then
         With_Entity ("org_freedesktop_DBus_Properties");
      end if;

      Start_Package (+Pkg.Name);
      begin
         --  Interface Type
         if not Pkg.Properties.Is_Empty then
            Declare_Type
              ("Child_Interface",
               "limited interface and" &
               " org_freedesktop_DBus_Properties.Child_Interface");
         else
            Declare_Type
              ("Child_Interface",
               "limited interface and D_Bus.Support.Server.Server_Interface");
         end if;
         New_Line;

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
         end if;

         --  If `M` contains any out parameters,
         --  it is declared `abstract`, since not returning a value
         --  is erroneous. It is otherwise declared `null`.
         for M of Pkg.Methods loop
            declare
               Contains_Out_Parameters : Boolean := False;
            begin
               for A of M.Arguments loop
                  if A.Direction = Parsing.DOut then
                     Contains_Out_Parameters := True;
                  end if;
               end loop;

               if Contains_Out_Parameters then
                  Declare_Abstract_Procedure (Unbound_Method_Signature (M));
               else
                  Declare_Null_Procedure (Unbound_Method_Signature (M));
               end if;
            end;

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
   procedure Print_Body (Pkg : Ada_Package_Type);
   procedure Print_Body (Pkg : Ada_Package_Type) is
   begin
      --  The interface requires no body if it has no properties
      --  or signals
      if Pkg.Properties.Is_Empty and Pkg.Signals.Is_Empty then
         return;
      end if;

      --  Preamble
      Use_Pragma ("Ada_2012");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Arguments.Containers");
      With_Entity ("D_Bus.Arguments.Basic");

      Start_Package_Body (+Pkg.Name);
      begin
         Declare_Entity
           ("Iface", "constant String", """" & (+Pkg.Real_Name) & """");

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
                     Call ("Bind_To_DBus (" & (+A.Name) & ", Argument)");
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
               Use_Entity ("Ada.Strings.Unbounded");

               Declare_Entity
                 ("Value", "D_Bus.Arguments.Containers.Variant_Type");
               Declare_Entity ("Ada_Value", Get_Ada_Type (+P.Type_Code));
            end;
            Begin_Code;
            begin
               --  Get the property via a more efficient method
               --  This also bypasses access checks
               Call
                 ("O.Get_Property (Iface, """ & (+P.Name) &
                  """, Value, Internal => True)");

               Call
                 ("Bind_To_Ada (" & Get_Library_DBus_Type (+P.Type_Code) &
                  " (Value.Get_Argument), Ada_Value)");

               Return_Entity ("Ada_Value");
            end;
            End_Function (Property_Read_Name (P));

            --  Set_<PropertyName> (...Value : <Type>)
            Start_Procedure (Property_Write_Signature (P));
            begin
               Use_Entity ("Ada.Strings.Unbounded");
               Use_All_Type ("D_Bus.Support.Server.Access_Type");

               Declare_Entity
                 ("DBus_Value", Get_Library_DBus_Type (+P.Type_Code));
            end;
            Begin_Code;
            begin
               Call ("Bind_To_DBus (Value, DBus_Value)");

               --  Assign the property via a more efficient method
               --  This also bypasses access checks
               --!pp off
               Call
                 ("O.Set_Property (Iface, """ & (+P.Name) &
                  """, D_Bus.Arguments.Containers.Create (DBus_Value)," &
                  " PAccess => " &
                     (case P.PAccess is
                        when Parsing.Read => "Read",
                        when Parsing.Write => "Write",
                        when Parsing.Readwrite => "Readwrite") & ")");
               --!pp on
            end;
            End_Procedure (Property_Write_Name (P));
         end loop;
      end;
      End_Package (+Pkg.Name);
   end Print_Body;

   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type) is
   begin
      --  Print the builtin Properties interface package if
      --  necessary.
      if +Pkg.Real_Name = "org.freedesktop.DBus.Properties" then
         Print_Properties (Pkg);
      else
         Print_Spec (Pkg);
         Print_Body (Pkg);
      end if;
   end Print;
end Codegen.Server.Iface;
