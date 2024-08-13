pragma Ada_2012;

with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
use Codegen.Output;
with Codegen.Types;

with Shared;               use Shared;
with Signatures.Unbounded; use Signatures.Unbounded;
with Signatures;           use Signatures;

package body Codegen.Client.Iface is
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
      With_Entity ("Interfaces");
      --  All basic number types
      New_Line;

      With_Entity ("GNAT.OS_Lib");
      --  `File_Descriptor`
      New_Line;

      With_Entity ("D_Bus.Arguments.Containers");
      --  `Variant_Type`

      With_Entity ("D_Bus.Types");
      --  `Obj_Path` `Signature`

      With_Entity ("D_Bus.Support.Client");
      --  `Client_Interface`

      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  <>
      New_Line;

      --  Package Spec
      Start_Package ((+Pkg.Name) & ".Client");
      begin
         --  Implement org.freedesktop.DBus.Deprecated
         if Pkg.Annotations.org_freedesktop_DBus_Deprecated then
            Use_Pragma ("Obsolescent");
         end if;

         Declare_Type
           ("Child_Interface",
            "limited interface and D_Bus.Support.Client.Client_Interface");
         New_Line;

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
            for M of Pkg.Methods loop
               Declare_Procedure (Method_Signature (M));

               --  Implement org.freedesktop.DBus.Deprecated
               if M.Annotations.org_freedesktop_DBus_Deprecated then
                  Use_Pragma ("Obsolescent");
               end if;

               New_Line;
            end loop;
         end if;

         --  Signals
         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
            for S of Pkg.Signals loop
               --  Implement org.freedesktop_DBus.Deprecated
               Declare_Procedure (Signal_Await_Signature (S));
               if S.Annotations.org_freedesktop_DBus_Deprecated then
                  Use_Pragma ("Obsolescent");
               end if;

               Declare_Procedure (Signal_Purge_Signature (S));
               if S.Annotations.org_freedesktop_DBus_Deprecated then
                  Use_Pragma ("Obsolescent");
               end if;

               New_Line;
            end loop;
         end if;

         --  Properties
         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
            for P of Pkg.Properties loop
               if P.PAccess in Parsing.Read | Parsing.Readwrite then
                  Declare_Function (Property_Read_Signature (P));

                  --  Implement org.freedesktop.DBus.Deprecated
                  if P.Annotations.org_freedesktop_DBus_Deprecated then
                     Use_Pragma ("Obsolescent");
                  end if;
               end if;

               if P.PAccess in Parsing.Write | Parsing.Readwrite then
                  Declare_Procedure (Property_Write_Signature (P));

                  --  Implement org.freedesktop.DBus.Deprecated
                  if P.Annotations.org_freedesktop_DBus_Deprecated then
                     Use_Pragma ("Obsolescent");
                  end if;
               end if;
               New_Line;
            end loop;
         end if;
      end;
      End_Package ((+Pkg.Name) & ".Client");
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body (Pkg : Ada_Package_Type);
   procedure Print_Body (Pkg : Ada_Package_Type) is
      use type Parsing.DBus_Direction;
   begin
      --  Don’t generate a body if the package is completely
      --  empty.
      if Pkg.Methods.Is_Empty and Pkg.Signals.Is_Empty and
        Pkg.Properties.Is_Empty
      then
         return;
      end if;

      --  Preamble
      Use_Pragma ("Ada_2012");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");

      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Messages");

      --  Package
      Start_Package_Body ((+Pkg.Name) & ".Client");
      begin
         --  Declares
         Declare_Entity
           ("Iface", "constant String", """" & (+Pkg.Real_Name) & """");

         --  Methods
         for M of Pkg.Methods loop
            Start_Procedure (Method_Signature (M));
            begin
               Declare_Entity
                 ("Request_Args", "D_Bus.Arguments.Argument_List_Type");
               Declare_Entity
                 ("Reply_Args", "D_Bus.Arguments.Argument_List_Type");
            end;
            Begin_Code;
            begin
               --  Bind each in argument
               for A of M.Arguments loop
                  if A.Direction = Parsing.DIn then
                     Declare_Code;
                     begin
                        Declare_Entity
                          ("Root_Object",
                           Get_Library_DBus_Type (+A.Type_Code));
                     end;
                     Begin_Code;
                     begin
                        Call ("Bind_To_DBus (" & (+A.Name) & ", Root_Object)");
                        Call ("Request_Args.Append (Root_Object)");
                     end;
                     End_Code;
                  end if;
               end loop;

               --  The method call itself
               if M.Annotations.org_freedesktop_DBus_Method_NoReply then
                  --  Call with no reply
                  Call
                    ("O.Call_No_Reply (Iface, """ & (+M.Name) &
                     """, Request_Args)");
               else
                  --  Blocking call
                  Assign
                    ("Reply_Args",
                     "O.Call_Blocking (Iface, """ & (+M.Name) &
                     """, Request_Args)");

                  --  Check the reply signature
                  Call
                    ("D_Bus.Support.Client.Check_Signature (Reply_Args, """ &
                     Codegen.Types.Calculate_Reply_Signature (M.Arguments) &
                     """)");
               end if;

               --  Bind each out argument
               declare
                  Index : Positive := 1;
               begin
                  for A of M.Arguments loop
                     if A.Direction = Parsing.DOut then
                        Call
                          ("Bind_To_Ada (" &
                           Get_Library_DBus_Type (+A.Type_Code) &
                           " (Reply_Args.Get_Element (" & Index'Image &
                           ")), " & (+A.Name) & ")");

                        Index := Index + 1;
                     end if;
                  end loop;
               end;
            end;
            End_Procedure (Method_Name (M));
         end loop;

         --  Signals
         for S of Pkg.Signals loop
            Start_Procedure (Signal_Await_Signature (S));
            begin
               Declare_Entity
                 ("Args", "constant D_Bus.Arguments.Argument_List_Type",
                  "O.Await_Signal (Iface, """ & (+S.Name) & """)");
            end;
            Begin_Code;
            begin
               --  Check the signature
               Call
                 ("D_Bus.Support.Client.Check_Signature (Args, """ &
                  Codegen.Types.Calculate_Reply_Signature (S.Arguments) &
                  """)");

               --  Bind arguments
               declare
                  Index : Positive := 1;
               begin
                  for A of S.Arguments loop
                     Call
                       ("Bind_To_Ada (" &
                        Get_Library_DBus_Type (+A.Type_Code) &
                        " (Args.Get_Element (" & Index'Image & ")), " &
                        (+A.Name) & ")");
                     Index := Index + 1;
                  end loop;
               end;
            end;
            End_Procedure (Signal_Await_Name (S));

            Start_Procedure (Signal_Purge_Signature (S));
            Begin_Code;
            begin
               Call ("O.Purge_Signal (Iface, """ & (+S.Name) & """)");
            end;
            End_Procedure (Signal_Purge_Name (S));
         end loop;

         --  Properties
         for P of Pkg.Properties loop
            --  Getter
            if P.PAccess in Parsing.Read | Parsing.Readwrite then
               declare
                  Ada_Type  : constant String := Get_Ada_Type (+P.Type_Code);
                  DBus_Type : constant String :=
                    Get_Library_DBus_Type (+P.Type_Code);
               begin
                  Start_Function (Property_Read_Signature (P));
                  begin
                     Use_Entity ("Ada.Strings.Unbounded");
                     Declare_Entity
                       ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
                     Declare_Entity ("Property", DBus_Type);
                     Declare_Entity ("Property_Ada", Ada_Type);
                  end;
                  Begin_Code;
                  begin
                     --  Get the property via an efficient wrapper
                     Call
                       ("O.Get_Property (Iface, """ & (+P.Name) &
                        """, Variant)");

                     --  Check that the property was of the right type
                     Call
                       ("D_Bus.Support.Client.Check_Signature" &
                        " (Variant.Get_Argument, """ &
                        As_String (+P.Type_Code) & """)");

                     --  Bind to Ada
                     Assign
                       ("Property", DBus_Type & " (Variant.Get_Argument)");

                     Call ("Bind_To_Ada (Property, Property_Ada)");
                     Return_Entity ("Property_Ada");
                  end;
                  End_Procedure (Property_Read_Name (P));
               end;
            end if;

            --  Setter
            if P.PAccess in Parsing.Write | Parsing.Readwrite then
               Start_Procedure (Property_Write_Signature (P));
               begin
                  Use_Entity ("Ada.Strings.Unbounded");
                  Declare_Entity
                    ("Property", Get_Library_DBus_Type (+P.Type_Code));
                  Declare_Entity
                    ("Variant", "D_Bus.Arguments.Containers.Variant_Type");
               end;
               Begin_Code;
               begin
                  Call ("Bind_To_DBus (Value, Property)");

                  Assign
                    ("Variant",
                     "D_Bus.Arguments.Containers.Create (Property)");

                  --  Set the property (via efficient wrapper)
                  Call
                    ("O.Set_Property (Iface, """ & (+P.Name) & """, Variant)");
               end;
               End_Procedure (Property_Write_Name (P));
            end if;
         end loop;
      end;
      End_Package ((+Pkg.Name) & ".Client");
   end Print_Body;

   procedure Print (Pkg : Ada_Package_Type) is
   begin
      Print_Spec (Pkg);
      Print_Body (Pkg);
   end Print;
end Codegen.Client.Iface;
