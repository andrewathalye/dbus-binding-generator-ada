pragma Ada_2012;

with Codegen.Output;             use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;
with Codegen.Types;

with Signatures;           use Signatures;
with Signatures.Unbounded; use Signatures.Unbounded;
with Parsing;
with Shared;               use Shared;

package body Codegen.Server.Objects is
   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec;
   procedure Print_Spec is
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      New_Line;

      With_Entity ("D_Bus.Support.Server");

      Start_Package ("D_Bus.Generated_Objects");
      begin
         Comment
           ("A package allowing for easy registration of D_Bus objects" &
            " which implement only generated interfaces.");
         New_Line;

         Declare_Procedure
           ("Register" &
            " (O : access D_Bus.Support.Server.Server_Object'Class)");
         Comment
           ("Register the object `O` with D_Bus. Its interfaces must all" &
            " have been generated. See `D_Bus.Support.Server` for specifics.");
      end;
      End_Package ("D_Bus.Generated_Objects");
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body (Pkgs : Codegen.Maps.Ada_Package_Map);
   procedure Print_Body (Pkgs : Codegen.Maps.Ada_Package_Map) is
      use all type Parsing.DBus_Direction;
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Style_Checks (Off)");
      Use_Pragma ("Warnings (Off, ""-gnatwu"")");
      Use_Pragma ("Warnings (Off, ""-gnatwr"")");
      Use_Pragma ("Warnings (Off, ""-gnatwm"")");
      Use_Pragma ("Warnings (Off, ""-gnatwf"")");

      With_Entity ("Ada.Strings.Unbounded");
      With_Entity ("Interfaces");
      With_Entity ("GNAT.OS_Lib");

      With_Entity ("D_Bus.Messages");
      With_Entity ("D_Bus.Arguments.Basic");
      With_Entity ("D_Bus.Arguments.Containers");
      With_Entity ("D_Bus.Types");

      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");

      --  With all interfaces
      for Pkg of Pkgs loop
         With_Entity ((+Pkg.Name) & ".Server");
      end loop;

      Start_Package_Body ("D_Bus.Generated_Objects");
      begin
         --  Interface Handlers
         for Pkg of Pkgs loop
            --  procedure <Iface>
            --    (O   : D_Bus.Support.Server.Server_Interface'Class;
            --     Msg : D_Bus.Messages.Message_Type);
            Start_Procedure (Interface_Handler_Signature (Pkg));
            begin
               --  Method handlers
               for M of Pkg.Methods loop
                  Start_Procedure (Method_Handler_Signature (Pkg, M));
                  begin
                     --  D_Bus in arguments
                     Declare_Entity
                       (Entity => "Args",
                        EType => "constant D_Bus.Arguments.Argument_List_Type",
                        Value  => "D_Bus.Messages.Get_Arguments (Request)");

                     --  Declare arguments
                     for A of M.Arguments loop
                        Declare_Entity
                          ((+A.Name), Get_Ada_Type (+A.Type_Code));
                     end loop;

                     --  Result
                     Declare_Entity
                       ("Result", "D_Bus.Arguments.Argument_List_Type");
                  end;
                  Begin_Code;
                  begin
                     --  Check request signature
                     Call
                       ("D_Bus.Support.Server.Check_Signature (Args, """ &
                        Codegen.Types.Calculate_Request_Signature
                          (M.Arguments) &
                        """)");

                     --  Bind in arguments
                     for I in 1 .. M.Arguments.Last_Index loop
                        if M.Arguments (I).Direction = DIn then
                           Call
                             ("Bind_To_Ada" & " (" &
                              Get_Library_DBus_Type
                                (+M.Arguments (I).Type_Code) &
                              " (Args.Get_Element (" & I'Image & "))" & ", " &
                              (+M.Arguments (I).Name) & ")");
                        end if;
                     end loop;

                     --  Call Method
                     Call ("O." & Method_Call_Expression (M));

                     --  Bind out arguments
                     for A of M.Arguments loop
                        if A.Direction = DOut then
                           Declare_Code;
                           begin
                              Declare_Entity
                                (Entity => "Root_Object",
                                 EType  =>
                                   Get_Library_DBus_Type (+A.Type_Code));
                           end;
                           Begin_Code;
                           begin
                              Call
                                ("Bind_To_DBus (" & (+A.Name) &
                                 ", Root_Object)");
                              Call ("Result.Append (Root_Object)");
                           end;
                           End_Code;
                        end if;
                     end loop;

                     --  Set reply arguments
                     Call ("D_Bus.Messages.Add_Arguments (Reply, Result)");
                  end;
                  End_Procedure (Method_Handler_Name (M));
               end loop;
            end;
            Begin_Code;
            begin
               --  Setup `Reply`
               Assign ("Reply", "D_Bus.Messages.New_Method_Return (Request)");

               --  Run the correct method handler
               for M of Pkg.Methods loop
                  Start_If
                    ("D_Bus.Messages.Get_Member (Request) = """ & (+M.Name) &
                     """");
                  begin
                     Call
                       (Method_Handler_Name (M) & "(" & (+Pkg.Name) &
                        ".Server.Child_Interface'Class" &
                        "(O), Request, Reply)");

                     Return_Null;
                  end;
                  End_If;
               end loop;

               Raise_Exception ("D_Bus.Support.Server.Unknown_Method");
            end;
            End_Procedure (Interface_Handler_Name (Pkg));
         end loop;

         --  Predefined Subprograms
         Start_Procedure
           ("Register" &
            " (O : access D_Bus.Support.Server.Server_Object'Class)");
         begin
            Declare_Entity ("Handlers", "D_Bus.Support.Server.Handler_Map");
         end;
         Begin_Code;
         --  Add handlers for each interface implemented by `O`
         for Pkg of Pkgs loop
            Start_If
              ("O.all in " & (+Pkg.Name) & ".Server.Child_Interface'Class");
            begin
               Call
                 ("Handlers.Insert (""" & (+Pkg.Real_Name) & """, " &
                  Interface_Handler_Name (Pkg) & "'Access)");
            end;
            End_If;
         end loop;

         Call ("D_Bus.Support.Server.Register (O, Handlers)");
         End_Procedure ("Register");
      end;
      End_Package ("D_Bus.Generated_Objects");
   end Print_Body;

   -----------
   -- Print --
   -----------
   procedure Print (Pkgs : Codegen.Maps.Ada_Package_Map) is
   begin
      Print_Spec;
      Print_Body (Pkgs);
   end Print;

end Codegen.Server.Objects;
