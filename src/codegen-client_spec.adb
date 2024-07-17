with Codegen.Output.Client;
use Codegen.Output.Client;
use Codegen.Output;

with Shared; use Shared;

package body Codegen.Client_Spec is
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
         --  Builtins
         if Pkg.Methods.Is_Empty
            and Pkg.Signals.Is_Empty and Pkg.Signals.Is_Empty
         then
            null;
         else
            Large_Comment ("Builtin");
            Declare_Subtype
              ("Object_Path", "Ada.Strings.Unbounded.Unbounded_String");
            Declare_Subtype
              ("Signature_Type", "Ada.Strings.Unbounded.Unbounded_String");
            New_Line;

            Declare_Entity ("No_Destination", "exception");
            Declare_Procedure ("Set_Destination (Dest : String)");
            Comment ("Connect and prepare to send messages to `Dest`");
            Comment ("This must be called before any other method.");
            New_Line;
         end if;

         --  Generated types
         if not Pkg.Type_Declarations.Is_Empty then
            Large_Comment ("Generated Types");
            Declare_Types (Pkg);
         end if;

         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
            for M of Pkg.Methods loop
               Declare_Procedure (Method_Signature (M));
               New_Line;
            end loop;
         end if;

         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
            for S of Pkg.Signals loop
               Declare_Procedure (Signal_Signature (S));
               New_Line;
            end loop;
         end if;

         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
            for P of Pkg.Properties loop
               if P.PAccess in Parsing.Read | Parsing.Readwrite then
                  Declare_Function (Property_Read_Signature (P));
               end if;

               if P.PAccess in Parsing.Write | Parsing.Readwrite then
                  Declare_Procedure (Property_Write_Signature (P));
               end if;
               New_Line;
            end loop;
         end if;
      End_Package (+Pkg.Name);
      --!pp on
   end Print;
end Codegen.Client_Spec;
