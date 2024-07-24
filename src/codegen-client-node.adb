with Ada.Containers;
use type Ada.Containers.Count_Type;

with Codegen.Output; use Codegen.Output;

with Shared; use Shared;

package body Codegen.Client.Node is
   procedure Print_Spec (N : Parsing.Node_Type) is
   begin
      --!pp off
      Use_Pragma ("Ada_2005");
      Use_Pragma ("Warnings (Off, ""not referenced"")");
      New_Line;

      Private_With_Entity ("D_Bus.Arguments");
      Private_With_Entity ("D_Bus.Messages");
      Private_With_Entity ("D_Bus.Types");
      New_Line;

      Start_Package (Sanitise_Name (+N.Name));
      if N.Interfaces.Length > 0 then
         Large_Comment ("Builtin");
         Declare_Entity ("No_Destination", "exception");
         Declare_Procedure ("Set_Destination (Dest : String)");
         Comment ("Set the target for this node’s DBus connection.");
         Comment
           ("This must be called before any other interface methods.");
         Private_Package;
            Declare_Function ("Get_Node return D_Bus.Types.Obj_Path");
            Declare_Function ("Get_Destination return String");
            Declare_Procedure ("Assert_Has_Destination");
      end if;
      End_Package (Sanitise_Name (+N.Name));
      --!pp on
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body (N : Parsing.Node_Type) is
   begin
      --!pp off
      Use_Pragma ("Style_Checks (Off)");
      --------------
      -- Includes --
      --------------
      With_Entity ("Ada.Strings.Unbounded");
      Use_Type ("Ada.Strings.Unbounded.Unbounded_String");

      With_Entity ("D_Bus.Types");
      Use_Type ("D_Bus.Types.Obj_Path");

      Start_Package_Body (Sanitise_Name (+N.Name));
         if N.Interfaces.Length > 0 then
            ----------
            -- Node --
            ----------
            Declare_Entity
              ("Node", "constant D_Bus.Types.Obj_Path",
               "+""" & (+N.Name) & """");
            Start_Function ("Get_Node return D_Bus.Types.Obj_Path");
            Begin_Code;
               Return_Entity ("Node");
            End_Function ("Get_Node");

            -----------------
            -- Destination --
            -----------------
            Declare_Entity
             ("Destination", "Ada.Strings.Unbounded.Unbounded_String");
            Start_Procedure ("Set_Destination (Dest : String)");
            Begin_Code;
               Assign
                 ("Destination",
                  "Ada.Strings.Unbounded.To_Unbounded_String (Dest)");
            End_Procedure ("Set_Destination");

            Start_Function ("Get_Destination return String");
            Begin_Code;
               Return_Entity ("Ada.Strings.Unbounded.To_String (Destination)");
            End_Function ("Get_Destination");

            Start_Procedure ("Assert_Has_Destination");
            Begin_Code;
               Start_If
                 ("Destination = Ada.Strings.Unbounded.Null_Unbounded_String");
                  Raise_Exception
                    ("No_Destination",
                     "No destination set for node " & (+N.Name));
               End_If;
            End_Procedure ("Assert_Has_Destination");
         end if;
      End_Package (Sanitise_Name (+N.Name));
      --!pp on
   end Print_Body;
end Codegen.Client.Node;
