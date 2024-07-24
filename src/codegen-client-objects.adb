with Codegen.Output; use Codegen.Output;
with Shared;         use Shared;

package body Codegen.Client.Objects is

   -----------
   -- Print --
   -----------
   procedure Print (Obj_Pkg : Ada_Objects_Package_Type) is
      use type Ada.Containers.Count_Type;

      function All_Implements (NTD : Node_Type_Definition) return String;
      function All_Implements (NTD : Node_Type_Definition) return String is
         use Ada.Strings.Unbounded;

         Buf : Ada.Strings.Unbounded.Unbounded_String;
      begin
         for I of NTD.Implements loop
            Append (Buf, " and ");
            Append (Buf, I);
            Append (Buf, ".Child_Interface");
         end loop;

         return +Buf;
      end All_Implements;
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      New_Line;

      With_Entity ("D_Bus.Support");

      --  With all Interfaces
      for NTD of Obj_Pkg.Nodes loop
         for I of NTD.Implements loop
            With_Entity (+I);
         end loop;
      end loop;
      New_Line;

      Start_Package ("D_Bus.Generated_Objects");
      Use_Pragma ("Elaborate_Body (D_Bus.Generated_Objects)");
      New_Line;

      --  Declare all types
      Large_Comment ("Generated Object Types");
      for NTD of Obj_Pkg.Nodes loop
         if NTD.Implements.Length > 0 then
            Declare_Type
              ("Node_" & (+NTD.Name),
               "new D_Bus.Support.Root_Object" & All_Implements (NTD) &
               " with null record");
            New_Line;
         end if;
      end loop;
      New_Line;

      Large_Comment ("Generated Objects");
      for NTD of Obj_Pkg.Nodes loop
         if NTD.Implements.Length > 0 then
            Declare_Entity ((+NTD.Name), "Node_" & (+NTD.Name));
         end if;
      end loop;

      End_Package ("D_Bus.Generated_Objects");

      --  Elaboration / constructors
      Use_Pragma ("Style_Checks (Off)");
      With_Entity ("Ada.Strings.Unbounded");
      Use_Entity ("Ada.Strings.Unbounded");
      Start_Package_Body ("D_Bus.Generated_Objects");
      Begin_Code;
      for NTD of Obj_Pkg.Nodes loop
         if NTD.Implements.Length > 0 then
            Call
              ((+NTD.Name) & ".Create (To_Unbounded_String (""" &
               (+NTD.Raw_Name) & """))");
         end if;
      end loop;
      End_Package ("D_Bus.Generated_Objects");
   end Print;

   --------------------
   -- Append_Objects --
   --------------------
   procedure Append_Objects
     (Obj_Pkg : in out Ada_Objects_Package_Type; Node : Parsing.Node_Type)
   is
      use Ada.Strings.Unbounded;

      NTD : Node_Type_Definition;
   begin
      NTD.Name     := +Sanitise_Name (+Node.Name);
      NTD.Raw_Name := Node.Name;

      for I of Node.Interfaces loop
         NTD.Implements.Append (+Sanitise_Name (+I.Name));
      end loop;

      Obj_Pkg.Nodes.Append (NTD);
   end Append_Objects;

end Codegen.Client.Objects;
