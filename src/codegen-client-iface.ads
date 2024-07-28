pragma Ada_2012;

with Codegen.Types;

package Codegen.Client.Iface is
   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   --  Print out a complete Ada client specification
   --  and body for a D_Bus interface.
end Codegen.Client.Iface;
