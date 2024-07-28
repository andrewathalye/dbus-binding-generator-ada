with Codegen.Types;

package Codegen.Server.Iface is
   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map; Pkg : Ada_Package_Type);
   --  Print a server-side specification for
   --  a D_Bus interface.
end Codegen.Server.Iface;
