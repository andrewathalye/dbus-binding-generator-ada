pragma Ada_2012;

with Codegen.Maps;

package Codegen.Server.Objects is
   procedure Print (Pkgs : Codegen.Maps.Ada_Package_Map);
   --  Print a full package `D_Bus.Generated_Objects`
   --  based upon a list of D_Bus interfaces (as Ada packages)
   --
   --  This package contains subprograms used to register
   --  objects implementing generated interfaces.
end Codegen.Server.Objects;
