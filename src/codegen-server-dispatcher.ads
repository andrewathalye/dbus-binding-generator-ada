pragma Ada_2012;

with Codegen.Maps;
with Codegen.Types;

package Codegen.Server.Dispatcher is
   procedure Print
     (Types : Codegen.Types.Ada_Type_Declaration_Map;
      Pkgs  : Codegen.Maps.Ada_Package_Map);
   --  Print a full package `D_Bus.Generated_Dispatcher`
   --  based upon a list of D_Bus interfaces (as Ada packages)
   --
   --  This package contains subprograms used to manage
   --  D_Bus objects and Dispatch incoming messages.
end Codegen.Server.Dispatcher;
