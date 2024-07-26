with Codegen.Lists;

package Codegen.Server.Dispatcher is
   procedure Print (Pkgs : Codegen.Lists.Ada_Package_List);
   --  Print a full package `D_Bus.Generated_Dispatcher`
   --  based upon a list of D_Bus interfaces (as Ada packages)
   --
   --  This package contains subprograms used to manage
   --  D_Bus objects and Dispatch incoming messages.
end Codegen.Server.Dispatcher;
