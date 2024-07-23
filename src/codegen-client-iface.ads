package Codegen.Client.Iface is
   procedure Print_Spec (Pkg : Ada_Package_Type);
   --  Print out a complete Ada client specification
   --  given a `Codegen.Ada_Package_Type`

   procedure Print_Body (Pkg : Ada_Package_Type);
   --  Print the corresponding body
end Codegen.Client.Iface;
