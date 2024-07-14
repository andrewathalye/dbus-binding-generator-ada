with Ada.Text_IO;

package Codegen.Specification is
   procedure Print
     (Pkg : Ada_Package_Type;
      File : Ada.Text_IO.File_Type);
   --  Print out a complete Ada specification
   --  given a `Codegen.Ada_Package_Type`
end Codegen.Specification;
