with Ada.Containers.Vectors;

package Codegen.Lists is
   package Ada_Package_Lists is new Ada.Containers.Vectors
     (Positive, Ada_Package_Type);
   subtype Ada_Package_List is Ada_Package_Lists.Vector;
end Codegen.Lists;
