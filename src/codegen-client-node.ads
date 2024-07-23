with Parsing;

package Codegen.Client.Node is
   procedure Print_Spec (N : Parsing.Node_Type);
   --  Print a complete Ada specification for a
   --  D_Bus node, _excluding_ its interfaces.

   procedure Print_Body (N : Parsing.Node_Type);
   --  Print the corresponding body.
end Codegen.Client.Node;
