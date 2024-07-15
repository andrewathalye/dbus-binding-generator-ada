with DOM.Core.Elements;
with DOM.Core.Nodes;

with Shared; use Shared;
with Debug;  use Debug;

package body Parsing is
   --  D_Bus argument
   function Process_Argument (Node : DOM.Core.Node) return Argument_Type is
     (+DOM.Core.Elements.Get_Attribute (Node, "name"),
      +DOM.Core.Elements.Get_Attribute (Node, "type"),
      +DOM.Core.Elements.Get_Attribute (Node, "direction"));

   --  D_Bus method
   function Process_Method (Node : DOM.Core.Node) return Method_Type;
   function Process_Method (Node : DOM.Core.Node) return Method_Type is
      Method_Name : Ada.Strings.Unbounded.Unbounded_String;

      Args      : DOM.Core.Node_List;
      Arguments : Argument_List;
   begin
      --  Read name
      Method_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Method: " & (+Method_Name));

      Args := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "arg");

      for AI in 1 .. DOM.Core.Nodes.Length (Args) loop
         Arguments.Append
           (Process_Argument (DOM.Core.Nodes.Item (Args, AI - 1)));
      end loop;

      return (Method_Name, Arguments);
   end Process_Method;

   --  D_Bus interface
   function Process_Interface (Node : DOM.Core.Node) return Interface_Type;
   function Process_Interface (Node : DOM.Core.Node) return Interface_Type is
      Interface_Name : Ada.Strings.Unbounded.Unbounded_String;

      Methods     : DOM.Core.Node_List;
      Methods_Ada : Method_List;
   begin
      --  Read name
      Interface_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Interface: " & (+Interface_Name));

      Methods := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "method");

      for MI in 1 .. DOM.Core.Nodes.Length (Methods) loop
         Methods_Ada.Append
           (Process_Method (DOM.Core.Nodes.Item (Methods, MI - 1)));
      end loop;

      return (Interface_Name, Methods_Ada);
   end Process_Interface;

   --  D_Bus node not to be confused with XMLAda node
   function Process_Node (Node : DOM.Core.Node) return Node_Type is
      Node_Name : Ada.Strings.Unbounded.Unbounded_String;

      Nodes          : DOM.Core.Node_List;
      Interfaces     : DOM.Core.Node_List;
      Interfaces_Ada : Interface_List;
   begin
      --  Read name
      Node_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Node: " & (+Node_Name));

      Nodes      := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "node");
      Interfaces :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "interface");

      for NI in 2 .. DOM.Core.Nodes.Length (Nodes) loop
         raise Program_Error with "Nested nodes not currently supported";
         --         Process_Node (DOM.Core.Nodes.Item (Nodes, NI - 1));
      end loop;

      for II in 1 .. DOM.Core.Nodes.Length (Interfaces) loop
         Interfaces_Ada.Append
           (Process_Interface (DOM.Core.Nodes.Item (Interfaces, II - 1)));
      end loop;

      return (Node_Name, Interfaces_Ada);
   end Process_Node;
end Parsing;
