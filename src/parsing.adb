with DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;

with Signatures.Unbounded; use Signatures.Unbounded;

with Shared; use Shared;
with Debug;  use Debug;

package body Parsing is
   --  D_Bus argument
   function Process_Argument (Node : DOM.Core.Node) return Argument_Type is
     (Name      => +DOM.Core.Elements.Get_Attribute (Node, "name"),
      Type_Code =>
        +Signatures.As_Signature
          (DOM.Core.Elements.Get_Attribute (Node, "type")),
      Direction =>
        (if +DOM.Core.Elements.Get_Attribute (Node, "direction") = "in" then
           DIn
         else DOut));

   --  D_Bus annotation
   procedure Process_Annotation
     (Node : DOM.Core.Node; Annotations : out Annotations_List);
   procedure Process_Annotation
     (Node : DOM.Core.Node; Annotations : out Annotations_List)
   is
      Name : constant String := DOM.Core.Elements.Get_Attribute (Node, "name");
      Value : constant String :=
        DOM.Core.Elements.Get_Attribute (Node, "value");
   begin
      if Name = "org.freedesktop.DBus.Deprecated" then
         Annotations.org_freedesktop_DBus_Deprecated :=
           (if Value = "true" then True else False);
      elsif Name = "org.freedesktop.DBus.Method.NoReply" then
         Annotations.org_freedesktop_DBus_Method_NoReply :=
           (if Value = "true" then True else False);
      elsif Name = "org.freedesktop.DBus.Property.EmitsChangedSignal" then
         Annotations.org_freedesktop_DBus_Property_EmitsChangedSignal :=
           (if Value = "true" then True
            elsif Value = "invalidates" then Invalidates
            elsif Value = "const" then Const else False);
      else
         Put_Debug ("Unknown annotation " & Name & ":" & Value);
      end if;
   end Process_Annotation;

   --  D_Bus method
   function Process_Method (Node : DOM.Core.Node) return Method_Type;
   function Process_Method (Node : DOM.Core.Node) return Method_Type is
      Method_Name : Ada.Strings.Unbounded.Unbounded_String;

      Args            : DOM.Core.Node_List;
      Arguments       : Argument_List;
      Annotations     : DOM.Core.Node_List;
      Annotations_Ada : Annotations_List;
   begin
      --  Read name
      Method_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Method: " & (+Method_Name));

      --  Add arguments
      Args := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "arg");
      for AI in 1 .. DOM.Core.Nodes.Length (Args) loop
         Arguments.Append
           (Process_Argument (DOM.Core.Nodes.Item (Args, AI - 1)));
      end loop;

      --  Add annotations
      Annotations :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "annotation");
      for AI in 1 .. DOM.Core.Nodes.Length (Annotations) loop
         Process_Annotation
           (DOM.Core.Nodes.Item (Annotations, AI - 1), Annotations_Ada);
      end loop;

      return (Method_Name, Arguments, Annotations_Ada);
   end Process_Method;

   --  D_Bus property
   --  TODO add annotation support
   function Process_Property (Node : DOM.Core.Node) return Property_Type;
   function Process_Property (Node : DOM.Core.Node) return Property_Type is
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Signatures.Unbounded.Unbounded_Signature;
      PAccess   : DBus_Access;

      Annotations     : DOM.Core.Node_List;
      Annotations_Ada : Annotations_List;
   begin
      Name      := +DOM.Core.Elements.Get_Attribute (Node, "name");
      Type_Code :=
        +Signatures.As_Signature
          (DOM.Core.Elements.Get_Attribute (Node, "type"));
      PAccess   :=
        (if +DOM.Core.Elements.Get_Attribute (Node, "access") = "read" then
           Read
         elsif +DOM.Core.Elements.Get_Attribute (Node, "access") = "write" then
           Write
         else Readwrite);

      --  Read Annotations
      Annotations :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "annotation");
      for AI in 1 .. DOM.Core.Nodes.Length (Annotations) loop
         Process_Annotation
           (DOM.Core.Nodes.Item (Annotations, AI - 1), Annotations_Ada);
      end loop;

      return (Name, Type_Code, PAccess, Annotations_Ada);
   end Process_Property;

   --  D_Bus interface
   function Process_Interface (Node : DOM.Core.Node) return Interface_Type;
   function Process_Interface (Node : DOM.Core.Node) return Interface_Type is
      Interface_Name : Ada.Strings.Unbounded.Unbounded_String;

      Methods     : DOM.Core.Node_List;
      Methods_Ada : Method_List;

      Signals     : DOM.Core.Node_List;
      Signals_Ada : Method_List;

      Properties     : DOM.Core.Node_List;
      Properties_Ada : Property_List;

      Children        : DOM.Core.Node_List;
      Annotations_Ada : Annotations_List;
   begin
      --  Read name
      Interface_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Interface: " & (+Interface_Name));

      Methods := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "method");
      Signals := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "signal");
      Properties :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "property");

      for MI in 1 .. DOM.Core.Nodes.Length (Methods) loop
         Methods_Ada.Append
           (Process_Method (DOM.Core.Nodes.Item (Methods, MI - 1)));
      end loop;

      --  A Signal is identical to a Method in terms of representation
      for SI in 1 .. DOM.Core.Nodes.Length (Signals) loop
         Signals_Ada.Append
           (Process_Method (DOM.Core.Nodes.Item (Signals, SI - 1)));
      end loop;

      for PI in 1 .. DOM.Core.Nodes.Length (Properties) loop
         Properties_Ada.Append
           (Process_Property (DOM.Core.Nodes.Item (Properties, PI - 1)));
      end loop;

      --  As with nodes, we sadly have to iterate over direct children
      --  to find annotations that apply to the interface.
      --
      --  Otherwise we’d get annotations from the methods, signals, and
      --  properties contained within the interface.
      Children := DOM.Core.Nodes.Child_Nodes (Node);
      for NI in 1 .. DOM.Core.Nodes.Length (Children) loop
         declare
            N : DOM.Core.Node renames DOM.Core.Nodes.Item (Children, NI - 1);
         begin
            if DOM.Core.Elements.Get_Tag_Name (N) = "annotation" then
               Process_Annotation (N, Annotations_Ada);
            end if;
         end;
      end loop;

      return
        (Interface_Name, Methods_Ada, Signals_Ada, Properties_Ada,
         Annotations_Ada);
   end Process_Interface;

   --  D_Bus node not to be confused with XMLAda node
   function Process_Node (Node : DOM.Core.Node) return Node_Type is
      Node_Name : Ada.Strings.Unbounded.Unbounded_String;

      Children       : DOM.Core.Node_List;
      Nodes_Ada      : Node_List;
      Interfaces_Ada : Interface_List;
   begin
      --  Read name
      Node_Name := +DOM.Core.Elements.Get_Attribute (Node, "name");

      Put_Debug ("Node: " & (+Node_Name));

      --  Ugly, but the only way to iterate over direct children
      Children := DOM.Core.Nodes.Child_Nodes (Node);
      for NI in 1 .. DOM.Core.Nodes.Length (Children) loop
         declare
            N : DOM.Core.Node renames DOM.Core.Nodes.Item (Children, NI - 1);
         begin
            if DOM.Core.Elements.Get_Tag_Name (N) = "node" then
               Nodes_Ada.Append (new Node_Type'(Process_Node (N)));
            elsif DOM.Core.Elements.Get_Tag_Name (N) = "interface" then
               Interfaces_Ada.Append (Process_Interface (N));
            end if;
         end;
      end loop;

      return (Node_Name, Nodes_Ada, Interfaces_Ada);
   end Process_Node;
end Parsing;
