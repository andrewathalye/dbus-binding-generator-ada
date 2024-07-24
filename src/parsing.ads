with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Ada.Unchecked_Deallocation;
with DOM.Core;

package Parsing is
   type DBus_Direction is (DIn, DOut);
   type DBus_Access is (Read, Write, Readwrite);

   --  TODO annotations are not processed
   type Argument_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
      Direction : DBus_Direction;
   end record;

   package Argument_Lists is new Ada.Containers.Vectors
     (Positive, Argument_Type);
   subtype Argument_List is Argument_Lists.Vector;

   type Method_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Arguments : Argument_List;
   end record;

   package Method_Lists is new Ada.Containers.Vectors (Positive, Method_Type);
   subtype Method_List is Method_Lists.Vector;

   subtype Signal_Type is Method_Type;
   package Signal_Lists renames Method_Lists;
   subtype Signal_List is Signal_Lists.Vector;

   type Property_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Ada.Strings.Unbounded.Unbounded_String;
      PAccess   : DBus_Access;
   end record;

   package Property_Lists is new Ada.Containers.Vectors
     (Positive, Property_Type);
   subtype Property_List is Property_Lists.Vector;

   type Interface_Type is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Methods    : Method_List;
      Signals    : Signal_List;
      Properties : Property_List;
   end record;

   package Interface_Lists is new Ada.Containers.Vectors
     (Positive, Interface_Type);
   subtype Interface_List is Interface_Lists.Vector;

   type Node_Type;
   type Node_Access is access Node_Type;

   package Node_Lists is new Ada.Containers.Vectors (Positive, Node_Access);
   subtype Node_List is Node_Lists.Vector;

   type Node_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Child_Nodes : Node_List;
      Interfaces  : Interface_List;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   function Process_Node (Node : DOM.Core.Node) return Node_Type;
   --  Reads the XML introspection data into memory and produces
   --  a structure suitable for binding generation.
   --
   --  Note: If there are child nodes, they must be freed recursively
   --  using the function `Free`
end Parsing;
