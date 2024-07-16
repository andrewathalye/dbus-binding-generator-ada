with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

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
      Signals    : Method_List;
      Properties : Property_List;
   end record;

   package Interface_Lists is new Ada.Containers.Vectors
     (Positive, Interface_Type);
   subtype Interface_List is Interface_Lists.Vector;

   type Node_Type is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      --  Inner_Node_Type (not implemented rn)
      Interfaces : Interface_List;
   end record;

   function Process_Node (Node : DOM.Core.Node) return Node_Type;
   --  Reads the XML introspection data into memory and produces
   --  a structure suitable for binding generation.
end Parsing;
