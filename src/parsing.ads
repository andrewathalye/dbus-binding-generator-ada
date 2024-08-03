with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Ada.Unchecked_Deallocation;
with DOM.Core;

with Signatures.Unbounded;

package Parsing is
   --------------
   -- Argument --
   --------------
   type DBus_Direction is (DIn, DOut);
   type Argument_Type is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code : Signatures.Unbounded.Unbounded_Signature;
      Direction : DBus_Direction;
   end record;

   package Argument_Lists is new Ada.Containers.Vectors
     (Positive, Argument_Type);
   subtype Argument_List is Argument_Lists.Vector;

   ----------------
   -- Annotation --
   ----------------
   type org_freedesktop_DBus_Property_EmitsChangedSignal_Type is
     (True, Invalidates, Const, False);
   subtype OFDP_ECST is org_freedesktop_DBus_Property_EmitsChangedSignal_Type;

   type Annotations_List is record
      org_freedesktop_DBus_Deprecated                  : Boolean   := False;
      org_freedesktop_DBus_Method_NoReply              : Boolean   := False;
      org_freedesktop_DBus_Property_EmitsChangedSignal : OFDP_ECST := True;
   end record;
   --  Record containing all currently-supported annotations.
   --  Default values are from the specification

   ------------
   -- Method --
   ------------
   type Method_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Arguments   : Argument_List;
      Annotations : Annotations_List;
   end record;

   package Method_Lists is new Ada.Containers.Vectors (Positive, Method_Type);
   subtype Method_List is Method_Lists.Vector;

   ------------
   -- Signal --
   ------------
   subtype Signal_Type is Method_Type;
   package Signal_Lists renames Method_Lists;
   subtype Signal_List is Signal_Lists.Vector;

   --------------
   -- Property --
   --------------
   type DBus_Access is (Read, Write, Readwrite);
   type Property_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Type_Code   : Signatures.Unbounded.Unbounded_Signature;
      PAccess     : DBus_Access;
      Annotations : Annotations_List;
   end record;

   package Property_Lists is new Ada.Containers.Vectors
     (Positive, Property_Type);
   subtype Property_List is Property_Lists.Vector;

   ---------------
   -- Interface --
   ---------------
   type Interface_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Methods     : Method_List;
      Signals     : Signal_List;
      Properties  : Property_List;
      Annotations : Annotations_List;
   end record;

   package Interface_Lists is new Ada.Containers.Vectors
     (Positive, Interface_Type);
   subtype Interface_List is Interface_Lists.Vector;

   --------------------
   -- Node (Objects) --
   --------------------
   type Node_Type;
   type Node_Access is access Node_Type;

   package Node_Lists is new Ada.Containers.Vectors (Positive, Node_Access);
   subtype Node_List is Node_Lists.Vector;

   type Node_Type is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Child_Nodes : Node_List;
      Interfaces  : Interface_List;
   end record;

   -----------------
   -- Subprograms --
   -----------------
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   --  Nested nodes are dynamically allocated. Free a Node_Access. This must
   --  be done once and only once per nested node.

   function Process_Node (Node : DOM.Core.Node) return Node_Type;
   --  Reads the XML introspection data into memory and produces
   --  a structure suitable for binding generation.
   --
   --  Note: If there are child nodes, they must be freed recursively
   --  using the function `Free`
end Parsing;
