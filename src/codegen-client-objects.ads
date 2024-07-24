private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;

package Codegen.Client.Objects is
   type Ada_Objects_Package_Type is private;

   procedure Print (Obj_Pkg : Ada_Objects_Package_Type);
   --  Prints a package `D_Bus.Generated_Objects` containing
   --  a list of predefined, typed D_Bus nodes.

   procedure Append_Objects
     (Obj_Pkg : in out Ada_Objects_Package_Type; Node : Parsing.Node_Type);
   --  Creates an object for each named, SURFACE-LEVEL node that implements
   --  all that node’s interfaces.
private
   package USL is new Ada.Containers.Vectors
     (Positive, Ada.Strings.Unbounded.Unbounded_String);
   type Node_Type_Definition is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Raw_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Implements : USL.Vector;
   end record;

   package NTDL is new Ada.Containers.Vectors (Positive, Node_Type_Definition);
   type Ada_Objects_Package_Type is record
      Nodes : NTDL.Vector;
   end record;
end Codegen.Client.Objects;
