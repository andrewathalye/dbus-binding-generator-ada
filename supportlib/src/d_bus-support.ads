pragma Ada_2005;

with D_Bus.Arguments;
with D_Bus.Types;
with D_Bus.Connection;

private with Ada.Unchecked_Conversion;

private with dbus_connection_h;

package D_Bus.Support is
   pragma Elaborate_Body (D_Bus.Support);

   --  A note on thread safety: This package initialises D_Bus’
   --  internal locking routine, meaning that it is safe to use
   --  these subprograms from multiple tasks. With that said, however,
   --  it is _not_ safe to access the same object from multiple tasks
   --  simultaneously.

   ------------------------
   -- Signature Checking --
   ------------------------
   function Get_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type) return String;
   --  Return the signature of `Arguments`

   ---------------------------
   -- Object-Oriented D_Bus --
   ---------------------------
   type Root_Object is abstract tagged limited private;
   --  The root object of all D_Bus objects. It provides
   --  the below methods.
   --
   --  Note that each descendent type of `Root_Object`
   --  maintains an internal D_Bus connection. This may
   --  be shared with other objects or unique to that connection.
   --
   --  To assign this connection, use methods provided by
   --  descendent types.

   function Node (O : Root_Object'Class) return D_Bus.Types.Obj_Path;
   --  Return the node name associated with `O`

   procedure Create
     (O          : out Root_Object; Node : D_Bus.Types.Obj_Path;
      Connection :     D_Bus.Connection.Connection_Type) is abstract;

   procedure Create
     (O : out Root_Object; Node : D_Bus.Types.Obj_Path) is abstract;
   --  Create a new object `O` with path `Node`. If `Connection` is
   --  not specified, the default internal session bus connection will
   --  be used.

   procedure Destroy (O : in out Root_Object) is abstract;
   --  Destroy `O` and free all associated structures.
   --  The connection used by `O` will not be freed.
private
   --  Internal Connection
   Internal_Connection : D_Bus.Connection.Connection_Type;

   --  Types
   type Root_Object is abstract tagged limited record
      Valid      : Boolean                          := False;
      Node       : D_Bus.Types.Obj_Path;
      Connection : D_Bus.Connection.Connection_Type := Internal_Connection;
   end record;

   --  Internal Methods
   procedure Assert_Valid (O : Root_Object'Class);
   procedure Assert_Invalid (O : Root_Object'Class);

   --  Datatype Conversions
   type Connection_Overlay is record
      Thin_Connection : access dbus_connection_h.DBusConnection;
   end record;

   function Convert is new Ada.Unchecked_Conversion
     (D_Bus.Connection.Connection_Type, Connection_Overlay);
   function Convert is new Ada.Unchecked_Conversion
     (Connection_Overlay, D_Bus.Connection.Connection_Type);

end D_Bus.Support;
