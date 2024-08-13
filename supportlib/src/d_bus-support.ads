pragma Ada_2005;

with D_Bus.Arguments;
with D_Bus.Types;
with D_Bus.Connection;

private with Ada.Unchecked_Conversion;

private with dbus_connection_h;

package D_Bus.Support is
   --  A note on thread safety: This package initialises D_Bus’
   --  internal locking routine, meaning that it is safe to use
   --  these routines from multiple threads. To do so, however,
   --  no two threads may use the same connection at the same
   --  time. Two threads which use a shared connection are said
   --  to use the same connection.

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

   function Connection
     (O : Root_Object'Class) return D_Bus.Connection.Connection_Type;
   --  Return the connection associated with `O`

   function Node (O : Root_Object'Class) return D_Bus.Types.Obj_Path;
   --  Return the node name associated with `O`

   procedure Create
     (O    : out Root_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path);
   --  Create a new object `O` with path `Node` on Connection `Connection`
   --  This will reference `Connection`.
   --
   --  BASE IMPLEMENTATION: Call first and then set your own structures
   --  and mark `O` as valid.

   procedure Destroy (O : in out Root_Object);
   --  Unregister and Destroy `O`, free associated structures.
   --  This will unreference the connection associated with `O`.
   --
   --  BASE IMPLEMENTATION: Call when ready to mark the object invalid.
private
   --  Types
   type Root_Object is abstract tagged limited record
      Valid      : Boolean := False;
      Registered : Boolean := False;
      Connection : D_Bus.Connection.Connection_Type;
      Node       : D_Bus.Types.Obj_Path;
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
