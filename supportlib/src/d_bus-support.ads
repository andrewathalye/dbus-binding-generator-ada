pragma Ada_2005;

with D_Bus.Arguments;
with D_Bus.Types;

private with Ada.Unchecked_Conversion;

private with D_Bus.Connection;

private with dbus_connection_h;

package D_Bus.Support is
   pragma Elaborate_Body (D_Bus.Support);

   --  A note on thread safety: This package does not call
   --  `dbus_threads_init_default`, but does its own, independent
   --  locking. This means that it is safe to use these subprograms
   --  from any task, _but_ using the built-in `D_Bus.<>` hierarchy
   --  should only be done after calling `dbus_threads_init_default`.
   --
   --  This package has its own, internal connection to the session bus,
   --  and will not conflict with the default shared connection or any
   --  other private connection.

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

   function Node (O : Root_Object'Class) return D_Bus.Types.Obj_Path;
   --  Return the node name associated with `O`

   procedure Destroy (O : in out Root_Object) is abstract;
   --  Destroy `O` and free all associated structures.
private
   --  Types
   type Root_Object is abstract tagged limited record
      Valid : Boolean := False;
      Node  : D_Bus.Types.Obj_Path;
   end record;

   --  Internal Methods
   procedure Assert_Valid (O : Root_Object'Class);
   procedure Assert_Invalid (O : Root_Object'Class);

   --  Connection Internals
   Connection : D_Bus.Connection.Connection_Type;

   --  Lock Internals
   protected type Lock is
      entry Acquire;
      entry Release;
   private
      Locked : Boolean := False;
   end Lock;

   D_Bus_Lock : Lock;

   --  Datatype Conversions
   type Connection_Overlay is record
      Thin_Connection : access dbus_connection_h.DBusConnection;
   end record;

   function Convert is new Ada.Unchecked_Conversion
     (D_Bus.Connection.Connection_Type, Connection_Overlay);
   function Convert is new Ada.Unchecked_Conversion
     (Connection_Overlay, D_Bus.Connection.Connection_Type);

end D_Bus.Support;
