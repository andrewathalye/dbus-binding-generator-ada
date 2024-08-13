pragma Ada_2005;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Support.Message_Handlers;

with dbus_types_h;
with dbus_threads_h;

package body D_Bus.Support is
   ---------------
   -- Internals --
   ---------------
   --  Private Methods
   procedure Assert_Valid (O : Root_Object'Class) is
   begin
      if not O.Valid then
         raise D_Bus_Error with "Attempted to use an invalid object.";
      end if;
   end Assert_Valid;

   procedure Assert_Invalid (O : Root_Object'Class) is
   begin
      if O.Valid then
         raise D_Bus_Error with "Attempted to re-initialise a valid object.";
      end if;
   end Assert_Invalid;

   ----------------
   -- Connection --
   ----------------
   function Connection
     (O : Root_Object'Class) return D_Bus.Connection.Connection_Type
   is
   begin
      Assert_Valid (O);
      return O.Connection;
   end Connection;

   ----------
   -- Node --
   ----------
   function Node (O : Root_Object'Class) return D_Bus.Types.Obj_Path is
   begin
      Assert_Valid (O);
      return O.Node;
   end Node;

   -------------------
   -- Get_Signature --
   -------------------
   function Get_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type) return String
   is
      use Ada.Strings.Unbounded;

      Buf : Unbounded_String;
   begin
      for I in 1 .. Arguments.Get_Count loop
         Append (Buf, Arguments.Get_Element (I).Get_Signature);
      end loop;

      return To_String (Buf);
   end Get_Signature;

   ------------
   -- Create --
   ------------
   procedure Create
     (O    : out Root_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path)
   is
      use type D_Bus.Connection.Connection_Type;
      use D_Bus.Types;
   begin
      Put_Line ("Create " & To_String (Node));
      Assert_Invalid (O);

      --  Ensure connection is not null
      if Connection = D_Bus.Connection.Null_Connection then
         raise D_Bus_Error with "Connection cannot be null";
      end if;

      O.Connection := D_Bus.Connection.Ref (Connection);
      O.Node := Node;

      --  Since this is just a base implementation, don’t set O.Valid
   end Create;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (O : in out Root_Object)
   is
      use D_Bus.Types;
   begin
      Put_Line ("Destroy " & To_String (O.Node));

      Assert_Valid (O);

      if O.Registered then
         D_Bus.Support.Message_Handlers.Unregister (O);
      end if;

      O.Valid := False;

      D_Bus.Connection.Unref (O.Connection);
   end Destroy;

   -----------------
   -- Elaboration --
   -----------------
begin
   --  Set up locked (multithreaded) DBus
   declare
      use type dbus_types_h.dbus_bool_t;
   begin
      pragma Annotate (Xcov, Exempt_On, "internal dbus failure");
      if dbus_threads_h.dbus_threads_init_default /= 1 then
         raise D_Bus_Error with "Unable to initialise locked D_Bus runtime.";
      end if;
      pragma Annotate (Xcov, Exempt_Off);
   end;
end D_Bus.Support;
