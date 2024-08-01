pragma Ada_2005;

with Ada.Strings.Unbounded;

with dbus_types_h;
with dbus_errors_h;
with dbus_bus_h;
with dbus_shared_h;
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

   -----------------
   -- Elaboration --
   -----------------
begin
   --  Set up locked (multithreaded) DBus
   declare
      use type dbus_types_h.dbus_bool_t;
   begin
      if dbus_threads_h.dbus_threads_init_default /= 1 then
         raise D_Bus_Error with "Unable to initialise locked D_Bus runtime.";
      end if;
   end;

   declare
      use type dbus_types_h.dbus_bool_t;

      CO  : Connection_Overlay;
      Err : aliased dbus_errors_h.DBusError;
   begin
      CO.Thin_Connection :=
        dbus_bus_h.dbus_bus_get_private
          (dbus_shared_h.DBUS_BUS_SESSION, Err'Access);

      if dbus_errors_h.dbus_error_is_set (Err'Access) = 1 then
         raise D_Bus_Error with "Unable to acquire a private session bus.";
      end if;

      Internal_Connection := Convert (CO);
   end;
end D_Bus.Support;
