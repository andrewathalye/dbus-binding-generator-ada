pragma Ada_2005;

with dbus_types_h;
with dbus_errors_h;
with dbus_bus_h;
with dbus_shared_h;

package body D_Bus.Support is
   ---------------
   -- Internals --
   ---------------
   --  Lock Implementation
   protected body Lock is
      entry Acquire when not Locked is
      begin
         Locked := True;
      end Acquire;

      entry Release when Locked is
      begin
         Locked := False;
      end Release;
   end Lock;

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

   ------------
   -- Public --
   ------------
   function Node (O : Root_Object'Class) return Unbounded_Object_Path is
      use D_Bus.Types;
   begin
      Assert_Valid (O);
      return Ada.Strings.Unbounded.To_Unbounded_String (To_String (O.Node));
   end Node;
begin
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

      Connection := Convert (CO);
   end;
end D_Bus.Support;
