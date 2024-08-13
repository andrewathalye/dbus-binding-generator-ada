pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

with Ada.Text_IO; use Ada.Text_IO;

with System;
with System.Address_To_Access_Conversions;
with Interfaces.C.Strings;

with dbus_connection_h;
with dbus_message_h;
with dbus_types_h;

package body D_Bus.Support.Message_Handlers is
   ------------
   -- Hashes --
   ------------
   function Hash
     (Path       : String;
      Iface      : String;
      Member     : String) return Message_Hash
   is
   begin
      return Message_Hash (Ada.Strings.Hash
        (Path & ASCII.US & Iface & ASCII.US & Member));
   end Hash;

   function Hash
     (Msg : D_Bus.Messages.Message_Type) return Message_Hash
   is
      use D_Bus.Messages;
   begin
      return Hash
        (Path => Get_Path (Msg),
         Iface => Get_Interface (Msg),
         Member => Get_Member (Msg));
   end Hash;

   ----------------------
   -- Message_Box_Type --
   ----------------------
   use type MRL.Cursor;
   package CL is new Ada.Containers.Vectors (Natural, MRL.Cursor);
   protected body Message_Box_Type is
      procedure Enqueue (Msg : D_Bus.Messages.Message_Type)
      is
      begin
         Data.Append ((Hash (Msg), D_Bus.Messages.Ref (Msg)));
      end Enqueue;

      procedure Consume
        (Id  :     Message_Hash;
         Msg : out D_Bus.Messages.Message_Type)
      is
         Target  : MRL.Cursor;
      begin
         --  Note: in two steps to avoid cursor tampering
         Find_Match :
         for Cursor in Data.Iterate loop
            if MRL.Element (Cursor).Id = Id then
               Target := Cursor;
               goto Found_Match;
            end if;
         end loop Find_Match;
         raise No_Cached_Message;

         <<Found_Match>>
         Msg := MRL.Element (Target).Msg;
         MRL.Delete (Data, Target);
      end Consume;

      procedure Clear (Id : Message_Hash)
      is
         Targets : CL.Vector;
         Msg     : D_Bus.Messages.Message_Type;
      begin
         Put_Line ("Clear " & Id'Image);
         --  Note: in two steps to avoid cursor tampering
         Find_Matches :
         for Cursor in Data.Iterate loop
            if MRL.Element (Cursor).Id = Id then
               Targets.Append (Cursor);
            end if;
         end loop Find_Matches;

         Clear_Matches :
         for Target of Targets loop
            Msg := MRL.Element (Target).Msg;
            D_Bus.Messages.Unref (Msg);
            MRL.Delete (Data, Target);
         end loop Clear_Matches;
      end Clear;

      procedure Clear is
      begin
         for Ref of Data loop
            D_Bus.Messages.Unref (Ref.Msg);
         end loop;

         Data.Clear;
      end Clear;
   end Message_Box_Type;

   --------------------------
   -- C Interface Wrappers --
   --------------------------
   type Root_Object_Class_Access is access all Root_Object'Class;
   --  Not to be freed!

   type Handler_Record is record
      Object  : Root_Object_Class_Access;
      Handler : Message_Handler;
   end record;

   package HR_ATAC is new System.Address_To_Access_Conversions
     (Handler_Record);

   procedure Free is new Ada.Unchecked_Deallocation
     (Handler_Record, HR_ATAC.Object_Pointer);

   procedure Internal_Unregister
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data  : System.Address);
   pragma Convention (C, Internal_Unregister);

   procedure Internal_Unregister
     (Connection : access dbus_connection_h.DBusConnection;
      User_Data  : System.Address)
   is
      pragma Unreferenced (Connection);

      HRA : HR_ATAC.Object_Pointer := HR_ATAC.To_Pointer (User_Data);
   begin
      Free (HRA);
   end Internal_Unregister;

   function Internal_Message
     (Connection : access dbus_connection_h.DBusConnection;
      Message    : access dbus_message_h.DBusMessage;
      User_Data  : System.Address) return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Internal_Message);

   function Internal_Message
     (Connection : access dbus_connection_h.DBusConnection;
      Message    : access dbus_message_h.DBusMessage;
      User_Data  : System.Address) return dbus_shared_h.DBusHandlerResult
   is
      HRA : constant HR_ATAC.Object_Pointer := HR_ATAC.To_Pointer (User_Data);
   begin
      return HRA.Handler
        (O => HRA.Object,
         Connection =>
            Convert
              (Connection_Overlay'(Thin_Connection => Connection)),
         Message => D_Bus.Messages.Create (Message));
   end Internal_Message;

   ---------------
   -- Constants --
   ---------------
   VTable : aliased dbus_connection_h.DBusObjectPathVTable :=
     (unregister_function => Internal_Unregister'Access,
      message_function    => Internal_Message'Access,
      dbus_internal_pad1  => null,
      dbus_internal_pad2  => null,
      dbus_internal_pad3  => null,
      dbus_internal_pad4  => null);

   --------------
   -- Register --
   --------------
   procedure Register
     (O : access Root_Object'Class; Handler : Message_Handler)
   is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO : constant Connection_Overlay := Convert (O.Connection);
      C_Path : Interfaces.C.Strings.chars_ptr :=
         Interfaces.C.Strings.New_String (D_Bus.Types.To_String (O.Node));
      HRA : HR_ATAC.Object_Pointer;

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      Put_Line ("Register " & To_String (O.Node));

      --  Check object
      Assert_Valid (O.all);

      if O.Registered then
         raise D_Bus_Error with "Object cannot be registered twice.";
      end if;

      --  Note: This is unchecked because we allow stack-allocated aliased
      --  objects to be used. Since the type is limited, however, it would be
      --  hard or impossible to exceed the lifetime this way.
      HRA := new Handler_Record'
        (Object => O.all'Unchecked_Access,
         Handler => Handler);

      D_Res := dbus_connection_h.dbus_connection_register_object_path
        (connection => CO.Thin_Connection,
         path => C_Path,
         vtable => VTable'Access,
         user_data => HR_ATAC.To_Address (HRA));

      Interfaces.C.Strings.Free (C_Path);

      --  Check result
      if D_Res /= 1 then
         --  Stop leaks
         Free (HRA);

         raise D_Bus_Error with "Not enough memory to register object.";
      end if;

      --  Also register interest in signals
      D_Bus.Connection.Add_Match
        (O.Connection,
         "type='signal', path='" & To_String (O.Node) & "'");

      O.Registered := True;
   end Register;

   ----------------
   -- Unregister --
   ----------------
   procedure Unregister (O : in out Root_Object'Class) is
      use D_Bus.Types;
      use type dbus_types_h.dbus_bool_t;

      CO : constant Connection_Overlay := Convert (O.Connection);
      C_Path : Interfaces.C.Strings.chars_ptr :=
         Interfaces.C.Strings.New_String (D_Bus.Types.To_String (O.Node));

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      --  Check object
      Assert_Valid (O);

      if not O.Registered then
         raise D_Bus_Error with "Object was not registered.";
      end if;

      D_Res := dbus_connection_h.dbus_connection_unregister_object_path
        (connection => CO.Thin_Connection,
         path => C_Path);

      Interfaces.C.Strings.Free (C_Path);

      --  Check result
      if D_Res /= 1 then
         raise D_Bus_Error with "Not enough memory to unregister object.";
      end if;

      --  Remove signal match rules
      D_Bus.Connection.Remove_Match
        (O.Connection,
         "type='signal', path='" & To_String (O.Node) & "'");

      O.Registered := False;
   end Unregister;
end D_Bus.Support.Message_Handlers;
