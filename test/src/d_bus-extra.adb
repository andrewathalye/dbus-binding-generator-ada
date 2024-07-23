with dbus_errors_h;
with dbus_types_h;
with dbus_signature_h;

with Interfaces.C.Strings;

package body D_Bus.Extra is
   procedure Assert_Success (DB : dbus_types_h.dbus_bool_t) is
      use type dbus_types_h.dbus_bool_t;
   begin
      if DB = 0 then
         raise D_Bus.D_Bus_Error with
            "Could not serialise basic type. See traceback.";
      end if;
   end Assert_Success;

   ------------
   -- DOUBLE --
   ------------
   function To_String (Arg : Double_Type) return String is
   begin
      return Arg.Value'Image
        (Arg.Value'Image'First + 1 .. Arg.Value'Image'Last);
   end To_String;

   function To_Ada (Arg : Double_Type) return Double is
   begin
      return Arg.Value;
   end To_Ada;

   function "+" (L : Double) return Double_Type is
   begin
      return Double_Type'(D_Bus.Arguments.Basic_Type with Value => L);
   end "+";

   overriding procedure Serialize
     (Arg   : Double_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      Value_Aliased : aliased constant Double := Arg.Value;
      DB : dbus_types_h.dbus_bool_t;
   begin
      DB := dbus_message_h.dbus_message_iter_append_basic
        (iter => D_Arg,
         c_type => Character'Pos ('d'),
         value => Value_Aliased'Address);

      Assert_Success (DB);
   end Serialize;

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Double_Type
   is
      Value_Aliased : aliased Double;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter  => D_Arg,
         value => Value_Aliased'Address);

      return +Value_Aliased;
   end Deserialize;

   ---------------------
   -- FILE_DESCRIPTOR --
   ---------------------
   function To_String (Arg : File_Descriptor_Type) return String is
   begin
      return Arg.Value'Image (Arg.Value'Image'First + 1 .. Arg.Value'Image'Last);
   end To_String;

   function To_Ada (Arg : File_Descriptor_Type) return File_Descriptor is
   begin
      return Arg.Value;
   end To_Ada;

   function "+" (L : File_Descriptor) return File_Descriptor_Type
   is
   begin
      return File_Descriptor_Type'(D_Bus.Arguments.Basic_Type with Value => L);
   end "+";

   overriding procedure Serialize
     (Arg   : File_Descriptor_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      Value_Aliased : aliased constant File_Descriptor := Arg.Value;
      DB : dbus_types_h.dbus_bool_t;
   begin
      DB := dbus_message_h.dbus_message_iter_append_basic
        (iter => D_Arg,
         c_type => Character'Pos ('h'),
         value => Value_Aliased'Address);

      Assert_Success (DB);
   end Serialize;

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return File_Descriptor_Type
   is
      Value_Aliased : aliased File_Descriptor;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter => D_Arg,
         value => Value_Aliased'Address);

      return +Value_Aliased;
   end Deserialize;

   ---------------
   -- SIGNATURE --
   ---------------
   function To_String (Arg : Signature_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Arg.Value);
   end To_String;

   function To_Ada (Arg : Signature_Type) return Signature is
   begin
      return Signature (Ada.Strings.Unbounded.To_String (Arg.Value));
   end To_Ada;

   function "+" (L : Signature) return Signature_Type
   is
      use type dbus_types_h.dbus_bool_t;

      Value_Aliased : aliased Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (L));

      Error : aliased dbus_errors_h.DBusError;
      DB : dbus_types_h.dbus_bool_t;
   begin
      --  Error on invalid signatures
      DB := dbus_signature_h.dbus_signature_validate (Value_Aliased, Error'Access);
      Interfaces.C.Strings.Free (Value_Aliased);

      --  Note: this leaks the error object but is considered acceptable
      if DB = 0 then
         raise D_Bus_Error with "Signature check failed: " & Interfaces.C.Strings.Value (Error.name) & ": " & Interfaces.C.Strings.Value (Error.message);
      end if;

      --  Return result if all-clear
      return Signature_Type'(D_Bus.Arguments.Basic_Type with Value => Ada.Strings.Unbounded.To_Unbounded_String (String (L)));
   end "+";

   overriding procedure Serialize
     (Arg   : Signature_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      Value_Aliased : aliased Interfaces.C.Strings.chars_ptr :=
         Interfaces.C.Strings.New_String (Ada.Strings.Unbounded.To_String (Arg.Value));
      DB : dbus_types_h.dbus_bool_t;
   begin
      DB := dbus_message_h.dbus_message_iter_append_basic
        (iter => D_Arg,
         c_type => Character'Pos ('g'),
         value => Value_Aliased'Address);

      Interfaces.C.Strings.Free (Value_Aliased);

      Assert_Success (DB);
   end Serialize;

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Signature_Type
   is
      Value_Aliased : aliased Interfaces.C.Strings.chars_ptr;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter => D_Arg,
         value => Value_Aliased'Address);

      return +Signature (String'(Interfaces.C.Strings.Value (Value_Aliased)));
   end Deserialize;

end D_Bus.Extra;
