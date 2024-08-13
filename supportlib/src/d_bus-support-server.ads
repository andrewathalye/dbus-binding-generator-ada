pragma Ada_2005;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with D_Bus.Arguments;
with D_Bus.Arguments.Containers;
with D_Bus.Messages;

package D_Bus.Support.Server is
   ----------------------
   -- Base Subprograms --
   ----------------------
   procedure Check_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type; Signature : String);
   --  Raise `Invalid_Signature` if the signature of `Arguments` is
   --  not equal to `Signature`

   ------------------------------------
   -- Object-Oriented Server Support --
   ------------------------------------
   type Server_Interface is limited interface;
   --  The root interface of all serverside D_Bus objects.
   --  You should only extend this interface if you wish to
   --  implement your own D_Bus interface. Otherwise, extend
   --  `Server_Object` with the set of interfaces it implements
   --  and use this to make calls.

   -------------
   -- Signals --
   -------------
   procedure Send_Signal
     (O    : Server_Interface; Iface : String; Name : String;
      Args : D_Bus.Arguments.Argument_List_Type) is abstract;
   --  See `D_Bus.Connection.Send_Signal`

   ----------------
   -- Properties --
   ----------------
   type PAccess_Type is (Unchanged, Read, Write, Readwrite);
   type Emit_Behaviour_Type is (Unchanged, True, Invalidates, False);

   procedure Set_Property
     (O         : in out Server_Interface; Iface : String; Name : String;
      Value     :        D_Bus.Arguments.Containers.Variant_Type;
      PAccess   :        PAccess_Type        := Unchanged;
      Does_Emit :        Emit_Behaviour_Type := Unchanged) is abstract;
   --  Set a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Set`
   --
   --  If PAccess is set to anything other than `Unchanged`,
   --  access checks will be disabled and the property will
   --  be created if it did not already exist.
   --
   --  If Emit_Behaviour is set to anything other than `Unchanged`:
   --  True => PropertiesChanged will be emitted on change.
   --  Invalidates => PropertiesChanged with no value will be emitted
   --  False => PropertiesChanged will not be emitted
   --
   --  If Emit_Behaviour is `Unchanged` and the property does not yet
   --  exist, an exception will be raised.

   procedure Get_Property
     (O        :     Server_Interface; Iface : String; Name : String;
      Value    : out D_Bus.Arguments.Containers.Variant_Type;
      Internal :     Boolean := False) is abstract;
   --  Get a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Get`
   --
   --  If `Internal` is set, access checks will be suppressed.

   procedure Get_All_Properties
     (O          :     Server_Interface; Iface : String;
      Properties : out D_Bus.Arguments.Containers.Array_Type) is abstract;
   --  Gets all properties with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.GetAll`

   -------------------
   -- Server_Object --
   -------------------
   type Server_Object is
     abstract limited new Root_Object and Server_Interface with private;
   --  The root type of all serverside D_Bus objects.
   --  See `Server_Interface` and `Root_Object` for more documentation.

   -------------------------
   -- Object Registration --
   -------------------------
   Unknown_Method      : exception;
   --  Message will be discarded
   Unknown_Property    : exception;
   --  Message will be relayed to caller
   Invalid_Signature   : exception;
   --  Message will be relayed to caller
   Property_Read_Only  : exception;
   --  Message will be relayed to caller
   Property_Write_Only : exception;
   --  Message will be relayed to caller

   --  The above are exceptions which a handler should raise in the
   --  event of a failure when processing a message.
   --
   --  They correspond to the standard D_Bus error names.

   type Handler_Access is
     access procedure
       (O       : in out Server_Interface'Class;
        Request :        D_Bus.Messages.Message_Type;
        Reply   :    out D_Bus.Messages.Message_Type);

   package Handler_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Handler_Access, Ada.Strings.Hash, "=");
   subtype Handler_Map is Handler_Maps.Map;

   procedure Register (O : access Server_Object'Class; Handlers : Handler_Map);
   --  Register object `O` with the D_Bus Connection so that its
   --  handler will automatically be called when dispatching pertinent
   --  messages. `Handlers` is a map `<Interface_Name, Handler_Access>`.
   --
   --  Once registered, `O` will be managed by the internal server connection.

   --------------------
   -- Implementation --
   --------------------
   overriding procedure Send_Signal
     (O    : Server_Object; Iface : String; Name : String;
      Args : D_Bus.Arguments.Argument_List_Type);

   overriding procedure Set_Property
     (O         : in out Server_Object; Iface : String; Name : String;
      Value     :        D_Bus.Arguments.Containers.Variant_Type;
      PAccess   :        PAccess_Type        := Unchanged;
      Does_Emit :        Emit_Behaviour_Type := Unchanged);

   overriding procedure Get_Property
     (O        :     Server_Object; Iface : String; Name : String;
      Value    : out D_Bus.Arguments.Containers.Variant_Type;
      Internal :     Boolean := False);

   overriding procedure Get_All_Properties
     (O          :     Server_Object; Iface : String;
      Properties : out D_Bus.Arguments.Containers.Array_Type);

   --------------------------------
   -- Constructors / Destructors --
   --------------------------------
   overriding procedure Create
     (O    : out Server_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path);
   --  See `D_Bus.Support.Create`

   overriding procedure Destroy (O : in out Server_Object);
   --  See `D_Bus.Support.Destroy` for the full details.
   --
   --  This also clears internal structures used by Server Objects
private
   -------------------
   -- Server_Object --
   -------------------
   subtype Valid_PAccess_Type is PAccess_Type range Read .. Readwrite;
   subtype EBT is Emit_Behaviour_Type;
   subtype Valid_Emit_Behaviour_Type is EBT range True .. False;

   type Property_Type is record
      PAccess        : Valid_PAccess_Type;
      Emit_Behaviour : Valid_Emit_Behaviour_Type;
      Value          : D_Bus.Arguments.Containers.Variant_Type;
   end record;

   --  "Name" => "Value"
   package Name_Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Property_Type,
      Hash     => Ada.Strings.Hash, Equivalent_Keys => "=");
   use type Name_Value_Maps.Map;

   --  "Iface" => ("Name" => "Value")
   package Property_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Name_Value_Maps.Map,
      Hash     => Ada.Strings.Hash, Equivalent_Keys => "=");

   type Server_Object is
   limited new Root_Object and Server_Interface with record
      Properties : Property_Maps.Map;
      Handlers   : Handler_Map;
   end record;
end D_Bus.Support.Server;
