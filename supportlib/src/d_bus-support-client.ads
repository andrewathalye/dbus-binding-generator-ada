pragma Ada_2005;

with D_Bus.Arguments.Containers;

private with Ada.Strings.Unbounded;
private with Ada.Strings.Hash;
private with Ada.Containers.Indefinite_Hashed_Maps;

package D_Bus.Support.Client is
   ----------
   -- Base --
   ----------
   Invalid_Signature : exception;

   procedure Check_Signature
     (Arguments : D_Bus.Arguments.Argument_List_Type; Signature : String);
   --  Raise `Invalid_Signature` if the signature of `Arguments` is
   --  not equal to `Signature`

   procedure Check_Signature
     (Argument : D_Bus.Arguments.Argument_Type'Class; Signature : String);
   --  The same, but for a single argument.

   ------------------------------------
   -- Object-Oriented Client Support --
   ------------------------------------
   type Client_Interface is limited interface;
   --  The root interface of all D_Bus clientside objects.
   --  You should only extend `Client_Interface` if you wish
   --  to implement a new D_Bus interface. In any other case,
   --  extend `Client_Object` with the list of interfaces that
   --  object implements.

   -------------
   -- Methods --
   -------------
   function Call_Blocking
     (O    : Client_Interface; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Same as D_Bus.Connection.Call_Blocking but object-oriented

   -------------
   -- Signals --
   -------------
   procedure Register_Signal
     (O : in out Client_Interface; Iface : String; Name : String) is abstract;
   --  Register a signal and store the registration data internally.

   procedure Unregister_Signal
     (O : in out Client_Interface; Iface : String; Name : String) is abstract;
   --  Unregister a signal that was stored internally.

   function Await_Signal
     (O : Client_Interface; Iface : String; Name : String)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Return the message for a registered signal with
   --  interface `Iface` and name `Name`.

   ----------------
   -- Properties --
   ----------------
   procedure Set_Property
     (O     : Client_Interface; Iface : String; Name : String;
      Value : D_Bus.Arguments.Containers.Variant_Type) is abstract;
   --  Set a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Set`

   procedure Get_Property
     (O     :     Client_Interface; Iface : String; Name : String;
      Value : out D_Bus.Arguments.Containers.Variant_Type) is abstract;
   --  Get a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Get`

   ----------------------------------------
   -- Implementation for `Client_Object` --
   ----------------------------------------
   type Client_Object is
     abstract limited new Root_Object and Client_Interface with private;
   --  The progenitor type of all D_Bus client objects.
   --  Also see `Client_Interface` and `Root_Object` for more documentation.

   overriding procedure Register_Signal
     (O : in out Client_Object; Iface : String; Name : String);

   overriding procedure Unregister_Signal
     (O : in out Client_Object; Iface : String; Name : String);

   overriding function Await_Signal
     (O : Client_Object; Iface : String; Name : String)
      return D_Bus.Arguments.Argument_List_Type;

   overriding function Call_Blocking
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type;

   procedure Set_Property
     (O     : Client_Object; Iface : String; Name : String;
      Value : D_Bus.Arguments.Containers.Variant_Type);

   procedure Get_Property
     (O     :     Client_Object; Iface : String; Name : String;
      Value : out D_Bus.Arguments.Containers.Variant_Type);

   ----------------------------------
   -- Constructors and Destructors --
   ----------------------------------
   overriding procedure Create
     (O    : out Client_Object; Connection : D_Bus.Connection.Connection_Type;
      Node :     D_Bus.Types.Obj_Path);
   --  See `D_Bus.Support.Create`

   function Destination (O : Client_Object) return String;
   --  Get the destination associated with `O`. This will return
   --  an empty String if no destination has been set.

   procedure Set_Destination (O : in out Client_Object; Destination : String);
   --  Set the D_Bus destination associated with `O`. By default, the
   --  object has no destination and messages cannot be sent over it.

   overriding procedure Destroy (O : in out Client_Object);
   --  See `D_Bus.Support.Destroy`
   --
   --  Additionally, a `Client_Object` may not be destroyed if any signals
   --  are registered on it. These must first be unregistered.
private
   --  Signal_Name => Signal_Match_Rule
   package Signal_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=");

   type Client_Object is
   limited new Root_Object and Client_Interface with record
      Destination : Ada.Strings.Unbounded.Unbounded_String;
      Signals     : Signal_Maps.Map;
   end record;
end D_Bus.Support.Client;
