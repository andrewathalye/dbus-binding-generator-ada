pragma Ada_2005;

private with Ada.Strings.Hash;
private with Ada.Containers.Indefinite_Hashed_Maps;

with D_Bus.Messages;
with D_Bus.Arguments;

package D_Bus.Support.Client is
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
   -- Signals --
   -------------
   procedure Register_Signal
     (O : in out Client_Interface; Iface : String; Name : String) is abstract;
   --  Register a signal and store the registration data internally.

   procedure Unregister_Signal
     (O : in out Client_Interface; Iface : String; Name : String) is abstract;
   --  Unregister a signal that was stored internally.

   procedure Await_Signal
     (O     : Client_Interface; Msg : out D_Bus.Messages.Message_Type;
      Iface : String; Name : String) is abstract;
   --  Return the message for a registered signal with
   --  interface `Iface` and name `Name`.

   -------------
   -- Methods --
   -------------
   function Call_Blocking
     (O    : Client_Interface; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Same as D_Bus.Connection.Call_Blocking but object-oriented

   ----------------------------------------
   -- Implementation for `Client_Object` --
   ----------------------------------------
   type Client_Object is abstract limited new Root_Object and Client_Interface
   with private;

   overriding procedure Register_Signal
     (O : in out Client_Object; Iface : String; Name : String);

   overriding procedure Unregister_Signal
     (O : in out Client_Object; Iface : String; Name : String);

   overriding procedure Await_Signal
     (O : Client_Object; Msg : out D_Bus.Messages.Message_Type; Iface : String;
      Name : String);

   overriding function Call_Blocking
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type;

   ----------------------------------
   -- Constructors and Destructors --
   ----------------------------------
   procedure Create (O : out Client_Object; Node : Unbounded_Object_Path);

   procedure Set_Destination (O : in out Client_Object; Destination : String);

   overriding procedure Destroy (O : in out Client_Object);
private
   --  Signal_Name => Signal_Match_Rule
   package Signal_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=");

   type Client_Object is limited new Root_Object and Client_Interface with
   record
      Destination : Ada.Strings.Unbounded.Unbounded_String;
      Signals     : Signal_Maps.Map;
   end record;
end D_Bus.Support.Client;
