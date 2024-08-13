pragma Ada_2012;

with D_Bus.Arguments.Containers;

private with Ada.Strings.Unbounded;
private with D_Bus.Messages;
private with D_Bus.Support.Message_Handlers;

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
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Same as D_Bus.Connection.Call_Blocking but object-oriented

   procedure Call_No_Reply
     (O    : Client_Interface; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List) is abstract;
   --  Same as D_Bus.Connection.Call_No_Reply but object-oriented

   -------------
   -- Signals --
   -------------
   function Await_Signal
     (O : in out Client_Interface; Iface : String; Name : String)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Return the message for a signal on `O` with interface `Iface`
   --   and name `Name`.
   --
   --  `O` must have been registered first before waiting for signals.
   --
   --  This blocks until a matching signal is received.
   --  TODO add a timeout option

   procedure Purge_Signal
     (O : in out Client_Interface; Iface : String; Name : String) is abstract;
   --  Purge the cache of signals from object `O`, on interface
   --  `Iface`, named `Name`.

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

   overriding function Await_Signal
     (O : in out Client_Object; Iface : String; Name : String)
      return D_Bus.Arguments.Argument_List_Type;

   overriding procedure Purge_Signal
     (O : in out Client_Object; Iface : String; Name : String);

   overriding function Call_Blocking
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List)
      return D_Bus.Arguments.Argument_List_Type;

   procedure Call_No_Reply
     (O    : Client_Object; Iface : String; Method : String;
      Args : D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Arguments.Empty_Argument_List);

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
     (O          : out Client_Object;
      Connection : D_Bus.Connection.Connection_Type;
      Node       : D_Bus.Types.Obj_Path);
   --  See `D_Bus.Support.Create`

   procedure Register (O : access Client_Object'Class);
   --  Register `O` on its connection and allow it to receive signals.
   --  This is optional for any object which does not need to receive signals.

   function Destination (O : Client_Object) return String;
   --  Get the destination associated with `O`. This will return
   --  an empty String if no destination has been set.

   procedure Set_Destination (O : in out Client_Object; Destination : String);
   --  Set the D_Bus destination associated with `O`. By default, the
   --  object has no destination and messages cannot be sent over it.

   overriding procedure Destroy (O : in out Client_Object);
   --  See `D_Bus.Support.Destroy`
   --
   --  This also clears the signal cache associated with `O`
private
   type Client_Object is
   limited new Root_Object and Client_Interface with record
      Destination : Ada.Strings.Unbounded.Unbounded_String;
      Messages    : D_Bus.Support.Message_Handlers.Message_Box_Type;
      Last_Signal : D_Bus.Messages.Message_Type := D_Bus.Messages.Null_Message;
   end record;
end D_Bus.Support.Client;
