pragma Ada_2005;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

with D_Bus.Arguments;
with D_Bus.Arguments.Containers;
with D_Bus.Messages;

package D_Bus.Support.Server is
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
     (O : Server_Interface;
      Iface : String;
      Name : String;
      Args : D_Bus.Arguments.Argument_List_Type) is abstract;
   --  See `D_Bus.Connection.Send_Signal`

   -------------
   -- Methods --
   -------------
   procedure Reply
     (O : Server_Interface;
      Request : D_Bus.Messages.Message_Type;
      Args : D_Bus.Arguments.Argument_List_Type) is abstract;
   --  Produce and dispatch a reply to `Request`
   --  containing `Args`. The reply will come
   --  from the object associated with `O`

   ----------------
   -- Properties --
   ----------------
   procedure Set_Property
     (O : in out Server_Interface;
      Iface : String;
      Name : String;
      Value : D_Bus.Arguments.Containers.Variant_Type) is abstract;
   --  Set a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Set`

   procedure Get_Property
     (O : Server_Interface;
      Iface : String;
      Name : String;
      Value : out D_Bus.Arguments.Containers.Variant_Type) is abstract;
   --  Get a property with semantics identical to the
   --  standard `org.freedesktop.DBus.Properties.Get`

   -------------------
   -- Server_Object --
   -------------------
   type Server_Object is abstract limited new Root_Object and Server_Interface
   with private;
   --  The root type of all serverside D_Bus objects.
   --  This is distinct from Client_Object to allow clientside and serverside
   --  bindings to be used by the same application.

   --------------------
   -- Implementation --
   --------------------
   procedure Send_Signal
     (O : Server_Object;
      Iface : String;
      Name : String;
      Args : D_Bus.Arguments.Argument_List_Type);

   procedure Reply
     (O : Server_Object;
      Request : D_Bus.Messages.Message_Type;
      Args : D_Bus.Arguments.Argument_List_Type);

   procedure Set_Property
     (O : in out Server_Object;
      Iface : String;
      Name : String;
      Value : D_Bus.Arguments.Containers.Variant_Type);

   procedure Get_Property
     (O : Server_Object;
      Iface : String;
      Name : String;
      Value : out D_Bus.Arguments.Containers.Variant_Type);

   --------------------------------
   -- Constructors / Destructors --
   --------------------------------
   procedure Create (O : out Server_Object; Node : Unbounded_Object_Path);
   --  Create a new Server_Object

   procedure Request_Name (Name : String);
   --  Set the global D_Bus name for this application’s connection.
   --  A connection can have multiple names, but each name can only
   --  be used by one application.

   overriding procedure Destroy (O : in out Server_Object);
private
   use type D_Bus.Arguments.Containers.Variant_Type;

   --  "Iface:Property_Name" => Property_Value
   package Property_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, D_Bus.Arguments.Containers.Variant_Type, Ada.Strings.Hash, "=");

   type Server_Object is limited new Root_Object and Server_Interface with
   record
      Properties : Property_Maps.Map;
   end record;
end D_Bus.Support.Server;
