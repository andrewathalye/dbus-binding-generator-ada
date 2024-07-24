pragma Ada_2005;
with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

with D_Bus.Arguments;
with D_Bus.Messages;
private with D_Bus.Types;

package D_Bus.Support is
   -----------
   -- Types --
   -----------
   subtype Unbounded_Object_Path is Ada.Strings.Unbounded.Unbounded_String;
   Null_Unbounded_Object_Path : constant Unbounded_Object_Path;

   subtype Unbounded_Signature is Ada.Strings.Unbounded.Unbounded_String;
   Null_Unbounded_Signature : constant Unbounded_Signature;

   ---------------------------
   -- Object-Oriented D_Bus --
   ---------------------------
   type Root_Interface is limited interface;

   --  OO Signals
   procedure Register_Signal
     (O : in out Root_Interface;
      Iface : String;
      Name : String) is abstract;
   --  Register a signal and store the registration data internally.

   procedure Unregister_Signal
    (O : in out Root_Interface;
     Iface : String;
     Name : String) is abstract;
   --  Unregister a signal that was stored internally.

   procedure Await_Signal
     (O : in out Root_Interface;
      Msg : out D_Bus.Messages.Message_Type;
      Iface : String;
      Name : String) is abstract;
   --  Return the message for a registered signal with
   --  interface `Iface` and name `Name`.

   --  OO Methods
   function Call_Blocking
     (O      : Root_Interface;
      Iface  : String;
      Method : String;
      Args   : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type is abstract;
   --  Same as D_Bus.Connection.Call_Blocking but object-oriented

   ------------------------------
   -- Base Implementation Type --
   ------------------------------
   type Root_Object is limited new Root_Interface with private;

   --  OO Signals
   procedure Register_Signal
     (O : in out Root_Object;
      Iface : String;
      Name : String);

   procedure Unregister_Signal
     (O : in out Root_Object;
      Iface : String;
      Name : String);

   procedure Await_Signal
     (O : in out Root_Object;
      Msg   : out D_Bus.Messages.Message_Type;
      Iface : String;
      Name : String);

   --  OO Methods
   function Call_Blocking
     (O      : Root_Object;
      Iface  : String;
      Method : String;
      Args   : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type;

   --  Constructor and Destructor
   procedure Create
     (O : out Root_Object;
      Node : Unbounded_Object_Path);

   procedure Set_Destination
     (O : out Root_Object;
      Destination : String);

   procedure Destroy (O : in out Root_Object);
private
   subtype Signal_Id is String;
   subtype Signal_Rule is String;

   package SM is new Ada.Containers.Indefinite_Hashed_Maps
     (Signal_Id, Signal_Rule, Ada.Strings.Hash, "=", "=");

   type Root_Object is limited new Root_Interface with record
      Valid : Boolean := False;
      Destination : Ada.Strings.Unbounded.Unbounded_String;
      Node : D_Bus.Types.Obj_Path;
      Signals : SM.Map;
   end record;

   Null_Unbounded_Object_Path : constant Unbounded_Object_Path :=
      Ada.Strings.Unbounded.Null_Unbounded_String;

   Null_Unbounded_Signature : constant Unbounded_Signature :=
      Ada.Strings.Unbounded.Null_Unbounded_String;
end D_Bus.Support;
