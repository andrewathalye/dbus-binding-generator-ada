pragma Ada_2005;
with Ada.Strings.Unbounded;

with D_Bus.Arguments;
with D_Bus.Types;
with D_Bus.Messages;

package D_Bus.Support is
   -----------
   -- Types --
   -----------
   subtype Unbounded_Object_Path is Ada.Strings.Unbounded.Unbounded_String;
   Null_Unbounded_Object_Path : constant Unbounded_Object_Path;

   subtype Unbounded_Signature is Ada.Strings.Unbounded.Unbounded_String;
   Null_Unbounded_Signature : constant Unbounded_Signature;

   type Signal_Id is private;
   Null_Signal_Id : constant Signal_Id;

   ----------------
   -- Properties --
   ----------------
   function Get_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String) return D_Bus.Arguments.Argument_Type'Class;
   --  See D_Bus specification for details.

   procedure Set_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String; Value : D_Bus.Arguments.Argument_Type'Class);
   --  See D_Bus specification for details.

   -------------
   -- Methods --
   -------------
   function Call_Blocking
     (Destination : String; Path : D_Bus.Types.Obj_Path; Iface : String;
      Method      : String; Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type;
   --  Same as D_Bus.Connection.Call_Blocking but uses internal connection.

   -------------
   -- Signals --
   -------------
   function Register_Signal
     (Node : D_Bus.Types.Obj_Path;
      Iface : String;
      Signal : String) return Signal_Id;
   --  Register a signal. A signal must be registered before you can use its
   --  ID to call `Await_Signal`.

   procedure Unregister_Signal (Id : out Signal_Id);
   --  Unregister a signal. The signal must have been registered previously.

   function Await_Signal (Id : Signal_Id) return D_Bus.Messages.Message_Type;
   --  Return the message for `Id` when it is received. Extraneous signals
   --  are stored and can be retrieved analogously.
   --  TODO not currently thread-safe
private
   type Signal_Id is record
      Registered : Boolean := False;
      Rule       : Ada.Strings.Unbounded.Unbounded_String;
      Node       : D_Bus.Types.Obj_Path;
      Iface      : Ada.Strings.Unbounded.Unbounded_String;
      Signal     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Null_Unbounded_Object_Path : constant Unbounded_Object_Path :=
      Ada.Strings.Unbounded.Null_Unbounded_String;

   Null_Unbounded_Signature : constant Unbounded_Signature :=
      Ada.Strings.Unbounded.Null_Unbounded_String;

   Null_Signal_Id : constant Signal_Id := (others => <>);
end D_Bus.Support;
