pragma Ada_2005;
with Ada.Strings.Unbounded;

with D_Bus.Arguments;
with D_Bus.Connection;
with D_Bus.Types;
with D_Bus.Messages;

package D_Bus.Support is
   -----------
   -- Types --
   -----------
   subtype Unbounded_Object_Path is Ada.Strings.Unbounded.Unbounded_String;
   subtype Unbounded_Signature is Ada.Strings.Unbounded.Unbounded_String;

   ----------------
   -- Properties --
   ----------------
   function Get_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String) return D_Bus.Arguments.Argument_Type'Class;
   procedure Set_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String; Value : D_Bus.Arguments.Argument_Type'Class);

   -------------
   -- Methods --
   -------------
   function Call_Blocking
     (Destination : String; Path : D_Bus.Types.Obj_Path; Iface : String;
      Method      : String; Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type;

   -------------
   -- Signals --
   -------------
   function Await_Signal
     (Node : D_Bus.Types.Obj_Path; Iface : String; Signal : String)
      return D_Bus.Messages.Message_Type;
end D_Bus.Support;
