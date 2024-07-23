pragma Ada_2005;
with D_Bus.Connection;
with D_Bus.Connection.Dispatch;
with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

package body D_Bus.Support is
   Connection : constant D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect;

   ------------------
   -- Get_Property --
   ------------------
   function Get_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String) return D_Bus.Arguments.Argument_Type'Class
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Request, Reply : D_Bus.Arguments.Argument_List_Type;
   begin
      --  Generate request
      D_Bus.Arguments.Append (Request, +Iface);
      D_Bus.Arguments.Append (Request, +Property);

      --  Reply
      Reply :=
        Call_Blocking
          (Destination, Node, "org.freedesktop.DBus.Properties",
           "Get", Request);

      return
        D_Bus.Arguments.Containers.Variant_Type (Reply.First_Element)
          .Get_Argument;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------
   procedure Set_Property
     (Destination : String; Node : D_Bus.Types.Obj_Path; Iface : String;
      Property    : String; Value : D_Bus.Arguments.Argument_Type'Class)
   is
      use type D_Bus.Arguments.Basic.String_Type;

      Variant          : D_Bus.Arguments.Containers.Variant_Type;
      Request, Discard : D_Bus.Arguments.Argument_List_Type;
   begin
      --  Generate request
      Variant := D_Bus.Arguments.Containers.Create (Value);
      D_Bus.Arguments.Append (Request, +Iface);
      D_Bus.Arguments.Append (Request, +Property);
      D_Bus.Arguments.Append (Request, Variant);

      --  Reply
      Discard := Call_Blocking
        (Destination,
         Node, "org.freedesktop.DBus.Properties", "Set", Request);
   end Set_Property;

   -------------------
   -- Call_Blocking --
   -------------------
   function Call_Blocking
     (Destination : String; Path : D_Bus.Types.Obj_Path; Iface : String;
      Method      : String; Args : D_Bus.Arguments.Argument_List_Type)
      return D_Bus.Arguments.Argument_List_Type
   is
   begin
      return D_Bus.Connection.Call_Blocking
        (Connection, Destination, Path, Iface, Method,
         D_Bus.Connection.Default_Timeout, Args);
   end Call_Blocking;

   ------------------
   -- Await_Signal --
   ------------------
   Signal_Node : D_Bus.Types.Obj_Path;
   Signal_Msg : D_Bus.Messages.Message_Type;
   Signal_Handled : exception;

   procedure Signal_Handler (Msg : D_Bus.Messages.Message_Type) is
   begin
      if D_Bus.Messages.Get_Path (Msg) = D_Bus.Types.To_String (Signal_Node)
      then
         Signal_Msg := Msg;
         raise Signal_Handled with "";
      end if;
   end Signal_Handler;

   function Await_Signal
     (Node : D_Bus.Types.Obj_Path; Iface : String; Signal : String)
      return D_Bus.Messages.Message_Type
   is
      
      Match_Rule : constant String :=
        "path=" & D_Bus.Types.To_String (Node) & ", interface=" & Iface &
        ", member=" & Signal;
      procedure Remove_Match is
         use D_Bus.Arguments.Basic;
         use type D_Bus.Types.Obj_Path;

         Args, Discard : D_Bus.Arguments.Argument_List_Type;
      begin
         D_Bus.Arguments.Append (Args, +Match_Rule);
         Discard :=
           Call_Blocking
             ("org.freedesktop.DBus", +"/org/freedesktop/DBus",
              "org.freedesktop.DBus", "RemoveMatch", Args);
      end Remove_Match;
   begin
      D_Bus.Connection.Add_Match (Connection, Match_Rule);
      Signal_Node := Node;
      D_Bus.Connection.Dispatch (Connection, Signal_Handler'Access);
      raise Program_Error with "Logic error in Await_Signal";
   exception
      when X : Signal_Handled =>
         Remove_Match;
         return Signal_Msg;
   end Await_Signal;
end D_Bus.Support;
