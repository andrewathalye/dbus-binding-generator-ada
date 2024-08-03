pragma Ada_2012;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.OS_Lib;

with D_Bus.Connection;

with Server;
with Client;
with Mixed;
with Annotations_Client;
with Annotations_Server;

procedure Bindings is
   --  Connections
   Private_Connection : D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect_Private;
   Shared_Connection : D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect;

   type Test_Procedure is access procedure
     (Connection : D_Bus.Connection.Connection_Type);

   task type Test_Task (Proc : Test_Procedure) is
      entry Start;
      entry Done;
   end Test_Task;

   task body Test_Task is
   begin
      loop
         select
            accept Start;
            Proc.all (Private_Connection);
            accept Done;
         or
            terminate;
         end select;
      end loop;
   exception
      when X : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (X));

         GNAT.OS_Lib.OS_Exit (-1);
   end Test_Task;

   Server_Task : Test_Task (Server'Access);
   Annotations_Task : Test_Task (Annotations_Server'Access);

   procedure Test_Routine;
   procedure Test_Routine is
   begin
      Ada.Text_IO.Put_Line ("TEST: ClientServer");
      Server_Task.Start;
      Client (Shared_Connection);
      Server_Task.Done;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("TEST: Mixed");
      Mixed;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("TEST: Annotations");
      Annotations_Task.Start;
      Annotations_Client (Shared_Connection);
      Annotations_Task.Done;
      Ada.Text_IO.New_Line;
   end Test_Routine;
begin
   case Ada.Command_Line.Argument_Count is
      when 1 =>
         if Ada.Command_Line.Argument (1) = "server" then
            Ada.Text_IO.Put_Line ("TEST: Server");
            Server (Private_Connection);
            Ada.Text_IO.New_Line;
         elsif Ada.Command_Line.Argument (1) = "test" then
            Test_Routine;
         elsif Ada.Command_Line.Argument (1) = "loop" then
            for I in 1 .. 1000 loop
               Ada.Text_IO.Put_Line ("TEST: Loop");
               Test_Routine;
            end loop;
         else
            raise Program_Error;
         end if;
      when others => raise Program_Error;
   end case;

   --  Free all connections
   D_Bus.Connection.Disconnect (Private_Connection);
   D_Bus.Connection.Free (Private_Connection);
   D_Bus.Connection.Free (Shared_Connection);
exception
   when X : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Information (X));

      GNAT.OS_Lib.OS_Exit (-1);
end Bindings;
