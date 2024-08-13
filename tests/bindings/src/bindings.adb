pragma Ada_2012;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.OS_Lib;

with D_Bus.Connection;
with D_Bus.G_Main;

with Server;
with Client;

with Properties_Client;
with Properties_Server;

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
   Properties_Task : Test_Task (Properties_Server'Access);

   procedure Test_Routine;
   procedure Test_Routine is
   begin
      Ada.Text_IO.Put_Line ("TEST: Client/Server");
      Server_Task.Start;
      Client (Shared_Connection);
      Server_Task.Done;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("TEST: Properties");
      Properties_Task.Start;
      Properties_Client (Shared_Connection);
      Properties_Task.Done;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("TEST: Mixed");
      Mixed (Shared_Connection);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("TEST: Annotations");
      Annotations_Task.Start;
      Annotations_Client (Shared_Connection);
      Annotations_Task.Done;
      Ada.Text_IO.New_Line;
   end Test_Routine;
begin
   case Ada.Command_Line.Argument_Count is
      when 0 =>
         Ada.Text_IO.Put_Line ("TEST: Single");
         Test_Routine;
      when 1 =>
         if Ada.Command_Line.Argument (1) = "loop" then
            for I in 1 .. 1000 loop
               Ada.Text_IO.Put_Line ("TEST: Loop");
               Test_Routine;
            end loop;
         elsif Ada.Command_Line.Argument (1) = "client" then
            Properties_Task.Start;
            Properties_Task.Done;
         elsif Ada.Command_Line.Argument (1) = "server" then
            Properties_Client (Shared_Connection);
         else
            raise Program_Error;
         end if;
      when others => raise Program_Error;
   end case;

   Ada.Text_IO.Put_Line ("PASS");

   --  Free all connections
   D_Bus.Connection.Disconnect (Private_Connection);
   D_Bus.Connection.Unref (Shared_Connection);

   --  Free all global variables used by D_Bus/Ada
   D_Bus.Shutdown;
exception
   when X : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Information (X));

      GNAT.OS_Lib.OS_Exit (-1);
end Bindings;
