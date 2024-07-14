with Shared; use Shared;

package body Codegen.The_Body is
   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type; File : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
   begin
      --  Preamble
      Put_Line (File, "with D_Bus.Connection;");
      Put_Line (File, "with D_Bus.Arguments;");
      New_Line (File);

      --  Package
      Put_Line (File, "package body " & (+Pkg.Name) & " is");

      --  Globals
      Put_Line (File, "Connection : D_Bus.Connection.Connection_Type;");
      Put_Line
        (File,
         "Destination : constant String := " &
         ASCII.Quotation & ASCII.Quotation & ";");
      Put_Line
        (File,
         "Path : constant String := " &
         ASCII.Quotation & (+Pkg.Node) & ASCII.Quotation & ";");
      Put_Line
        (File,
         "Iface : constant String := " &
         ASCII.Quotation & (+Pkg.Iface) & ASCII.Quotation & ";");
      New_Line (File);

      --  Private
      Put_Line (File, "procedure Connect is");
      Put_Line (File, "begin");
      Put_Line (File, "Connection := D_Bus.Connection.Connect;");
      Put_Line (File, "end Connect;");
      New_Line (File);

      --  Subprograms
      for SP of Pkg.Subprograms loop
         Print_Signature (SP, File);
         Put_Line (File, " is");

         --  Locals
         Put_Line
           (File,
            "Request_Args, Reply_Args : D_Bus.Arguments.Argument_List_Type;");

         Put_Line (File, "begin");

         --  Bind each in argument
         for A of SP.Arguments loop
            if +A.Direction = "in" then
               Bind_To_DBus (A);
            end if;
         end loop;

         --  The call itself
         Put (File, "Reply_Args := D_Bus.Connection.Call_Blocking ");
         Put (File, "(Connection, Destination, Path, Iface, ");
         Put (File, ASCII.Quotation & (+SP.Name) & ASCII.Quotation);
         Put_Line (File, ", D_Bus.Connection.Default_Timeout, Request_Args);");

         --  Bind each out argument
         for A of SP.Arguments loop
            if +A.Direction = "out" then
               Bind_To_Ada (A);
            end if;
         end loop;

         Put_Line (File, "end " & (+SP.Name) & ";");
         New_Line (File);
      end loop;

      --  Package
      Put_Line (File, "end " & (+Pkg.Name) & ";");
   end Print;

end Codegen.The_Body;
