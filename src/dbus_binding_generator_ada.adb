pragma Ada_2012;

with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;

with GNAT.Command_Line;
with GNAT.OS_Lib;

--  xmlada
with Input_Sources.File;
with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with Sax.Readers;
with Schema.Dom_Readers;
with Schema.Schema_Readers;
with Schema.Validators;

--  Basic Codegen
with Parsing;
with Codegen;
with Codegen.Maps;

--  Types Codegen
with Codegen.Types;

--  Client / Server Codegen
with Codegen.Client.Iface;
with Codegen.Server.Iface;
with Codegen.Server.Objects;

--  Utils
with Constants;
with Shared; use Shared;
with Debug;  use Debug;

procedure DBus_Binding_Generator_Ada is
   --------------------
   -- Instantiations --
   --------------------
   package File_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   use type Parsing.Node_Type;
   package Node_Lists is new Ada.Containers.Vectors
     (Positive, Parsing.Node_Type);

   -----------------
   -- Subprograms --
   -----------------
   procedure Show_Help;
   procedure Show_Help is
      use Ada.Text_IO;
   begin
      Put_Line
        ("Usage: " & Ada.Command_Line.Command_Name &
         " [-client | -server | -types] [--] [input files]");
      GNAT.OS_Lib.OS_Exit (-1);
   end Show_Help;

   procedure Error_Message (Msg : String);
   procedure Error_Message (Msg : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Msg);
      GNAT.OS_Lib.OS_Exit (-1);
   end Error_Message;

   -----------
   -- Types --
   -----------
   type Mode_Type is (Client, Server, Types);

   ---------------
   -- Constants --
   ---------------
   pragma Style_Checks (Off);
   Introspect_Schema : constant String := $INTROSPECT_SCHEMA;
   pragma Style_Checks (On);

   ---------------
   -- Variables --
   ---------------
   File_List : File_Lists.Vector;
   Node_List : Node_Lists.Vector;
   Mode      : Mode_Type := Client;

   ---------
   -- XML --
   ---------
   Grammar : Schema.Validators.XML_Grammar;
begin
   Put_Debug ("dbus_binding_generator_ada");

   -------------------------
   -- Arguments and Files --
   -------------------------
   loop
      begin
         case GNAT.Command_Line.Getopt ("* - client server types help -help")
         is
            when 'c' =>
               Mode := Client;
            when 's' =>
               Mode := Server;
            when 't' =>
               Mode := Types;
            when '-' =>
               --  Stop taking arguments
               if GNAT.Command_Line.Full_Switch = "-" then
                  exit;
               elsif GNAT.Command_Line.Full_Switch = "-help" then
                  Show_Help;
               end if;
            when 'h' =>
               Show_Help;
            when '*' =>
               File_List.Append (GNAT.Command_Line.Full_Switch);
            when ASCII.NUL =>
               exit;
            when others =>
               null;
         end case;
      end;
   end loop;

   ----------------
   -- Files Only --
   ----------------
   loop
      case GNAT.Command_Line.Getopt ("*") is
         when '*' =>
            File_List.Append (GNAT.Command_Line.Full_Switch);
         when ASCII.NUL =>
            exit;
         when others =>
            null;
      end case;
   end loop;

   ------------
   -- Checks --
   ------------
   if File_List.Is_Empty then
      GNAT.Command_Line.Try_Help;
      Error_Message ("No input files specified");
   end if;

   --  Check that input files exist
   declare
      use type Ada.Directories.File_Kind;
   begin
      for File of File_List loop
         if Ada.Directories.Exists (File)
           and then Ada.Directories.Kind (File) = Ada.Directories.Ordinary_File
         then
            null;
         else
            Error_Message
              ("Input file " & File &
               " does not exist or is not an ordinary file.");
         end if;
      end loop;
   end;

   -----------------
   -- Load schema --
   -----------------
   declare
      Input  : Input_Sources.File.File_Input;
      Reader : Schema.Schema_Readers.Schema_Reader;
   begin
      Input_Sources.File.Open (Introspect_Schema, Input);
      Reader.Parse (Input);
      Input.Close;
      Grammar := Reader.Get_Grammar;
      Reader.Free;
   exception
      when Schema.Validators.XML_Validation_Error =>
         Error_Message ("SCHEMA ERROR: " & Reader.Get_Error_Message);

   end;
   Put_Debug ("Loaded grammar");

   --------------------------------
   -- Parse all files into nodes --
   --------------------------------
   declare
      Input    : Input_Sources.File.File_Input;
      Reader   : Schema.Dom_Readers.Tree_Reader;
      Document : DOM.Core.Document;

      function Parse_File (Name : String) return Parsing.Node_Type;
      function Parse_File (Name : String) return Parsing.Node_Type is
         Result : Parsing.Node_Type;
      begin
         -----------------------------------
         -- Load document from input file --
         -----------------------------------
         declare
         begin
            Input_Sources.File.Open (Name, Input);
            Reader.Parse (Input);
            Input.Close;
            Document := Reader.Get_Tree;
            Reader.Free;
         exception
            when Schema.Validators.XML_Validation_Error =>
               Error_Message ("SPEC ERROR: " & Reader.Get_Error_Message);
         end;
         Put_Debug ("Loaded tree");

         ----------------------
         -- Process document --
         ----------------------
         Result :=
           Parsing.Process_Node (DOM.Core.Documents.Get_Element (Document));
         DOM.Core.Nodes.Free (Document);
         Put_Debug ("Parsed nodes for " & Name);

         return Result;
      end Parse_File;
   begin
      Reader.Set_Grammar (Grammar);
      Reader.Set_Feature (Sax.Readers.Schema_Validation_Feature, True);

      for File of File_List loop
         Node_List.Append (Parse_File (File));
      end loop;
   end;

   -----------------
   -- Check nodes --
   -----------------
   declare
      procedure Recurse_Node (Node : in out Parsing.Node_Type);
      procedure Recurse_Node (Node : in out Parsing.Node_Type) is
         Has_Properties_Interface : Boolean := False;
      begin
         for Child_Node of Node.Child_Nodes loop
            Recurse_Node (Child_Node.all);
         end loop;

         --  Search for a properties interface
         for I of Node.Interfaces loop
            if +I.Name = Constants.Properties_Interface then
               Has_Properties_Interface := True;
               exit;
            end if;
         end loop;

         --  Ensure no interface has properties defined if
         --  there is no properties interface
         if not Has_Properties_Interface then
            for I of Node.Interfaces loop
               if not I.Properties.Is_Empty then
                  Error_Message
                    ("CHECK ERROR: Interface " & (+I.Name) &
                     " has properties, but its parent node" &
                     " does not implement " & Constants.Properties_Interface);
               end if;
            end loop;
         end if;
      end Recurse_Node;
   begin
      for Top_Level_Node of Node_List loop
         Recurse_Node (Top_Level_Node);
      end loop;
   end;
   Put_Debug ("All nodes checked and semantically valid");

   -------------------
   -- Generate code --
   -------------------
   declare
      Pkgs : Codegen.Maps.Ada_Package_Map;
      --  All declared interfaces as Ada packages

      procedure Recurse_Node (Node : in out Parsing.Node_Type);
      procedure Recurse_Node (Node : in out Parsing.Node_Type) is
      begin
         for Child_Node of Node.Child_Nodes loop
            Recurse_Node (Child_Node.all);
            Parsing.Free (Child_Node);
         end loop;

         for I of Node.Interfaces loop
            if not Pkgs.Contains (I.Name) then
               Pkgs.Insert (I.Name, Codegen.Create_Package (I));
            else
               Put_Debug ("Duplicate interface " & (+I.Name));
            end if;
         end loop;

         Put_Debug
           ("Created packages for node " & (+Node.Name) & "'s interfaces");
      end Recurse_Node;
   begin
      --  Collect all packages / interfaces
      for Top_Level_Node of Node_List loop
         Recurse_Node (Top_Level_Node);
      end loop;

      --  Generate client code, server code, or types
      case Mode is
         when Client =>
            for Pkg of Pkgs loop
               Codegen.Client.Iface.Print (Pkg);
            end loop;
         when Server =>
            for Pkg of Pkgs loop
               Codegen.Server.Iface.Print (Pkg);
            end loop;

            Codegen.Server.Objects.Print (Pkgs);
         when Types =>
            declare
               Types : Codegen.Types.Ada_Type_Declaration_Map;
            begin
               for Pkg of Pkgs loop
                  Codegen.Types.Print_Neutral (Pkg);
                  Codegen.Types.Add_Types (Types, Pkg);
               end loop;

               Codegen.Types.Print (Types);
            end;
      end case;

      Put_Debug ("Generated code");
   end;
end DBus_Binding_Generator_Ada;
