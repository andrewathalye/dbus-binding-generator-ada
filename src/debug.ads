--  with Ada.Text_IO;

package Debug is
   procedure Put_Debug (Item : String) is null;
   --   procedure Put_Debug (Item : String) renames Ada.Text_IO.Put_Line;
end Debug;
