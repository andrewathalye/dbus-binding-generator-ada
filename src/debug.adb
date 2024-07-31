--  with System;

package body Debug is
   procedure Put_Debug (Item : String) is
   --      procedure puts (Item : System.Address);
   --      pragma Import (C, puts);
   --
   --      Alias : aliased String := Item & ASCII.NUL;
   begin
      null;
      --      puts (Alias'Address);
   end Put_Debug;
end Debug;
