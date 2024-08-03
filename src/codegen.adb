pragma Ada_2022;

with Codegen.Output;

with Parsing; use Parsing;

with Shared; use Shared;

package body Codegen is
   --  Ensure that all Method parameters are named
   procedure Name_Parameters (M : in out Method_Type);
   procedure Name_Parameters (M : in out Method_Type) is
      use Ada.Strings.Unbounded;

      FI : constant Positive := M.Arguments.First_Index;
      LI : constant Natural  := M.Arguments.Last_Index;
   begin
      for I in FI .. LI loop
         M.Arguments (I).Name :=
           +Codegen.Output.Sanitise_Name (+M.Arguments (I).Name);

         --  Give a unique name to unnamed parameters
         if M.Arguments (I).Name = Null_Unbounded_String then
            M.Arguments (I).Name :=
              +("Parameter_" & I'Image (I'Image'First + 1 .. I'Image'Last));
         end if;
      end loop;
   end Name_Parameters;

   --------------------
   -- Create_Package --
   --------------------
   function Create_Package (I : Interface_Type) return Ada_Package_Type is
      use Ada.Strings.Unbounded;

      Pkg : Ada_Package_Type;
   begin
      Append (Pkg.Name, Codegen.Output.Sanitise_Name (+I.Name));

      Pkg.Real_Name := I.Name;

      -----------------
      -- Subprograms --
      -----------------
      for M of I.Methods loop
         declare
            M2 : Method_Type := M;
         begin
            Name_Parameters (M2);
            Pkg.Methods.Append (M2);
         end;
      end loop;

      for S of I.Signals loop
         declare
            S2 : Method_Type := S;
         begin
            Name_Parameters (S2);
            Pkg.Signals.Append (S2);
         end;
      end loop;

      Pkg.Properties  := I.Properties;
      Pkg.Annotations := I.Annotations;

      return Pkg;
   end Create_Package;
end Codegen;
