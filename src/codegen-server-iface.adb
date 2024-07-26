pragma Ada_2012;

with Codegen.Output; use Codegen.Output;
with Codegen.Output.Subprograms; use Codegen.Output.Subprograms;

with Shared; use Shared;

package body Codegen.Server.Iface is
   ----------------
   -- Print_Spec --
   ----------------
   procedure Print_Spec (Pkg : Ada_Package_Type);
   procedure Print_Spec (Pkg : Ada_Package_Type) is
   begin
      --  Preamble
      Use_Pragma ("Ada_2005");
      New_Line;

      With_Entity ("D_Bus.Support");
      --  `Unbounded_Object_Path`, `Unbounded_Signature`
      With_Entity ("D_Bus.Support.Server");
      --  `Server_Interface` and associated methods
      With_Entity ("D_Bus.Generated_Types");
      Use_Entity ("D_Bus.Generated_Types");
      --  All generated types
      New_Line;

      --!pp off
      Start_Package (+Pkg.Name);
         --  Interface Type
         --  Note: This _should_ extend `org.freedesktop.DBus.Properties` but
         --  we choose not to to reduce the size of the closure. This is
         --  most likely irrelevant as Server_Interface provides the same
         --  methods as `org.freedesktop.DBus.Properties`
         Declare_Type
           ("Child_Interface",
            "limited interface and D_Bus.Support.Server.Server_Interface");
         New_Line;

         --  Methods
         if not Pkg.Methods.Is_Empty then
            Large_Comment ("Methods");
            for M of Pkg.Methods loop
               Declare_Null_Procedure (Method_Signature (M));
               New_Line;
            end loop;
         end if;

         --  Signals
         if not Pkg.Signals.Is_Empty then
            Large_Comment ("Signals");
            for S of Pkg.Signals loop
               Declare_Procedure (Signal_Signature (S));
               New_Line;
            end loop;
         end if;

         --  Properties
         if not Pkg.Properties.Is_Empty then
            Large_Comment ("Properties");
            for P of Pkg.Properties loop
               Declare_Function (Property_Read_Signature (P));
               Declare_Function (Property_Write_Signature (P));
               New_Line;
            end loop;
         end if;
      End_Package (+Pkg.Name);
      --!pp on
   end Print_Spec;

   ----------------
   -- Print_Body --
   ----------------
   procedure Print_Body (Pkg : Ada_Package_Type) is null;

   -----------
   -- Print --
   -----------
   procedure Print (Pkg : Ada_Package_Type) is
   begin
      Print_Spec (Pkg);
      Print_Body (Pkg);
   end Print;
end Codegen.Server.Iface;
