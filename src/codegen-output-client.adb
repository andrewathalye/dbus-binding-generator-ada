with Type_Checking;
with Shared; use Shared;

package body Codegen.Output.Client is

   -----------------
   -- Method_Name --
   -----------------

   function Method_Name (M : Parsing.Method_Type) return String is
   begin
      return Sanitise_Name (+M.Name);
   end Method_Name;

   ----------------------
   -- Method_Signature --
   ----------------------

   function Method_Signature (M : Parsing.Method_Type) return String is
   begin
      return Method_Name (M) & " " & Get_Arguments
        (M.Arguments, Client => True);
   end Method_Signature;

   -----------------
   -- Signal_Name --
   -----------------

   function Signal_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Await_" & Sanitise_Name (+S.Name);
   end Signal_Name;

   ----------------------
   -- Signal_Signature --
   ----------------------

   function Signal_Signature (S : Parsing.Signal_Type) return String is
   begin
      return Signal_Name (S) & " " & Get_Arguments
        (S.Arguments, Client => True);
   end Signal_Signature;

   ------------------------
   -- Property_Read_Name --
   ------------------------

   function Property_Read_Name (P : Parsing.Property_Type) return String is
   begin
      return Sanitise_Name (+P.Name);
   end Property_Read_Name;

   -----------------------------
   -- Property_Read_Signature --
   -----------------------------

   function Property_Read_Signature (P : Parsing.Property_Type) return String
   is
   begin
      return Property_Read_Name (P) &
         " return " & Type_Checking.Get_Ada_Type (+P.Type_Code);
   end Property_Read_Signature;

   -------------------------
   -- Property_Write_Name --
   -------------------------

   function Property_Write_Name (P : Parsing.Property_Type) return String is
   begin
      return "Set_" & Sanitise_Name (+P.Name);
   end Property_Write_Name;

   ------------------------------
   -- Property_Write_Signature --
   ------------------------------

   function Property_Write_Signature (P : Parsing.Property_Type) return String
   is
   begin
      return Property_Write_Name (P) & " (Value : " &
         Type_Checking.Get_Ada_Type (+P.Type_Code);
   end Property_Write_Signature;
end Codegen.Output.Client;
