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
      return
        Method_Name (M) & " " & Get_Arguments (M.Arguments, Client => True);
   end Method_Signature;

   -----------------
   -- Signal_Name --
   -----------------
   function Signal_Name (S : Parsing.Signal_Type) return String;
   function Signal_Name (S : Parsing.Signal_Type) return String is
   begin
      return Sanitise_Name (+S.Name);
   end Signal_Name;

   function Signal_Id_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Id_" & Signal_Name (S);
   end Signal_Id_Name;

   --------------------------
   -- Signal_Register_Name --
   --------------------------
   function Signal_Register_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Register_" & Signal_Name (S);
   end Signal_Register_Name;

   -------------------------------
   -- Signal_Register_Signature --
   -------------------------------
   function Signal_Register_Signature
     (S : Parsing.Signal_Type) return String
   is (Signal_Register_Name (S) &
       "(Node : D_Bus.Support.Unbounded_Object_Path :=" &
        " D_Bus.Support.Null_Unbounded_Object_Path)");

   ----------------------------
   -- Signal_Unregister_Name --
   ----------------------------
   function Signal_Unregister_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Unregister_" & Signal_Name (S);
   end Signal_Unregister_Name;

   ---------------------------------
   -- Signal_Unregister_Signature --
   ---------------------------------
   function Signal_Unregister_Signature
     (S : Parsing.Signal_Type) return String
   is (Signal_Unregister_Name (S));

   -----------------------
   -- Signal_Await_Name --
   -----------------------
   function Signal_Await_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Await_" & Signal_Name (S);
   end Signal_Await_Name;

   ----------------------------
   -- Signal_Await_Signature --
   ----------------------------
   function Signal_Await_Signature (S : Parsing.Signal_Type) return String is
   begin
      return
        Signal_Await_Name (S) & " " &
        Get_Arguments (S.Arguments, Client => True);
   end Signal_Await_Signature;

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
      return
        Property_Read_Name (P) & " return " &
        Type_Checking.Get_Ada_Type (+P.Type_Code);
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
      return
        Property_Write_Name (P) & " (Value : " &
        Type_Checking.Get_Ada_Type (+P.Type_Code) & ")";
   end Property_Write_Signature;
end Codegen.Output.Client;
