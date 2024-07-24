package Codegen.Output.Client is
   -------------
   -- Methods --
   -------------
   function Method_Name (M : Parsing.Method_Type) return String;
   function Method_Signature (M : Parsing.Method_Type) return String;

   -------------
   -- Signals --
   -------------
   function Signal_Id_Name (S : Parsing.Signal_Type) return String;
   function Signal_Register_Name (S : Parsing.Signal_Type) return String;
   function Signal_Register_Signature
     (S : Parsing.Signal_Type) return String;
   function Signal_Unregister_Name (S : Parsing.Signal_Type) return String;
   function Signal_Unregister_Signature
     (S : Parsing.Signal_Type) return String;
   function Signal_Await_Name (S : Parsing.Signal_Type) return String;
   function Signal_Await_Signature
     (S : Parsing.Signal_Type) return String;

   ----------------
   -- Properties --
   ----------------
   function Property_Read_Name (P : Parsing.Property_Type) return String;
   function Property_Read_Signature (P : Parsing.Property_Type) return String;

   function Property_Write_Name (P : Parsing.Property_Type) return String;
   function Property_Write_Signature (P : Parsing.Property_Type) return String;
end Codegen.Output.Client;
