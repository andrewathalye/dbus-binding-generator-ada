package Codegen.Output.Client is
   --  Note: All names produced are pre-sanitised

   function Method_Name (M : Parsing.Method_Type) return String;
   function Method_Signature (M : Parsing.Method_Type) return String;

   function Signal_Name (S : Parsing.Signal_Type) return String;
   function Signal_Signature (S : Parsing.Signal_Type) return String;

   function Property_Read_Name (P : Parsing.Property_Type) return String;
   function Property_Read_Signature (P : Parsing.Property_Type) return String;

   function Property_Write_Name (P : Parsing.Property_Type) return String;
   function Property_Write_Signature (P : Parsing.Property_Type) return String;
end Codegen.Output.Client;
