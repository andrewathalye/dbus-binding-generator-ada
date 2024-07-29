package Codegen.Output.Subprograms is
   ----------------
   -- Interfaces --
   ----------------
   --  Serverside
   function Interface_Handler_Name (Pkg : Ada_Package_Type) return String;
   function Interface_Handler_Signature (Pkg : Ada_Package_Type) return String;

   -------------
   -- Methods --
   -------------
   function Method_Name (M : Parsing.Method_Type) return String;
   function Method_Signature (M : Parsing.Method_Type) return String;

   --  Serverside
   function Unbound_Method_Signature (M : Parsing.Method_Type) return String;
   --  The method signature of a null or abstract method.

   function Method_Call_Expression (M : Parsing.Method_Type) return String;
   function Method_Handler_Name (M : Parsing.Method_Type) return String;
   function Method_Handler_Signature
     (Pkg : Ada_Package_Type; M : Parsing.Method_Type) return String;

   -------------
   -- Signals --
   -------------
   --  Clientside
   function Signal_Register_Name (S : Parsing.Signal_Type) return String;
   function Signal_Register_Signature (S : Parsing.Signal_Type) return String;

   function Signal_Unregister_Name (S : Parsing.Signal_Type) return String;
   function Signal_Unregister_Signature
     (S : Parsing.Signal_Type) return String;

   function Signal_Await_Name (S : Parsing.Signal_Type) return String;
   function Signal_Await_Signature (S : Parsing.Signal_Type) return String;

   --  Serverside
   function Signal_Name (S : Parsing.Signal_Type) return String;
   function Signal_Signature (S : Parsing.Signal_Type) return String;

   ----------------
   -- Properties --
   ----------------
   function Property_Read_Name (P : Parsing.Property_Type) return String;
   function Property_Read_Signature (P : Parsing.Property_Type) return String;

   function Property_Write_Name (P : Parsing.Property_Type) return String;
   function Property_Write_Signature (P : Parsing.Property_Type) return String;

   -------------
   -- Binding --
   -------------
   function Bind_To_Ada_Name return String;
   function Bind_To_Ada_Signature (TD : Ada_Type_Declaration) return String;

   function Bind_To_DBus_Name return String;
   function Bind_To_DBus_Signature (TD : Ada_Type_Declaration) return String;
end Codegen.Output.Subprograms;
