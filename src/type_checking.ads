package Type_Checking is
   pragma Preelaborate (Type_Checking);

   function Is_Basic (T : String) return Boolean;
   --  Check whether a DBus type is basic (per spec)

   function Sanitise (T : String) return String;
   --  Sanitise a DBus type string to be Ada-compatible

   function Get_Interior (DType : String) return String;
   --  Get the type(s) contained within a container

   No_More_Complete_Types : exception;
   function Get_Complete_Type
     (T : String;
      Index : Positive := 1) return String;
   --  Returns the complete type at index `Index` within T

   function Get_Ada_Type (T : String) return String;
   --  Generate a consistent Ada name for a DBus type
end Type_Checking;
