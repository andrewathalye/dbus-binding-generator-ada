package Type_Checking is
   pragma Preelaborate (Type_Checking);

   Object_Path : constant String := "o";

   function Is_Basic (T : String) return Boolean;
   --  Check whether a DBus type is basic (per spec)

   function Is_Stringlike (T : String) return Boolean;
   --  Check whether a DBus type is a stringlike (per spec)

   function Get_Interior (DType : String) return String;
   --  Get the type(s) contained within a container

   No_More_Complete_Types : exception;
   function Get_Complete_Type
     (T : String; Index : Positive := 1) return String;
   --  Returns the complete type at index `Index` within T

   function Get_Ada_Type (T : String) return String;
   --  Generate a consistent Ada name for a DBus type

   function Get_Library_DBus_Type (T : String) return String;
   --  Get the D_Bus-Ada type name for a DBus type on the wire

   function Get_Library_Ada_Type (T : String) return String;
   --  Get the D_Bus-Ada type name for a DBus basic type
end Type_Checking;
