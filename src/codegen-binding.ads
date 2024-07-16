package Codegen.Binding is
   procedure Bind_To_DBus
     (Pkg : Ada_Package_Type; TD : Ada_Type_Declaration; Ada_Name : String;
      DBus_Name : String);
   --  Bind the existing Ada entity `Ada_Name` to the existing
   --  DBus entity `DBus_Name`
   --  based upon the given `Ada_Type_Declaration`.

   procedure Bind_To_Ada
     (Pkg : Ada_Package_Type; TD : Ada_Type_Declaration; DBus_Name : String;
      Ada_Name : String);
   --  Bind the existing DBus entity `DBus_Name` to the existing
   --  Ada entity `Ada_Name`
   --  based upon the given `Ada_Type_Declaration`.
end Codegen.Binding;
