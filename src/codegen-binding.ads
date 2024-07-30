with Codegen.Types;

with Signatures.Unbounded;

package Codegen.Binding is
   procedure Bind_To_DBus
     (Types     : Codegen.Types.Ada_Type_Declaration_Map;
      Type_Code : Signatures.Unbounded.Unbounded_Signature; Ada_Name : String;
      DBus_Name : String);
   --  Bind the existing Ada entity `Ada_Name` to the existing
   --  DBus entity `DBus_Name`.

   procedure Bind_To_Ada
     (Types     : Codegen.Types.Ada_Type_Declaration_Map;
      Type_Code : Signatures.Unbounded.Unbounded_Signature; DBus_Name : String;
      Ada_Name  : String);
   --  Bind the existing DBus entity `DBus_Name` to the existing
   --  Ada entity `Ada_Name`
end Codegen.Binding;
