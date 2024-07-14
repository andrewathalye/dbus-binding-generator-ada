with Ada.Strings.Unbounded;

with Shared; use Shared;

package body Type_Checking is
   --  Ensure a DBus type is valid
   procedure Validate_Type (T : String);
   procedure Validate_Type (T : String) is
   begin
      --  Excluding '{' since it can’t start a type
      if not (T (T'First) in 'y' | 'b' | 'n' | 'q' | 'i' | 'u' | 'x' | 't'
         | 'd' | 's' | 'o' | 'g' | 'a' | '(' | 'v' | 'h')
      then
         raise Program_Error with "Invalid type: " & T;
      end if;
   end Validate_Type;

   --------------
   -- Is_Basic --
   --------------
   function Is_Basic (T : String) return Boolean
   is (T (T'First) in 'y' | 'b' | 'n' | 'q' | 'i' | 'u' | 'x' | 't' | 'd'
      | 's' | 'o' | 'g' | 'h');

   --  Sanitise a type name to be suitable for Ada type names
   --  Not exported
   function Sanitise (T : String) return String;
   function Sanitise (T : String) return String
   is
      use Ada.Strings.Unbounded;

      Buf : Unbounded_String;
   begin
      for C of T loop
         case C is
            when 'A' .. 'z' =>
               Append (Buf, C);
            when '(' | ')' =>
               Append (Buf, 'r');
            when '{' | '}' =>
               Append (Buf, 'e');
            when others => raise Program_Error;
         end case;
      end loop;

      return +Buf;
   end Sanitise;

   ------------------
   -- Get_Interior --
   ------------------
   function Get_Interior (DType : String) return String
   is
   begin
      case DType (DType'First) is
         when 'a' =>
            --  Check for dicts
            case DType (DType'First + 1) is
               when '{' => return DType (DType'First + 2 .. DType'Last - 1);
               when others => return DType (DType'First + 1 .. DType'Last);
            end case;
         when '(' =>
            return DType (DType'First + 1 .. DType'Last - 1);
         when others => return DType;
      end case;
   end Get_Interior;

   -----------------------
   -- Get_Complete_Type --
   -----------------------
   function Get_Complete_Type
     (T : String;
      Index : Positive := 1) return String
   is
      use Ada.Strings.Unbounded;

      Buf : Unbounded_String;
      Count : Natural := 0;
   begin
      Main_Loop :
         for I in T'Range loop
            case T (I) is
               --  Complex types
               when 'a' =>
                  case T (I + 1) is
                     when '{' => --  Return the full a{...}
                        Find_Dict :
                           for I2 in I .. T'Last loop
                              if T (I2) = '}' then
                                 Buf := +(T (I .. I2));
                                 exit Find_Dict;
                              end if;
                           end loop Find_Dict;
                     when others => -- Return a...
                        Buf := Null_Unbounded_String;
                        Append (Buf, 'a');
                        Append (Buf, Get_Complete_Type (T (I + 1 .. T'Last)));
                  end case;
               when '(' => --  Return the full (...)
                  Find_Struct :
                     for I2 in I .. T'Last loop
                        if T (I2) = ')' then
                           Buf := +(T (I .. I2));
                           exit Find_Struct;
                        end if;
                     end loop Find_Struct;
               when others =>
                  Buf := Null_Unbounded_String;
                  Append (Buf, T (I));
            end case;

            Count := Count + 1;
            if Count = Index then
               return +Buf;
            end if;
         end loop Main_Loop;
      return raise No_More_Complete_Types with T & Index'Image;
   end Get_Complete_Type;

   ------------------
   -- Get_Ada_Type --
   ------------------
   function Get_Ada_Type (T : String) return String
   is
      AType_First : constant Character := T (T'First);
   begin
      Validate_Type (T);

      case AType_First is
         when 'y' => return "Interfaces.Unsigned_8";
         when 'b' => return "Boolean";
         when 'n' => return "Interfaces.Integer_16";
         when 'q' => return "Interfaces.Unsigned_16";
         when 'i' => return "Interfaces.Integer_32";
         when 'u' => return "Interfaces.Unsigned_32";
         when 'x' => return "Interfaces.Integer_64";
         when 't' => return "Interfaces.Unsigned_64";
         when 'd' => return "Interfaces.Float_64";
         when 's' => return "Ada.Strings.Unbounded.Unbounded_String";
         when 'o' => return "Object_Path";
         when 'g' => return "Signature_Type";
         when 'a' =>
            --  Check for dicts
            case T (T'First + 1) is
               when '{' =>
                  return "Dict_" & Sanitise (Get_Interior (T));
               when others =>
                  return "Array_" & Sanitise (Get_Interior (T));
            end case;
         when '(' => return "Struct_" & Sanitise (Get_Interior (T));
         when 'v' => return "D_Bus.Arguments.Containers.Variant_Type";
         when 'h' => return "GNAT.OS_Lib.File_Descriptor";
         when others => raise Program_Error;
      end case;
   end Get_Ada_Type;

   ---------------------------
   -- Get_Library_DBus_Type --
   ---------------------------
   function Get_Library_DBus_Type (T : String) return String
   is
      AType_First : constant Character := T (T'First);
   begin
      Validate_Type (T);

      case AType_First is
         when 'y' => return "D_Bus.Arguments.Basic.Byte_Type";
         when 'b' => return "D_Bus.Arguments.Basic.Boolean_Type";
         when 'n' => return "D_Bus.Arguments.Basic.Int16_Type";
         when 'q' => return "D_Bus.Arguments.Basic.U_Int16_Type";
         when 'i' => return "D_Bus.Arguments.Basic.Int32_Type";
         when 'u' => return "D_Bus.Arguments.Basic.U_Int32_Type";
         when 'x' => return "D_Bus.Arguments.Basic.Int64_Type";
         when 't' => return "D_Bus.Arguments.Basic.U_Int64_Type";
         when 'd' => return "D_Bus.Arguments.Basic.Double_Type";
         --  TODO this isn’t going to work (D_Bus-Ada has no doubles)
         when 's' => return "D_Bus.Arguments.Basic.String_Type";
         when 'o' => return "D_Bus.Arguments.Basic.Object_Path_Type";
         when 'g' => return "D_Bus.Arguments.Basic.String_Type";
         when 'a' =>
            --  Check for dicts
            case T (T'First + 1) is
               when '{' =>
                  return "D_Bus.Arguments.Containers.Dict_Type";
               when others =>
                  return "D_Bus.Arguments.Containers.Array_Type";
            end case;
         when '(' => return "D_Bus.Arguments.Containers.Struct_Type";
         when 'v' => return "D_Bus.Arguments.Containers.Variant_Type";
         when 'h' => return "D_Bus.Arguments.Basic.File_Descriptor_Type";
         --  TODO this isn’t going to work (D_Bus-Ada has no fd type)
         when others => raise Program_Error;
      end case;
   end Get_Library_DBus_Type;

   --------------------------
   -- Get_Library_Ada_Type --
   --------------------------
   function Get_Library_Ada_Type (T : String) return String
   is
      AType_First : constant Character := T (T'First);
   begin
      if not Is_Basic (T) then
         raise Program_Error with T & " is not a basic type";
      end if;
      --  ONLY for Basic Types
      case AType_First is
         when 'y' => return "D_Bus.Byte";
         when 'b' => return "Boolean";
         when 'n' => return "D_Bus.Signed_16";
         when 'q' => return "D_Bus.Unsigned_16";
         when 'i' => return "D_Bus.Signed_32";
         when 'u' => return "D_Bus.Unsigned_32";
         when 'x' => return "D_Bus.Signed_64";
         when 't' => return "D_Bus.Unsigned_64";
         when 'd' => return "D_Bus.Double";
         --  TODO this isn’t going to work (D_Bus-Ada has no doubles)
         when 's' => return "String";
         when 'o' => return "D_Bus.Types.Obj_Path";
         when 'g' => return "String";
         when 'h' => return "D_Bus.File_Descriptor_Type";
         --  TODO this isn’t going to work (D_Bus-Ada has no fd type)
         when others => raise Program_Error;
      end case;
   end Get_Library_Ada_Type;
end Type_Checking;
