with Ada.Strings.Unbounded;

with Shared; use Shared;
with Debug; use Debug;

package body Type_Checking is
   --------------
   -- Is_Basic --
   --------------
   function Is_Basic (T : String) return Boolean is
     (T (T'First) in
        'y' | 'b' | 'n' | 'q' | 'i' | 'u' | 'x' | 't' | 'd' | 's' | 'o' | 'g'
        | 'h');

   function Is_Stringlike (T : String) return Boolean is
     (T (T'First) in 's' | 'o' | 'g');

   --  Sanitise a type name to be suitable for Ada type names
   --  Not exported
   function Sanitise (T : String) return String;
   function Sanitise (T : String) return String is
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
            when others =>
               raise Program_Error;
         end case;
      end loop;

      return +Buf;
   end Sanitise;

   ------------------
   -- Get_Interior --
   ------------------
   function Get_Interior (DType : String) return String is
   begin
      case DType (DType'First) is
         when 'a' =>
            --  Check for dicts
            case DType (DType'First + 1) is
               when '{' =>
                  return DType (DType'First + 2 .. DType'Last - 1);
               when others =>
                  return DType (DType'First + 1 .. DType'Last);
            end case;
         when '(' =>
            return DType (DType'First + 1 .. DType'Last - 1);
         when others =>
            return DType;
      end case;
   end Get_Interior;

   -----------------------
   -- Get_Complete_Type --
   -----------------------
   function Get_Complete_Type (T : String; Index : Positive := 1) return String
   is
      use Ada.Strings.Unbounded;

      Buf   : Unbounded_String;
      Count : Natural := 0;

      --  Return the full length of a complete inner type
      --  (as) -> 4
      --  a{tvs} => 6
      function Inner (T : String) return Positive;
      function Inner (T : String) return Positive is
      begin
         case T (T'First) is
            --  Recursive array handler
            when 'a' =>
               case T (T'First + 1) is
                  when '(' | '{' =>
                     return Inner (T (T'First + 1 .. T'Last)) + 1;
                  when others => return 2;
               end case;

            --  Search for terminating ')'
            when '(' =>
               Struct_Depth_Search :
                  declare
                     Depth : Natural := 0;
                  begin
                     for I in T'Range loop
                        case T (I) is
                           when '(' => Depth := Depth + 1;
                           when ')' => Depth := Depth - 1;
                           when others => null;
                        end case;

                        --  Return the length of the struct
                        if Depth = 0 then
                           return I - T'First + 1;
                        end if;
                     end loop;

                     raise Program_Error with "Invalid struct";
                  end Struct_Depth_Search;

            --  Idem, but for '}'
            when '{' =>
               Dict_Depth_Search :
                  declare
                     Depth : Natural := 0;
                  begin
                     for I in T'Range loop
                        case T (I) is
                           when '{' => Depth := Depth + 1;
                           when '}' => Depth := Depth - 1;
                           when others => null;
                        end case;

                        --  Return the length of the dict
                        if Depth = 0 then
                           return I - T'First + 1;
                        end if;
                     end loop;
                  end Dict_Depth_Search;

                  raise Program_Error with "Invalid dict";
            when others =>
               raise Program_Error with "Inner called on simple type";
         end case;
      end Inner;

      I : Natural := T'First;
   begin
      Put_Debug ("Get_Complete_Type: " & T & "," & Index'Image);

      while I <= T'Last loop
         case T (I) is
            --  Complex Container
            when 'a' | '(' =>
               declare
                  Container_Length : constant Positive :=
                    Inner (T (I .. T'Last));
               begin
                  Count := Count + 1;

                  if Count = Index then
                     Append (Buf, T (I .. I + Container_Length - 1));
                     Put_Debug ("Complete_Type: " & (+Buf));
                     return +Buf;
                  end if;

                  I := I + Container_Length - 1;
               end;

            --  Invalid
            when ')' | '{' | '}' =>
               raise Program_Error with "Invalid signature";

            --  Basic Type / Simple Container Type
            when others =>
               Count := Count + 1;

               if Count = Index then
                  Put_Debug ("Complete_Type: " & T (I));
                  return (1 => T (I));
               end if;
         end case;

         I := I + 1;
      end loop;

      --  Failed to find the Index'th complete type
      raise No_More_Complete_Types;
   end Get_Complete_Type;

   ------------------
   -- Get_Ada_Type --
   ------------------
   function Get_Ada_Type (T : String) return String is
      AType_First : constant Character := T (T'First);
   begin
      case AType_First is
         when 'y' =>
            return "Interfaces.Unsigned_8";
         when 'b' =>
            return "Boolean";
         when 'n' =>
            return "Interfaces.Integer_16";
         when 'q' =>
            return "Interfaces.Unsigned_16";
         when 'i' =>
            return "Interfaces.Integer_32";
         when 'u' =>
            return "Interfaces.Unsigned_32";
         when 'x' =>
            return "Interfaces.Integer_64";
         when 't' =>
            return "Interfaces.Unsigned_64";
         when 'd' =>
            return "Interfaces.IEEE_Float_64";
         when 's' =>
            return "Ada.Strings.Unbounded.Unbounded_String";
         when 'o' =>
            return "D_Bus.Support.Unbounded_Object_Path";
         when 'g' =>
            return "D_Bus.Support.Unbounded_Signature";
         when 'a' =>
            --  Check for dicts
            case T (T'First + 1) is
               when '{' =>
                  return "Dict_" & Sanitise (Get_Interior (T));
               when others =>
                  return "Array_" & Sanitise (Get_Interior (T));
            end case;
         when '(' =>
            return "Struct_" & Sanitise (Get_Interior (T));
         when 'v' =>
            return "D_Bus.Arguments.Containers.Variant_Type";
         when 'h' =>
            return "GNAT.OS_Lib.File_Descriptor";
         when others =>
            raise Program_Error;
      end case;
   end Get_Ada_Type;

   ---------------------------
   -- Get_Library_DBus_Type --
   ---------------------------
   function Get_Library_DBus_Type (T : String) return String is
      AType_First : constant Character := T (T'First);
   begin
      case AType_First is
         when 'y' =>
            return "D_Bus.Arguments.Basic.Byte_Type";
         when 'b' =>
            return "D_Bus.Arguments.Basic.Boolean_Type";
         when 'n' =>
            return "D_Bus.Arguments.Basic.Int16_Type";
         when 'q' =>
            return "D_Bus.Arguments.Basic.U_Int16_Type";
         when 'i' =>
            return "D_Bus.Arguments.Basic.Int32_Type";
         when 'u' =>
            return "D_Bus.Arguments.Basic.U_Int32_Type";
         when 'x' =>
            return "D_Bus.Arguments.Basic.Int64_Type";
         when 't' =>
            return "D_Bus.Arguments.Basic.U_Int64_Type";
         when 'd' =>
            return "D_Bus.Extra.Double_Type";
         when 's' =>
            return "D_Bus.Arguments.Basic.String_Type";
         when 'o' =>
            return "D_Bus.Arguments.Basic.Object_Path_Type";
         when 'g' =>
            return "D_Bus.Extra.Signature_Type";
         when 'a' =>
            return "D_Bus.Arguments.Containers.Array_Type";
         when '(' =>
            return "D_Bus.Arguments.Containers.Struct_Type";
         when 'v' =>
            return "D_Bus.Arguments.Containers.Variant_Type";
         when 'h' =>
            return "D_Bus.Extra.File_Descriptor_Type";
         when others =>
            raise Program_Error;
      end case;
   end Get_Library_DBus_Type;

   --------------------------
   -- Get_Library_Ada_Type --
   --------------------------
   function Get_Library_Ada_Type (T : String) return String is
      AType_First : constant Character := T (T'First);
   begin
      if not Is_Basic (T) then
         raise Program_Error with T & " is not a basic type";
      end if;
      --  ONLY for Basic Types
      case AType_First is
         when 'y' =>
            return "D_Bus.Byte";
         when 'b' =>
            return "Boolean";
         when 'n' =>
            return "D_Bus.Signed_16";
         when 'q' =>
            return "D_Bus.Unsigned_16";
         when 'i' =>
            return "D_Bus.Signed_32";
         when 'u' =>
            return "D_Bus.Unsigned_32";
         when 'x' =>
            return "D_Bus.Signed_64";
         when 't' =>
            return "D_Bus.Unsigned_64";
         when 'd' =>
            return "D_Bus.Extra.Double";
         when 's' =>
            return "String";
         when 'o' =>
            return "D_Bus.Types.Obj_Path";
         when 'g' =>
            return "D_Bus.Extra.Signature";
         when 'h' =>
            return "D_Bus.Extra.File_Descriptor";
         when others =>
            raise Program_Error;
      end case;
   end Get_Library_Ada_Type;
end Type_Checking;
