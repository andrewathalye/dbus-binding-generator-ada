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

   --------------
   -- Sanitise --
   --------------
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
         when 's' => return "Interfaces.Unbounded_String";
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
         when 'h' => return "System.OS_Lib.File_Descriptor";
         when others => raise Program_Error;
      end case;
   end Get_Ada_Type;
end Type_Checking;
