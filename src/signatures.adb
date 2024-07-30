pragma Ada_2012;

package body Signatures is
   --------------
   -- Is_Basic --
   --------------
   function Is_Basic (Sig : Signature) return Boolean is
     (Sig'Length = 1 and then Sig (Sig'First) in Basic_Type);

   -----------------
   -- Is_Starting --
   -----------------
   function Is_Starting (Sig : Signature) return Boolean is
     (Sig'Length = 1 and then Sig (Sig'First) in Starting_Type);

   -------------------
   -- Is_Stringlike --
   -------------------
   function Is_Stringlike (Sig : Signature) return Boolean is
     (Sig'Length = 1 and then Sig (Sig'First) in Stringlike_Type);

   -----------------
   -- Conversions --
   -----------------
   function As_Character (SE : Signature_Element) return Character;
   function As_Character (SE : Signature_Element) return Character is
   begin
      case SE is
         when 'y' =>
            return 'y';
         when 'b' =>
            return 'b';
         when 'n' =>
            return 'n';
         when 'q' =>
            return 'q';
         when 'i' =>
            return 'i';
         when 'u' =>
            return 'u';
         when 'x' =>
            return 'x';
         when 't' =>
            return 't';
         when 'd' =>
            return 'd';
         when 's' =>
            return 's';
         when 'o' =>
            return 'o';
         when 'g' =>
            return 'g';
         when 'h' =>
            return 'h';
         when 'v' =>
            return 'v';
         when 'a' =>
            return 'a';
         when '(' =>
            return '(';
         when ')' =>
            return ')';
         when '{' =>
            return '{';
         when '}' =>
            return '}';
      end case;
   end As_Character;

   function As_Signature_Element (C : Character) return Signature_Element;
   function As_Signature_Element (C : Character) return Signature_Element is
   begin
      case C is
         when 'y' =>
            return 'y';
         when 'b' =>
            return 'b';
         when 'n' =>
            return 'n';
         when 'q' =>
            return 'q';
         when 'i' =>
            return 'i';
         when 'u' =>
            return 'u';
         when 'x' =>
            return 'x';
         when 't' =>
            return 't';
         when 'd' =>
            return 'd';
         when 's' =>
            return 's';
         when 'o' =>
            return 'o';
         when 'g' =>
            return 'g';
         when 'h' =>
            return 'h';
         when 'v' =>
            return 'v';
         when 'a' =>
            return 'a';
         when '(' =>
            return '(';
         when ')' =>
            return ')';
         when '{' =>
            return '{';
         when '}' =>
            return '}';
         when others =>
            raise Constraint_Error;
      end case;
   end As_Signature_Element;

   function As_String (Sig : Signature) return String is
      Result : String (Sig'Range);
   begin
      for I in Sig'Range loop
         Result (I) := As_Character (Sig (I));
      end loop;

      return Result;
   end As_String;

   function As_Signature (Sig : String) return Signature is
      Result : Signature (Sig'Range);
   begin
      for I in Sig'Range loop
         Result (I) := As_Signature_Element (Sig (I));
      end loop;

      return Result;
   end As_Signature;

   --  Sanitise a type name to be suitable for Ada type names
   --  Not exported
   function Sanitise (T : Signature) return String;
   function Sanitise (T : Signature) return String is
      Result : String (T'Range);
   begin
      for I in T'Range loop
         case T (I) is
            when '(' | ')' =>
               Result (I) := 'r';
            when '{' | '}' =>
               Result (I) := 'e';
            when others =>
               Result (I) := As_Character (T (I));
         end case;
      end loop;

      return Result;
   end Sanitise;

   ------------------
   -- Get_Interior --
   ------------------
   function Get_Interior (DType : Signature) return Signature is
      DType_First : constant Complex_Starting_Type := DType (DType'First);
   begin
      case DType_First is
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
      end case;
   end Get_Interior;

   -----------------------
   -- Get_Complete_Type --
   -----------------------
   function Get_Complete_Type
     (T : Signature; Index : Positive := 1) return Signature
   is
      Count : Natural := 0;

      --  Return the full length of a complete inner type
      --  Called only on container types
      --  (as) -> 4
      --  a{tvs} => 6
      function Inner (T : Signature) return Positive;
      function Inner (T : Signature) return Positive is
      begin
         case T (T'First) is
            --  Recursive array handler
            when 'a' =>
               case T (T'First + 1) is
                  when '(' | '{' =>
                     return Inner (T (T'First + 1 .. T'Last)) + 1;
                  when others =>
                     return 2;
               end case;

               --  Search for terminating ')'
            when '(' =>
               Struct_Depth_Search :
               declare
                  Depth : Natural := 0;
               begin
                  for I in T'Range loop
                     case T (I) is
                        when '(' =>
                           Depth := Depth + 1;
                        when ')' =>
                           Depth := Depth - 1;
                        when others =>
                           null;
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
                        when '{' =>
                           Depth := Depth + 1;
                        when '}' =>
                           Depth := Depth - 1;
                        when others =>
                           null;
                     end case;

                     --  Return the length of the dict
                     if Depth = 0 then
                        return I - T'First + 1;
                     end if;
                  end loop;
               end Dict_Depth_Search;

               raise Program_Error with "Invalid dict";
            when others =>
               raise Program_Error;
         end case;
      end Inner;

      I : Natural := T'First;
   begin
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
                     return T (I .. I + Container_Length - 1);
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
   function Get_Ada_Type (T : Signature) return String is
      AType_First : constant Starting_Type := T (T'First);
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
            return "D_Bus.Types.Obj_Path";
         when 'g' =>
            return "D_Bus.Types.Signature";
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
      end case;
   end Get_Ada_Type;

   ---------------------------
   -- Get_Library_DBus_Type --
   ---------------------------
   function Get_Library_DBus_Type (T : Signature) return String is
      AType_First : constant Starting_Type := T (T'First);
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
            return "D_Bus.Arguments.Basic.Double_Type";
         when 's' =>
            return "D_Bus.Arguments.Basic.String_Type";
         when 'o' =>
            return "D_Bus.Arguments.Basic.Object_Path_Type";
         when 'g' =>
            return "D_Bus.Arguments.Basic.Signature_Type";
         when 'a' =>
            return "D_Bus.Arguments.Containers.Array_Type";
         when '(' =>
            return "D_Bus.Arguments.Containers.Struct_Type";
         when 'v' =>
            return "D_Bus.Arguments.Containers.Variant_Type";
         when 'h' =>
            return "D_Bus.Arguments.Basic.File_Descriptor_Type";
      end case;
   end Get_Library_DBus_Type;

   --------------------------
   -- Get_Library_Ada_Type --
   --------------------------
   function Get_Library_Ada_Type (T : Signature) return String is
      AType_First : constant Basic_Type := T (T'First);
   begin
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
            return "D_Bus.Double";
         when 's' =>
            return "String";
         when 'o' =>
            return "D_Bus.Types.Obj_Path";
         when 'g' =>
            return "D_Bus.Types.Signature";
         when 'h' =>
            return "D_Bus.File_Descriptor";
      end case;
   end Get_Library_Ada_Type;
end Signatures;
