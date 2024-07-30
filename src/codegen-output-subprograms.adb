pragma Ada_2012;

with Signatures.Unbounded; use Signatures.Unbounded;
with Signatures;           use Signatures;
with Shared;               use Shared;

package body Codegen.Output.Subprograms is
   -------------------
   -- Get_Arguments --
   -------------------
   function Get_Arguments
     (AL : Parsing.Argument_List; Reverse_Direction : Boolean := False)
      return String;
   function Get_Arguments
     (AL : Parsing.Argument_List; Reverse_Direction : Boolean := False)
      return String
   is
      use Ada.Strings.Unbounded;
      Buf : Unbounded_String;

      function To_Ada_Direction (D : Parsing.DBus_Direction) return String;
      function To_Ada_Direction (D : Parsing.DBus_Direction) return String is
      begin
         case D is
            when Parsing.DIn =>
               return (if Reverse_Direction then "out" else "");
            when Parsing.DOut =>
               return (if Reverse_Direction then "" else "out");
         end case;
      end To_Ada_Direction;
   begin
      if not AL.Is_Empty then
         declare
            FI : constant Positive := AL.First_Index;
            LI : constant Positive := AL.Last_Index;
         begin
            for I in FI .. LI loop
               Append
                 (Buf,
                  (+AL (I).Name) & " : " &
                  To_Ada_Direction (AL (I).Direction) & " " &
                  (Signatures.Get_Ada_Type (+AL (I).Type_Code)));

               if I /= LI then
                  Append (Buf, "; ");
               end if;
            end loop;
         end;
      end if;

      return To_String (Buf);
   end Get_Arguments;
   --  Return a list of Ada-formatted arguments for `AL`
   --  ex. A : in A_Type; B : out B_Type

   ----------------------------
   -- Interface_Handler_Name --
   ----------------------------
   function Interface_Handler_Name (Pkg : Ada_Package_Type) return String is
   begin
      return "H_" & (+Pkg.Name);
   end Interface_Handler_Name;

   ---------------------------------
   -- Interface_Handler_Signature --
   ---------------------------------
   function Interface_Handler_Signature (Pkg : Ada_Package_Type) return String
   is
   begin
      return
        Interface_Handler_Name (Pkg) &
        " (O : in out D_Bus.Support.Server.Server_Interface'Class;" &
        " Request : D_Bus.Messages.Message_Type;" &
        " Reply : out D_Bus.Messages.Message_Type)";
   end Interface_Handler_Signature;

   -----------------
   -- Method_Name --
   -----------------
   function Method_Name (M : Parsing.Method_Type) return String is
   begin
      return Sanitise_Name (+M.Name);
   end Method_Name;

   ----------------------
   -- Method_Signature --
   ----------------------
   function Method_Signature (M : Parsing.Method_Type) return String is
      Args : constant String := Get_Arguments (M.Arguments);
   begin
      return
        Method_Name (M) & " (O : in out Child_Interface'Class" &
        (if Args'Length > 0 then "; " & Args else "") & ")";
   end Method_Signature;

   function Unbound_Method_Signature (M : Parsing.Method_Type) return String is
      Args : constant String := Get_Arguments (M.Arguments);
   begin
      return
        Method_Name (M) & " (O : in out Child_Interface" &
        (if Args'Length > 0 then "; " & Args else "") & ")";
   end Unbound_Method_Signature;

   ----------------------------
   -- Method_Call_Expression --
   ----------------------------
   function Method_Call_Expression (M : Parsing.Method_Type) return String is
      use Ada.Strings.Unbounded;
      use type Parsing.Argument_Lists.Cursor;

      Arguments : Unbounded_String;
   begin
      if not M.Arguments.Is_Empty then
         Append (Arguments, " (");

         for AC in M.Arguments.Iterate loop
            if AC = M.Arguments.First then
               Append (Arguments, M.Arguments (AC).Name);
            else
               Append (Arguments, ", ");
               Append (Arguments, M.Arguments (AC).Name);
            end if;
         end loop;

         Append (Arguments, ")");
      end if;

      return Method_Name (M) & To_String (Arguments);
   end Method_Call_Expression;

   -------------------------
   -- Method_Handler_Name --
   -------------------------
   function Method_Handler_Name (M : Parsing.Method_Type) return String is
   begin
      return "H_" & Method_Name (M);
   end Method_Handler_Name;

   ------------------------------
   -- Method_Handler_Signature --
   ------------------------------
   function Method_Handler_Signature
     (Pkg : Ada_Package_Type; M : Parsing.Method_Type) return String
   is
   begin
      return
        Method_Handler_Name (M) & " (O : in out " & (+Pkg.Name) &
        ".Child_Interface'Class;" & " Request : D_Bus.Messages.Message_Type;" &
        " Reply : out D_Bus.Messages.Message_Type)";
   end Method_Handler_Signature;

   -----------------
   -- Signal_Name --
   -----------------
   function Signal_Name (S : Parsing.Signal_Type) return String is
   begin
      return Sanitise_Name (+S.Name);
   end Signal_Name;

   ----------------------
   -- Signal_Signature --
   ----------------------
   function Signal_Signature (S : Parsing.Signal_Type) return String is
      Args : constant String :=
        Get_Arguments (AL => S.Arguments, Reverse_Direction => True);
   begin
      return
        Signal_Name (S) & "(O : Child_Interface'Class" &
        (if Args'Length = 0 then "" else "; " & Args) & ")";
   end Signal_Signature;

   --------------------------
   -- Signal_Register_Name --
   --------------------------
   function Signal_Register_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Register_" & Signal_Name (S);
   end Signal_Register_Name;

   -------------------------------
   -- Signal_Register_Signature --
   -------------------------------
   function Signal_Register_Signature
     (S : Parsing.Signal_Type) return String is
     (Signal_Register_Name (S) & "(O : in out Child_Interface'Class)");

   ----------------------------
   -- Signal_Unregister_Name --
   ----------------------------
   function Signal_Unregister_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Unregister_" & Signal_Name (S);
   end Signal_Unregister_Name;

   ---------------------------------
   -- Signal_Unregister_Signature --
   ---------------------------------
   function Signal_Unregister_Signature
     (S : Parsing.Signal_Type) return String is
     (Signal_Unregister_Name (S) & " (O : in out Child_Interface'Class)");

   -----------------------
   -- Signal_Await_Name --
   -----------------------
   function Signal_Await_Name (S : Parsing.Signal_Type) return String is
   begin
      return "Await_" & Signal_Name (S);
   end Signal_Await_Name;

   ----------------------------
   -- Signal_Await_Signature --
   ----------------------------
   function Signal_Await_Signature (S : Parsing.Signal_Type) return String is
      Args : constant String := Get_Arguments (S.Arguments);
   begin
      return
        Signal_Await_Name (S) & " (O : Child_Interface'Class" &
        (if Args'Length > 0 then "; " & Args else "") & ")";
   end Signal_Await_Signature;

   ------------------------
   -- Property_Read_Name --
   ------------------------
   function Property_Read_Name (P : Parsing.Property_Type) return String is
   begin
      return Sanitise_Name (+P.Name);
   end Property_Read_Name;

   -----------------------------
   -- Property_Read_Signature --
   -----------------------------
   function Property_Read_Signature (P : Parsing.Property_Type) return String
   is
   begin
      return
        Property_Read_Name (P) & " (O : Child_Interface'Class) return " &
        Signatures.Get_Ada_Type (+P.Type_Code);
   end Property_Read_Signature;

   -------------------------
   -- Property_Write_Name --
   -------------------------
   function Property_Write_Name (P : Parsing.Property_Type) return String is
   begin
      return "Set_" & Sanitise_Name (+P.Name);
   end Property_Write_Name;

   ------------------------------
   -- Property_Write_Signature --
   ------------------------------
   function Property_Write_Signature (P : Parsing.Property_Type) return String
   is
   begin
      return
        Property_Write_Name (P) & " (O : in out Child_Interface'Class;" &
        " Value : " & Signatures.Get_Ada_Type (+P.Type_Code) & ")";
   end Property_Write_Signature;

   ----------------------
   -- Bind_To_Ada_Name --
   ----------------------
   function Bind_To_Ada_Name return String is ("Bind_To_Ada");

   ---------------------------
   -- Bind_To_Ada_Signature --
   ---------------------------
   function Bind_To_Ada_Signature (TD : Ada_Type_Declaration) return String is
   begin
      return
        "Bind_To_Ada (DBus_Entity : " & Get_Library_DBus_Type (+TD.Type_Code) &
        "; Ada_Entity : out " & Get_Ada_Type (+TD.Type_Code) & ")";
   end Bind_To_Ada_Signature;

   -----------------------
   -- Bind_To_DBus_Name --
   -----------------------
   function Bind_To_DBus_Name return String is ("Bind_To_DBus");

   ----------------------------
   -- Bind_To_DBus_Signature --
   ----------------------------
   function Bind_To_DBus_Signature (TD : Ada_Type_Declaration) return String is
   begin
      return
        "Bind_To_DBus (Ada_Entity : " & Get_Ada_Type (+TD.Type_Code) &
        "; DBus_Entity : out " & Get_Library_DBus_Type (+TD.Type_Code) & ")";
   end Bind_To_DBus_Signature;

end Codegen.Output.Subprograms;
