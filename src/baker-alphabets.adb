pragma Ada_2012;

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Fixed;
with Interfaces;

package body Baker.Alphabets is
   use SPARKNaCl;

   procedure Sort_String is
     new Ada.Containers.Generic_Array_Sort (Index_Type   => Positive,
                                            Element_Type => Character,
                                            Array_Type   => String);
   --------------------------
   -- Has_Duplicated_Chars --
   --------------------------

   function Has_Duplicated_Chars (X : String) return Boolean is
      Tmp : String := X;
   begin
      Sort_String (Tmp);

      return (for some I in Tmp'First .. Tmp'Last - 1 =>
                Tmp (I) = Tmp (I + 1));
   end Has_Duplicated_Chars;

   -------------------
   -- Make_Alphabet --
   -------------------

   function Make_Alphabet
     (Alphabet : Ada.Strings.Maps.Character_Set;
      Optimize : Optimization_Choice := Speed) return Cookie_Alphabet
   is (Make_Alphabet (Ada.Strings.Maps.To_Sequence (Alphabet), Optimize));
   -------------------
   -- Make_Alphabet --
   -------------------

   function Make_Alphabet
     (Alphabet : String;
      Optimize : Optimization_Choice := Speed)
      return Cookie_Alphabet
   is
      use Ada.Strings;

      procedure Closest_Power_Of_Two (AB_Size  : out Positive;
                                      Log_Size : out Bit_Counter;
                                      X        : Positive)
      is
      begin
         AB_Size := 1;
         Log_Size := 0;

         while AB_Size * 2 <= X loop
            AB_Size := AB_Size * 2;
            Log_Size := Log_Size + 1;
         end loop;
      end Closest_Power_Of_Two;

      AB_Size : Positive;
      Log_Size : Bit_Counter;
   begin
      if Has_Duplicated_Chars (Alphabet) then
         raise Constraint_Error;
      end if;

      if Optimize /= Speed then
         raise Program_Error with "Optimize = space not implemented";
      end if;

      case Optimize is
         when Size =>
            AB_Size := Alphabet'Length;
            Log_Size := 0;

         when Speed =>
            Closest_Power_Of_Two (AB_Size, Log_Size, Alphabet'Length);
      end case;

      declare
         Result : Cookie_Alphabet :=
                    (Size             => AB_Size,
                     Log_Size         => Log_Size,
                     Optimization     => Optimize,
                     Direct_Alphabet  => Fixed.Head (Source => Alphabet,
                                                     Count  => AB_Size),
                     Reverse_Alphabet => (others => Empty_Entry));
      begin
         for Idx in Result.Direct_Alphabet'Range loop
            Result.Reverse_Alphabet (Result.Direct_Alphabet (Idx)) := Idx;
         end loop;

         return Result;
      end;
   end Make_Alphabet;

   --------------
   -- Contains --
   --------------

   function Contains (Alphabet : Cookie_Alphabet; C : Character) return Boolean
   is (Alphabet.Reverse_Alphabet (C) /= Empty_Entry);

   function To_Text_Compact
     (Input : Byte_Seq; Alphabet : Cookie_Alphabet) return String
   is
      pragma Unreferenced (Alphabet, Input);
   begin
      pragma Compile_Time_Warning (True, "To_text_compact unimplemented");
      return raise Program_Error;
   end To_Text_Compact;

   -------------
   -- To_Text --
   -------------

   function To_Text
     (Input    : Byte_Seq;
      Alphabet : Cookie_Alphabet) return String
   is
   begin
      if Alphabet.Optimization = Size then
         return To_Text_Compact (Input, Alphabet);
      end if;

      --
      --  It is worth spending a couple of words about the algorithm used.
      --  It is not especially ingenious, but it can be easy to get lost in
      --  it if we do not describe it with some precision.
      --
      --  I consider the input byte_seq as a number in base 256, every entry
      --  a digit.  I want to convert it into a number in base Alphabet.AB_Size
      --
      --  Since AB_Size here is a power of two (since Optimization = speed)
      --  division and mod by AB_Size are just shift operations that "shift
      --  rigidly" the bitstring Input. This is not true if AB_Size is not
      --  a power of two.
      --
      --  The algorithm is the usual one: take Input mod AB_Size to find
      --  the least significant digit, then divide by AB_Size.
      --
      --  Let 2**nu = AB_Size.  We represent the current value with a
      --  triple (Tail, Nbit, I), where Tail is mod 2**Nbit, Nbit >= nu
      --  (nu=log2(alphabet)) and I is an integer.  The represented value is
      --
      --        N = Tail + 2**Nbit M
      --
      --  where
      --
      --        M = \sum_{k=I}^L V(k) 256^(k-I)
      --
      --  clearly, since Nbit >= nu
      --
      --        N mod 2**nu = Tail mod 2**nu
      --
      --        N / 2**nu = Tail / 2**nu + 2**(Nbit-nu) M
      --
      --  If Nbit-nu >= nu, the triple
      --
      --              [Tail',   Nbit',   I],
      --
      --  with
      --              Tail'=Tail / 2**nu
      --              Nbit'=Nbit-nu
      --
      --  correctly represents N / 2**nu; but if Nbit' < nu the
      --  condition on the second element is not satisfied anymore and we
      --  need to "borrow" a term from M
      --
      --     Tail / 2**nu + 2**(Nbit-nu) M =
      --     Tail' + 2**(Nbit-nu) V(I) + 2**(c-nu+8) M'
      --     Tail' + 2**Nbit' V(I) + 2**(Nbit'+8) M'
      --
      --  where
      --
      --      M' = \sum_{k=I+1}^L V(k) 256^(k-(I+1))
      --
      --  It turns out that the new triple is
      --
      --      [Tail'' V(I),  Nbit'', I+1]
      --
      --  with
      --      Tail'' = Tail' + 2**Nbit' V(I)
      --      Nbit'' = Nbit' + 8

      --  Note that c-nu+8 >= nu since nu < 8 and c >= nu.
      --  Note also that the borrowing is possible if I <= L.
      --  If I > L, the value represented is just a' that is smaller
      --  than 2**nu and the expansion ends.
      --  Finally, note that c <= 16
      --

      declare
         use Interfaces;

         Tail : Unsigned_16 := 0;
         Nbit_Tail : Bit_Counter := 0;
         Input_Cursor : Integer_32 := Input'First;

         --
         --  Number of output symbols.  The number of bits of the input
         --  is 8 bit/value, while every output symbol stores Log_Size
         --  bits.  If 8 * Input'Length is divisible by Log_Size we need
         --  exactly 8 * Input'Length /Log_Size; otherwise we need
         --  one additional symbol.
         --
         --  We add the additional symbol anyway since it suffices that
         --  Output_Size is large enough, it is not necessary that is
         --  the exact number of output symbols.
         --
         Output_Size : constant Positive :=
                         8 * Input'Length / Positive (Alphabet.Log_Size) + 1;

         Result : String (1 .. Output_Size);
         Output_Cursor : Natural := Result'First;

         procedure Shift_Value is
         begin
            Tail := Tail +
              (2 ** Natural (Nbit_Tail)) * Unsigned_16 (Input (Input_Cursor));

            Nbit_Tail := Nbit_Tail + 8;
            Input_Cursor := Input_Cursor + 1;
         end Shift_Value;

         procedure Push (C : Unsigned_16) is
         begin
            Result (Output_Cursor) := Alphabet.Direct_Alphabet (Integer (C));
            Output_Cursor := Output_Cursor + 1;
         end Push;
      begin
         if Input'Length = 0 then
            raise Constraint_Error;
         end if;

         Shift_Value;

         if Input'Length > 1 then
            Shift_Value;
         end if;

         loop
            Push (Tail mod (2 ** Natural (Alphabet.Log_Size)));

            Tail := Tail / (2 ** Natural (Alphabet.Log_Size));
            Nbit_Tail := Nbit_Tail - Alphabet.Log_Size;

            if Nbit_Tail < Alphabet.Log_Size then
               if Input_Cursor <= Input'Last then
                  Shift_Value;
               else
                  Push (Tail);
                  exit;
               end if;
            end if;
         end loop;

         pragma Assert (Output_Cursor = Result'Last);

         return Result;
      end;
   end To_Text;

   function To_Byte_Seq_Compact
     (Text : String; Alphabet : Cookie_Alphabet) return Byte_Seq
   is
      pragma Unreferenced (Text, Alphabet);
   begin
      pragma Compile_Time_Warning (True, "To_text_compact unimplemented");
      return raise Program_Error;
   end To_Byte_Seq_Compact;

   -----------------
   -- To_Byte_Seq --
   -----------------

   function To_Byte_Seq (Text     : String;
                         Alphabet : Cookie_Alphabet)
                         return Byte_Seq
   is
      use Interfaces;

      Tail : Unsigned_16 := 0;
      Nbit : Bit_Counter := 0;
      Input_Cursor : Positive := Text'First;

      Result : Byte_Seq (0 .. Text'Length - 1);
      Output_Cursor : Integer_32 := Result'First;

      procedure Shift_Input is
         function Val (Idx : Positive) return Unsigned_16
         is
            V : constant Integer := Alphabet.Reverse_Alphabet (Text (Idx));
         begin
            if V = Empty_Entry then
               raise Program_Error;
            end if;

            return Unsigned_16 (V);
         end Val;
      begin
         Tail := Tail + Val (Input_Cursor) * 2 ** Natural (Nbit);
         Nbit := Nbit + Alphabet.Log_Size;
         Input_Cursor := Input_Cursor + 1;
      end Shift_Input;

      procedure Push_Output (X : Unsigned_16) is
      begin
         Result (Output_Cursor) := Byte (X);
         Output_Cursor := Output_Cursor + 1;
      end Push_Output;
   begin
      if Alphabet.Optimization = Size then
         return To_Byte_Seq_Compact (Text, Alphabet);
      end if;

      --
      --  The input value (Tail, Nbit, I) is
      --
      --     Q = Tail + 2**Nbit * sum_{k=I}^L V(k) 2**(nu*(k-I))
      --
      --  If Nbit >= 8 then
      --
      --     Q mod 256 = Tail mod 256
      --     Q / 256 = Tail / 256 + 2**(Nbit-8) * sum_{k=I}^L ...
      --
      --  If Nbit - 8 >= 8 then the new representives are
      --
      --     Tail' = Tail / 256
      --     Nbit' = Nbit - 8
      --     I
      --
      --  If Nbit - 8 < 8 and I <= L we can borrow
      --
      --     Tail'' = Tail' + V(I) * 2**Nbit'
      --     Nbit'' = Nbit' + 8
      --     I' = I + 1
      --
      --  Borrowing is done as long as Nbit < 8 and I <= L.
      --  When Nbit < 8 and I > L the conversion is completed
      --

      Tail := 0;
      Nbit := 0;

      loop
         while Nbit < 8 and then Input_Cursor <= Text'Last loop
            Shift_Input;
         end loop;

         Push_Output (Tail mod 256);
         Tail := Tail / 256;

         --
         --  If Nbit < 8 it must be Input_Cursor > Text'Last
         --  otherwise we would had be still in the while loop
         --
         exit when Nbit < 8;
      end loop;

      pragma Assert (Tail = 0 and then Input_Cursor > Text'Last);

      return Result (Result'First .. Output_Cursor - 1);
   end To_Byte_Seq;

end Baker.Alphabets;
