with Ada.Strings.Maps;

with SPARKNaCl;

package Baker.Alphabets is
   type Cookie_Alphabet (<>) is private;

   type Optimization_Choice is (Speed, Size);

   function Has_Duplicated_Chars (X : String) return Boolean;

   function Make_Alphabet (Alphabet : Ada.Strings.Maps.Character_Set;
                           Optimize : Optimization_Choice := Speed)
                           return Cookie_Alphabet
     with
       Pre =>
         Ada.Strings.Maps.To_Sequence (Alphabet)'Length > 1
         and then Optimize = Speed; -- Optimize = Space not implemented yet

   function Make_Alphabet (Alphabet : String;
                           Optimize : Optimization_Choice := Speed)
                           return Cookie_Alphabet
     with
       Pre =>
         not Has_Duplicated_Chars (Alphabet)
         and then Optimize = Speed; -- Optimize = Space not implemented yet

   function Contains (Alphabet : Cookie_Alphabet;
                      C        : Character)
                      return Boolean;

   function To_Text (Input    : SPARKNaCl.Byte_Seq;
                     Alphabet : Cookie_Alphabet)
                     return String
     with
       Post => (for all C of To_Text'Result => Contains (Alphabet, C));

   function To_Byte_Seq (Text     : String;
                         Alphabet : Cookie_Alphabet)
                         return SPARKNaCl.Byte_Seq
     with
       Pre => (for all C of Text => Contains (Alphabet, C));
private
   Empty_Entry : constant Integer := -1;

   type Reverse_Map is array (Character) of Integer;

   function Is_Inverse (Dir : String;
                        Rev : Reverse_Map)
                        return Boolean
   is ((for all I in Dir'Range =>
           Rev (Dir (I)) = I)
       and then
         (for all Ch in Rev'Range =>
             Rev (Ch) = Empty_Entry or else Dir (Rev (Ch)) = Ch));

   type Bit_Counter is range 0 .. 16;

   type Cookie_Alphabet (Size : Positive) is
      record
         Optimization     : Optimization_Choice;
         Direct_Alphabet  : String (1 .. Size);
         Reverse_Alphabet : Reverse_Map;
         Log_Size         : Bit_Counter;
      end record
     with
       Dynamic_Predicate =>
         not Has_Duplicated_Chars (Cookie_Alphabet.Direct_Alphabet)
     and then
       Is_Inverse (Cookie_Alphabet.Direct_Alphabet,
                   Cookie_Alphabet.Reverse_Alphabet);

end Baker.Alphabets;
