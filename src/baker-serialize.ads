with SPARKNaCl.Stream;
with SPARKNaCl.Core;

use SPARKNaCl;

generic
   type Element_Type is private;

   Label : Label_Type;
package Baker.Serialize is
   type Cookie_Alphabet (<>) is private;

   Rfc_6265_Alphabet : constant Cookie_Alphabet;

   type Optimization_Choice is (Speed, Size);

   function Has_Duplicated_Chars (X : String) return Boolean;

   function Make_Alphabet (Alphabet : String;
                           Optimize : Optimization_Choice := Speed)
                           return Cookie_Alphabet
     with
       Pre => not Has_Duplicated_Chars (Alphabet);

   type Cookie_Type is new String;

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Alphabet : Cookie_Alphabet := Rfc_6265_Alphabet;
      Optimize : Optimization_Choice := Speed)
      return Cookie_Type;

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Nonce    : Stream.HSalsa20_Nonce;
      Alphabet : Cookie_Alphabet := Rfc_6265_Alphabet;
      Optimize : Optimization_Choice := Speed)
      return Cookie_Type;

   type Parsing_Result is
     (
      Success,
      Mac_Failed,
      Wrong_Type
     );

   procedure Parse_Cookie
     (Item   : out Element_Type;
      Status : out Parsing_Result;
      Cookie : Cookie_Type;
      Key    : Core.Salsa20_Key);
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

   type Cookie_Alphabet (Size : Positive) is
      record
         Optimization     : Optimization_Choice;
         Direct_Alphabet  : String (1 .. Size);
         Reverse_Alphabet : Reverse_Map;
      end record
     with
       Dynamic_Predicate =>
         not Has_Duplicated_Chars (Cookie_Alphabet.Direct_Alphabet)
     and then
       Is_Inverse (Cookie_Alphabet.Direct_Alphabet,
                   Cookie_Alphabet.Reverse_Alphabet);

   Rfc_6265_Alphabet : constant Cookie_Alphabet :=
                         Make_Alphabet ("!\#$%&'()*+-./0123456789:<=>?@"
                                        & "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`"
                                        & "abcdefghijklmnopqrstuvwxyz{|}~");

end Baker.Serialize;
