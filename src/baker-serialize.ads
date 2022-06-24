with SPARKNaCl.Stream;
with SPARKNaCl.Core;

use SPARKNaCl;

generic
   type Element_Type is private;

   Label : Label_Type;
package Baker.Serialize is
   function Is_Sorted (X : String) return Boolean
   is (X'Length < 2
       or else
         (for all I in X'First .. X'Last - 1 => X (I) < X (I + 1)));

   subtype Cookie_Alphabet is  String
     with
       Dynamic_Predicate =>
         Cookie_Alphabet'Length > 1
         and then
           Is_Sorted (Cookie_Alphabet);

   Rfc_6265_Alphabet : constant Cookie_Alphabet :=
                         "!\#$%&'()*+-./0123456789:<=>?@"
                         & "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`"
                         & "abcdefghijklmnopqrstuvwxyz{|}~";

   type Optimization_Choice is (Speed, Size);

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

end Baker.Serialize;
