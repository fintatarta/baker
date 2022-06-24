with SPARKNaCl.Stream;
with SPARKNaCl.Core;

use SPARKNaCl;

with Baker.Alphabets;

generic
   type Element_Type is private;

   Label : Label_Type;
package Baker.Serialize is

   Rfc_6265_Alphabet : constant Alphabets.Cookie_Alphabet;

   type Cookie_Type is new String;

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Alphabet : Alphabets.Cookie_Alphabet := Rfc_6265_Alphabet)
      return Cookie_Type;

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Nonce    : Stream.HSalsa20_Nonce;
      Alphabet : Alphabets.Cookie_Alphabet := Rfc_6265_Alphabet)
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
      Key    : Core.Salsa20_Key;
      Alphabet : Alphabets.Cookie_Alphabet);
private
   Rfc_6265_Alphabet : constant Alphabets.Cookie_Alphabet :=
                         Alphabets.Make_Alphabet
                           ("!\#$%&'()*+-./0123456789:<=>?@"
                            & "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`"
                            & "abcdefghijklmnopqrstuvwxyz{|}~");

end Baker.Serialize;
