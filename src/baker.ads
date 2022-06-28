with SPARKNaCl.Core;
with SPARKNaCl.Stream;

package Baker is
   type Label_Type is new String (1 .. 4);

   subtype Byte_Seq is SPARKNaCl.Byte_Seq;
   subtype Secret_Key is SPARKNaCl.Core.Salsa20_Key;
   subtype Nonce_Value is SPARKNaCl.Stream.HSalsa20_Nonce;
end Baker;
