pragma Ada_2012;
with Ada.Numerics.Discrete_Random;
with Ada.Storage_IO;

with SPARKNaCl.Secretbox;

with Baker.Data_Packets;

package body Baker.Serialize is
   package Element_Storage_Io is
     new Ada.Storage_Io (Element_Type);

   package Byte_Generators is
     new Ada.Numerics.Discrete_Random (SPARKNaCl.Byte);

   Byte_Generator : Byte_Generators.Generator;

   function Random_Nonce return Stream.HSalsa20_Nonce
   is
      function Random_Byte return SPARKNaCl.Byte
      is (Byte_Generators.Random (Byte_Generator));

      Result : Stream.HSalsa20_Nonce;
   begin
      for I in Result'Range loop
         Result (I) := Random_Byte;
      end loop;

      return Result;
   end Random_Nonce;

   -----------------
   -- Make_Cookie --
   -----------------

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Alphabet : Alphabets.Cookie_Alphabet := Rfc_6265_Alphabet)
      return Cookie_Type
   is (Make_Cookie (Item, Key, Random_Nonce, Alphabet));

   -----------------
   -- Make_Cookie --
   -----------------

   function Make_Cookie
     (Item     : Element_Type;
      Key      : Core.Salsa20_Key;
      Nonce    : Stream.HSalsa20_Nonce;
      Alphabet : Alphabets.Cookie_Alphabet := Rfc_6265_Alphabet)
      return Cookie_Type
   is
      use Data_Packets;

      Buffer : Element_Storage_Io.Buffer_Type;
   begin
      Element_Storage_Io.Write (Buffer => Buffer,
                               Item   => Item);

      declare
         Packet : constant Tagged_Element := Join (Label, Buffer);

         Encrypted : Encrypted_Payload (Packet'Range);

         Status : Boolean;
      begin
         Secretbox.Create (C      => Byte_Seq (Encrypted),
                           Status => Status,
                           M      => Byte_Seq (Packet),
                           N      => Nonce,
                           K      => Key);

         return Cookie_Type
           (Alphabets.To_Text (Input    => Byte_Seq (Join (Encrypted, Nonce)),
                               Alphabet => Alphabet));
      end;
   end Make_Cookie;

   ------------------
   -- Parse_Cookie --
   ------------------

   procedure Parse_Cookie
     (Item     : out Element_Type;
      Status   : out Parsing_Result;
      Cookie   :     Cookie_Type;
      Key      :     Core.Salsa20_Key;
      Alphabet :     Alphabets.Cookie_Alphabet)
   is
      use Alphabets;
      use Data_Packets;

      Packet : constant Full_Packet :=
                 Full_Packet (To_Byte_Seq (String (Cookie), Alphabet));

      Nonce     : constant Stream.HSalsa20_Nonce := Nonce_Of (Packet);
      Encrypted : constant Encrypted_Payload := Payload_Of (Packet);
      Cleartext : Tagged_Element (Encrypted'Range);
      Valid_Data : Boolean;
   begin
      Secretbox.Open (M      => Byte_Seq (Cleartext),
                      Status => Valid_Data,
                      C      => Byte_Seq (Encrypted),
                      N      => Nonce,
                      K      => Key);

      if not Valid_Data then
         Status := Mac_Failed;
         return;
      end if;

      if Label_Of (Cleartext) /= Label then
         Status := Wrong_Type;
         return;
      end if;

      Element_Storage_Io.Read (Buffer => Content_Of (Cleartext),
                               Item   => Item);

      Status := Success;
   end Parse_Cookie;

begin
   Byte_Generators.Reset (Byte_Generator);
end Baker.Serialize;
