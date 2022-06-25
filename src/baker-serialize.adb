pragma Ada_2012;
with Ada.Numerics.Discrete_Random;
with Ada.Storage_IO;

with SPARKNaCl.Secretbox;
with Interfaces;
with System.Storage_Elements;

package body Baker.Serialize is
   use type Interfaces.Integer_32;

   package Elemnt_Storage_Io is
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

   type Full_Packet is new Byte_Seq;
   type Encrypted_Payload is new Byte_Seq;

   function Join (Packet : Encrypted_Payload;
                  Nonce  : Stream.HSalsa20_Nonce)
                  return Full_Packet
   is (Full_Packet (Byte_Seq (Nonce) & Byte_Seq (Packet)));

   function Nonce_Of (Item : Full_Packet) return Stream.HSalsa20_Nonce
   is (Stream.HSalsa20_Nonce (Item (Item'First .. Item'First + 23)));

   function Payload_Of (Item : Full_Packet) return Encrypted_Payload
   is (Encrypted_Payload (Item (Item'First + 24 .. Item'Last)));

   type Tagged_Element is new Byte_Seq;

   function To_Byte_Seq (Input : Elemnt_Storage_Io.Buffer_Type)
                          return Byte_Seq
   is
      pragma Compile_Time_Error
        (System.Storage_Elements.Storage_Element'Size /= 8,
         "At the moment it requires that storage elements are octects");

      use Interfaces;

      Result : Byte_Seq
        (Integer_32 (Input'First) .. Integer_32 (Input'Last));
   begin
      for I in Input'Range loop
         Result (Integer_32 (I)) := Byte (Input (I));
      end loop;

      return Result;
   end To_Byte_Seq;

   function To_Byte_Seq (Input : Label_Type) return Byte_Seq
   is
      use Interfaces;

      Result : Byte_Seq
        (Integer_32 (Input'First) .. Integer_32 (Input'Last));
   begin
      for I in Input'Range loop
         Result (Integer_32 (I)) := Byte (Character'Pos (Input (I)));
      end loop;

      return Result;
   end To_Byte_Seq;

   function Join (Label   : Label_Type;
                  Content : Elemnt_Storage_Io.Buffer_Type)
                  return Tagged_Element
   is (Tagged_Element (Zero_Bytes_32
       & To_Byte_Seq (Label)
       & To_Byte_Seq (Content)));

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
      Buffer : Elemnt_Storage_Io.Buffer_Type;
   begin
      Elemnt_Storage_Io.Write (Buffer => Buffer,
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

      Packet : constant Full_Packet :=
                 Full_Packet (To_Byte_Seq (String (Cookie), Alphabet));

      Nonce     : constant Stream.HSalsa20_Nonce := Nonce_Of (Packet);
      Encrypted : constant Encrypted_Payload := Payload_Of (Packet);
      Cleartext : Byte_Seq (Encrypted'Range);
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

      Elemnt_Storage_Io.Read (Buffer => Data_Buffer (Cleartext),
                              Item   => Item);

      Status := Success;
   end Parse_Cookie;

begin
   Byte_Generators.Reset (Byte_Generator);
end Baker.Serialize;
