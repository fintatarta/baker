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

   function join (Packet : Byte_Seq;
                  Nonce  : Stream.HSalsa20_Nonce)
                  return Byte_Seq
   is (Packet & Byte_Seq (Nonce));

   function nonce_of (item : Byte_Seq) return Stream.HSalsa20_Nonce
   is (Stream.HSalsa20_Nonce (item (item'Last - 23 .. item'Last)));

   function payload_of (item : Byte_Seq) return Byte_Seq
   is (item (item'First .. item'Last - 24));

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
      function To_Byte_Seq (input : Label_Type) return Byte_Seq
      is
         use Interfaces;

         result : Byte_Seq
           (Integer_32 (input'First) .. Integer_32 (input'Last));
      begin
         for i in input'Range loop
            result (Integer_32 (i)) := Byte (Character'Pos (input (i)));
         end loop;

         return result;
      end To_Byte_Seq;

      function To_Byte_Seq (input : Elemnt_Storage_Io.Buffer_Type)
                            return Byte_Seq
      is
         pragma Compile_Time_Error
           (System.Storage_Elements.Storage_Element'Size /= 8,
            "At the moment it requires that storage elements are octects");

         use Interfaces;

         result : Byte_Seq
           (Integer_32 (input'First) .. Integer_32 (input'Last));
      begin
         for i in input'Range loop
            result (Integer_32 (i)) := Byte (input (i));
         end loop;

         return result;
      end To_Byte_Seq;
      Buffer : Elemnt_Storage_Io.Buffer_Type;
   begin
      Elemnt_Storage_Io.Write (Buffer => Buffer,
                               Item   => Item);

      declare
         Packet : constant Byte_Seq :=
                    Zero_Bytes_32
                    & To_Byte_Seq (Label)
                    & To_Byte_Seq (Buffer);

         Encrypted : Byte_Seq (Packet'Range);

         Status : Boolean;
      begin
         Secretbox.Create (C      => Encrypted,
                           Status => Status,
                           M      => Packet,
                           N      => Nonce,
                           K      => Key);

         return Cookie_Type
           (Alphabets.To_Text (Input    => join (Encrypted, Nonce),
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
      Key      : Core.Salsa20_Key;
      Alphabet :     Alphabets.Cookie_Alphabet)
   is
      packet : constant Byte_Seq :=
                 Alphabets.To_Byte_Seq (String (Cookie), Alphabet);

      nonce  : constant Stream.HSalsa20_Nonce := nonce_of (packet);
      encrypted : constant Byte_Seq := payload_of (packet);
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Parse_Cookie unimplemented");
      raise Program_Error with "Unimplemented procedure Parse_Cookie";
   end Parse_Cookie;

begin
   Byte_Generators.Reset (Byte_Generator);
end Baker.Serialize;
