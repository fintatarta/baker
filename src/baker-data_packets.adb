pragma Ada_2012;
with Interfaces;    use Interfaces;

package body Baker.Data_Packets is
   pragma Compile_Time_Error
     (System.Storage_Elements.Storage_Element'Size /= 8,
      "At the moment it requires that storage elements are octects");

   function To_Byte_Seq (Input : System.Storage_Elements.Storage_Array)
                         return Byte_Seq
   is
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
      Result : Byte_Seq
        (Integer_32 (Input'First) .. Integer_32 (Input'Last));
   begin
      for I in Input'Range loop
         Result (Integer_32 (I)) := Byte (Character'Pos (Input (I)));
      end loop;

      return Result;
   end To_Byte_Seq;

   function To_Label (Input : Byte_Seq) return Label_Type
   is
      Result : Label_Type;
      J      : Integer_32;
   begin
      for I in Result'Range loop
         J := Integer_32 (I - Result'First) + Input'First;
         Result (I) := Character'Val (Input (J));
      end loop;

      return Result;
   end To_Label;

   function Join (Label   : Label_Type;
                  Content : System.Storage_Elements.Storage_Array)
                  return Tagged_Element
   is (Tagged_Element (Zero_Bytes_32
       & To_Byte_Seq (Label)
       & To_Byte_Seq (Content)));

   --------------
   -- Label_Of --
   --------------

   function Label_Of (Item : Tagged_Element) return Label_Type
   is
      From : constant Integer_32 := Item'First + 32;
      To   : constant Integer_32 := From + Integer_32 (Label_Type'Length) - 1;
   begin
      return To_Label (Byte_Seq (Item (From .. To)));
   end Label_Of;

   function Content_Of (Item : Tagged_Element)
                        return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;

      subtype Offset is Storage_Offset;

      Skip : constant Offset := Offset (32 + Integer_32 (Label_Type'Length));
      From : constant Offset := Offset (Item'First) + Skip;
      To   : constant Offset := Offset (Item'Last);

   begin
      return Result : Storage_Array (From .. To) do
         for I in Result'Range loop
            Result (I) := Storage_Element (Item (Integer_32 (I)));
         end loop;
      end return;
   end Content_Of;

   ----------
   -- Join --
   ----------

   function Join (Packet : Encrypted_Payload;
                  Nonce  : Stream.HSalsa20_Nonce)
                  return Full_Packet
   is (Full_Packet (Byte_Seq (Nonce) & Byte_Seq (Packet)));

   function Nonce_Of (Item : Full_Packet)
                      return Stream.HSalsa20_Nonce
   is (Stream.HSalsa20_Nonce (Item (Item'First .. Item'First + 23)));

   function Payload_Of (Item : Full_Packet)
                        return Encrypted_Payload
   is (Encrypted_Payload (Item (Item'First + 24 .. Item'Last)));

end Baker.Data_Packets;
