with System.Storage_Elements;

with SPARKNaCl;   use SPARKNaCl;
with SPARKNaCl.Stream;

private package Baker.Data_Packets is
   type Tagged_Element is new Byte_Seq;

   function Join (Label   : Label_Type;
                  Content : System.Storage_Elements.Storage_Array)
                  return Tagged_Element;

   function Label_Of (Item : Tagged_Element) return Label_Type;

   function Content_Of (Item : Tagged_Element)
                        return System.Storage_Elements.Storage_Array;

   type Full_Packet is new Byte_Seq;
   type Encrypted_Payload is new Byte_Seq;

   function Join (Packet : Encrypted_Payload;
                  Nonce  : Stream.HSalsa20_Nonce)
                  return Full_Packet;

   function Nonce_Of (Item : Full_Packet) return Stream.HSalsa20_Nonce;

   function Payload_Of (Item : Full_Packet) return Encrypted_Payload;

end Baker.Data_Packets;
