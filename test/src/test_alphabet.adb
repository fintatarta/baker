with Ada.Text_IO; use Ada.Text_IO;
with Baker.Alphabets;   use Baker.Alphabets;


procedure Test_Alphabet is
   Ab : Cookie_Alphabet := Make_Alphabet ("0123456789abcdef");
   CC : Cookie_Alphabet := Make_Alphabet ("01234567");

   Q  : constant String := To_Text ((10, 20, 32, 55), Ab);
   P  : constant Baker.Byte_Seq := To_Byte_Seq (Q, Ab);
   R  : constant String := To_Text ((10, 20, 32, 55), CC);
   T  : constant Baker.Byte_Seq := To_Byte_Seq (R, Cc);
begin
   Put_Line (Q);
   for X of P loop
      Put_Line (X'Image);
   end loop;

   Put_Line (R);
   for X of T loop
      Put_Line (X'Image);
   end loop;
end Test_Alphabet;
