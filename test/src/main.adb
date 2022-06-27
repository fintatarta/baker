with Ada.Text_Io;    use Ada.Text_Io;

with Baker.Serialize;
with SPARKNaCl.Core;

procedure Main is
   type Pippo is
      record
         X, Y : Float;
         S    : String (1 .. 3);
      end record;

   package Bb is
     new Baker.Serialize (Pippo, "pipo");

   K : constant SPARKNaCl.Core.Salsa20_Key :=
         SPARKNaCl.Core.Construct ((others => 42));

   Q : constant Pippo := (4.0, -1.3, "abc");
   C : constant Bb.Cookie_Type := Bb.Make_Cookie (Item     => Q,
                                                  Key      => K);

   D : Pippo;
   Ok : Bb.Parsing_Result;
begin
   Bb.Parse_Cookie (Item     => D,
                    Status   => OK,
                    Cookie   => C,
                    Key      => K);

   Put_Line (String (C));
   Put_Line (Ok'Image);
   Put_Line (D.X'Image & D.Y'Image & " (" & D.S & ")");
end Main;
