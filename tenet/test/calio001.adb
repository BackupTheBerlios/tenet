with Ada.Calendar, Tenet.Calendar_IO, Ada.Text_IO;
use  Ada.Calendar, Tenet.Calendar_IO, Ada.Text_IO;

procedure calio001 is

   TF_Name: constant String := "caliotf1.txt";

   TF: File_Type;
   T1: Time := Time_Of( 1985, 4, 12, 14*60*60.0 + 23*60.0 + 45.67);
   TT: Time;

   function F1 (T: in Time; N: in Integer) return Time is
   begin
      return T + Duration(N)*123456.789;
   end;

   Bad: Natural := 0;

begin
   Tenet.Calendar_IO.Default_Seconds_Aft := 15;

   -- Write test file:
   Create( TF, Out_File, TF_Name );
   for i in 0..19 loop
      TT := F1(T1,i);
      Put( TT ); New_Line;
      Put( TF, TT ); New_Line( TF );
   end loop;

   -- Read and compare test file:
   Reset( TF, In_File );
   for i in 0..19 loop
      Get( TF, TT ); Skip_Line( TF );
      if TT /= F1(T1,i) then
         Bad := Bad + 1;
         Put( "MISMATCH: " ); Put( TT ); Put( " :: " ); Put( F1(T1,i) ); New_Line;
      end if;
   end loop;

   Close( TF );
   if Bad > 0 then raise Program_Error; end if;

end calio001;

