-- Test for Tenet.Interchange_IO package

with Tenet.Interchange_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

procedure intio001 is

   type Test_Record is
      record
         First_Name:        Unbounded_String;
         Last_Name:         Unbounded_String;
         Address_1:         Unbounded_String;
         City_or_District:  Unbounded_String;
--         State_or_Division: Unbounded_String;
         Postal_Code:       Unbounded_String;
      end record;

--   type CSV_Field is (First, Last, Street, City, State, ZIP);
   type CSV_Field is (ZIP, First, Last, Street, City);

   function CSV_Get (Rec:   in Test_Record;
                     Field: in CSV_Field) return String is
   begin
      case Field is
         when First  => return To_String( Rec.First_Name );
         when Last   => return To_String( Rec.Last_Name );
         when Street => return To_String( Rec.Address_1 );
         when City   => return To_String( Rec.City_or_District );
--         when State  => return To_String( Rec.State_or_Division );
         when ZIP    => return To_String( Rec.Postal_Code );
      end case;
   end CSV_Get;

   procedure CSV_Set (Rec:   in out Test_Record;
                      Field: in CSV_Field;
                      Image: in String) is
   begin
      case Field is
         when First  => Rec.First_Name        := To_Unbounded_String( Image );
         when Last   => Rec.Last_Name         := To_Unbounded_String( Image );
         when Street => Rec.Address_1         := To_Unbounded_String( Image );
         when City   => Rec.City_or_District  := To_Unbounded_String( Image );
--         when State  => Rec.State_or_Division := To_Unbounded_String( Image );
         when ZIP    => Rec.Postal_Code       := To_Unbounded_String( Image );
      end case;
   end CSV_Set;

   package CSV_IO is new Tenet.Interchange_IO( Test_Record, CSV_Field ); use CSV_IO;

   package CSV_Input  is new CSV_IO.Generic_Interchange_Input ( CSV_Set ); use CSV_Input;
   package CSV_Output is new CSV_IO.Generic_Interchange_Output( CSV_Get ); use CSV_Output;

   F1, F2: CSV_IO.File_Type;

   Argument_Error: exception;

   Temp: Test_Record;

begin

   if Argument_Count /= 2 then raise Argument_Error; end if;

   Open  ( F1, In_File,  Argument(1) );
   Create( F2, Out_File, Argument(2) );

   while not End_of_File(F1) loop
      Temp := ( others => To_Unbounded_String("<NOT SET>") );
      Read ( F1, Temp );
      Write( F2, Temp );
   end loop;

   Close( F1 );
   Close( F2 );

end intio001;

