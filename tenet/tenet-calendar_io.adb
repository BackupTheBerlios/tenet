-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Calendar_IO package body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Tenet.Debugging; use Tenet.Debugging; --$D
-- with Ada.Characters.Handling;


-------------------------------------------------------------------------------------------------------------------
package body Tenet.Calendar_IO is


   use Ada.Calendar;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Numeric imaging and evaluation =###=

   subtype Positive_Field is Field range 1..Field'Last;

   subtype Hour_of_Day      is Integer range 0..59;
   subtype Minute_of_Hour   is Integer range 0..59;
   subtype Second_of_Minute is Integer range 0..59;

   subtype Fraction_of_Second is Day_Duration range 0.0 .. 1.0;

   type Fraction_Integer is range 0 .. 10**Fraction_of_Second'Aft;

   -------------------------------------------------
   -- =##= Decimal digit imaging and evaluation =##=

   -- =#= The decimal digit characters =#=

   subtype Decimal_Digit_Image is Character range '0'..'9';
   subtype Decimal_Digit_Value is Natural   range  0 .. 9;

   -- =#= Value (between 0 and 9) of a decimal digit character =#=

   function Value (Digit: in Decimal_Digit_Image) return Decimal_Digit_Value is
   begin
      return Decimal_Digit_Image'Pos(Digit) - Decimal_Digit_Image'Pos('0');
   end;

   -- =#= Decimal digit character of a value (between 0 and 9) =#=

   function Image (Digit: in Decimal_Digit_Value) return Decimal_Digit_Image is
   begin
      return Decimal_Digit_Image'Val( Digit + Decimal_Digit_Image'Pos('0') );
   end;

   -------------------------------------------------
   -- =##= Evaluate an unsigned decimal integer =##=

   generic
      with procedure Look_Ahead (Item: out Character; End_of_Data: out Boolean) is <>;
      with procedure Skip_Character is <>;

   procedure Generic_Get_Decimal (Result:   out    Natural;
                                  Min, Max: in     Natural;  -- used to check result and raise correct exception
                                  Width:    in     Field;    -- maximum number of digits to get
                                  Count:    in out Field;    -- running counter of characters got
                                  Limit:    in     Field);   -- maximum for Count, or 0 for no limit

   procedure Generic_Get_Decimal (Result:   out    Natural;
                                  Min, Max: in     Natural;
                                  Width:    in     Field;
                                  Count:    in out Field;
                                  Limit:    in     Field) is

      Val: Natural := 0; -- working value
      LAC: Character;    -- look-ahead character
      EOD: Boolean;      -- end of data flag

--$[]      package Int_IO is new Ada.Text_IO.Integer_IO(Integer); --$D

   begin

--$[]      Ada.Text_IO.Put( Ada.Text_IO.Current_Error, "Min: " ); --$D
--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Min,  10 ); --$D
--$[]      Ada.Text_IO.New_Line( Ada.Text_IO.Current_Error ); --$D

--$[]      Ada.Text_IO.Put( Ada.Text_IO.Current_Error, "Max: " ); --$D
--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Max,  10 ); --$D
--$[]      Ada.Text_IO.New_Line( Ada.Text_IO.Current_Error ); --$D

      for D in 1..Width loop
         Look_Ahead( LAC, EOD );
         if EOD or LAC not in Decimal_Digit_Image then
            if D = 1 then raise Data_Error; end if; -- no digits at all
            exit;
         end if;
         if Limit /= 0 and Count = Limit then raise Data_Error; end if;
         Count := Count+1;
         Skip_Character;
         Val := Val*10 + Value( Decimal_Digit_Image'(LAC) );

--$[]         Int_IO.Put( Ada.Text_IO.Current_Error, D,     2 ); --$D
--$[]         Int_IO.Put( Ada.Text_IO.Current_Error, Val,  10 ); --$D
--$[]         Ada.Text_IO.New_Line( Ada.Text_IO.Current_Error ); --$D

      end loop;

      if Val < Min or Val > Max then raise Data_Error; end if; -- out of legal range

      Result := Val; -- all is well, return result

   end Generic_Get_Decimal;

   ----------------------------------------------
   -- =##= Image an unsigned decimal integer =##=

   generic
      with procedure Put (Item: in Character) is <>;

   procedure Generic_Put_Decimal (Item:  in Natural;
                                  Width: in Positive_Field);

   procedure Generic_Put_Decimal (Item:  in Natural;
                                  Width: in Positive_Field) is

      Val: Natural := Item;   -- working value
      Img: String(1..Width); -- resulting image

   begin

      -- Image each digit:
      for i in Img'Range loop
         Img(i) := Image( Decimal_Digit_Value'( Val mod 10 ) );
         Val := Val / 10;
      end loop;

      -- Emit image (in reverse order):
      for i in reverse Img'Range loop
         Put( Img(i) );
      end loop;

   end Generic_Put_Decimal;

   --------------------------------------------
   -- =##= Evaluate a fraction of a second =##=

   generic
      with procedure Look_Ahead (Item: out Character; End_of_Data: out Boolean) is <>;
      with procedure Skip_Character is <>;

   procedure Generic_Get_Fraction (Result: out    Fraction_of_Second;
                                   Width:  in     Field;   -- maximum number of digits to get
                                   Count:  in out Field;   -- running counter of characters got
                                   Limit:  in     Field);  -- maximum for Count, or 0 for no limit

   procedure Generic_Get_Fraction (Result: out    Fraction_of_Second;
                                   Width:  in     Field;
                                   Count:  in out Field;
                                   Limit:  in     Field) is

      Val: Fraction_of_Second := 0.0; -- working value
      Pwr: Fraction_of_Second := 1.0; -- power of 10
      LAC: Character; -- look-ahead character
      EOD: Boolean;   -- end of data flag
      Dig: Positive := 1; -- digit counter

   begin
      loop
         Look_Ahead( LAC, EOD );
         if EOD or LAC not in Decimal_Digit_Image then
            if Dig = 1 then raise Data_Error; end if; -- no digits at all
            exit;
         end if;
         if Limit /= 0 and Count = Limit then raise Data_Error; end if;
         Dig := Dig+1;
         Count := Count+1;
         Skip_Character;
--         if Dig <= Fraction_of_Second'Aft then -- if it is a significant digit
            Pwr := Pwr/10;
            Val := Val + Pwr*Value( Decimal_Digit_Image'(LAC) );
--         end if;
      end loop;

      Result := Val; -- all is well, return result

   end Generic_Get_Fraction;

   -----------------------------------------
   -- =##= Image a fraction of a second =##=

   generic
      with procedure Put (Item: in Character) is <>;

   procedure Generic_Put_Fraction (Item:  in Fraction_of_Second;
                                   Width: in Positive_Field);

   procedure Generic_Put_Fraction (Item:  in Fraction_of_Second;
                                   Width: in Positive_Field) is

      Eff_Wid: Positive_Field := Field'Min(Width,Fraction_of_Second'Aft);
      -- effective width, must be no more than Fraction_of_Second'Aft (so not to exceed range of Fraction_Integer)

      Val: Fraction_Integer := Fraction_Integer( Item * 10**Eff_Wid );
      -- working value, an integer representing the required digits of the fraction

      Img: String(1..Eff_Wid); -- resulting image

   begin

      -- Image each digit:
      for i in Img'Range loop
         Img(i) := Image( Decimal_Digit_Value( Val mod 10 ) );
         Val := Val / 10;
      end loop;

      -- Emit image (in reverse order):
      for i in reverse Img'Range loop
         Put( Img(i) );
      end loop;

      -- Emit any extra trailing zeroes necessary:
      for i in 1..Width-Eff_Wid loop Put('0'); end loop;

   end Generic_Put_Fraction;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Skipping characters =###=

   ---------------------------------------------------------------
   -- =##= Skip any number of a certain character on one line =##=

   generic
      with procedure Look_Ahead (Item: out Character; End_of_Data: out Boolean) is <>;
      with procedure Skip_Character is <>;

   procedure Generic_Skip_Repeatedly (Literal: in     Character;
                                      Count:   in out Field;
                                      Width:   in     Field);

   procedure Generic_Skip_Repeatedly (Literal: in     Character;
                                      Count:   in out Field;
                                      Width:   in     Field) is

      LAC: Character; -- look-ahead character
      EOD: Boolean;   -- end of data flag

   begin

      loop
         Look_Ahead( LAC, EOD );
         exit when EOD or LAC /= Literal;
         if Width /= 0 and Count = Width then raise Data_Error; end if;
         Count := Count+1;
         Skip_Character;
      end loop;

   end Generic_Skip_Repeatedly;

   ----------------------------------------------------------------------
   -- =##= Skip a specific string (error if input does not match it) =##=

   generic
      with procedure Look_Ahead (Item: out Character; End_of_Data: out Boolean) is <>;
      with procedure Skip_Character is <>;

   procedure Generic_Skip_Exactly (Literal: in     String;
                                   Count:   in out Field;
                                   Width:   in     Field);

   procedure Generic_Skip_Exactly (Literal: in     String;
                                   Count:   in out Field;
                                   Width:   in     Field) is

      LAC: Character;
      EOD: Boolean;

   begin

      for i in Literal'Range loop
         if Width /= 0 and Count = Width then raise Data_Error; end if;
         Look_Ahead( LAC, EOD );
         if EOD or LAC /= Literal(i) then raise Data_Error; end if;
         Count := Count+1;
         Skip_Character;
      end loop;

   end Generic_Skip_Exactly;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Evaluating and imaging a value of the type Ada.Calendar.Time =###=

   Space:             constant Character := ' '; -- according to me (NJR :-)
   Date_Separator:    constant Character := '-'; -- according to ISO 8601
   Time_Separator:    constant Character := ':'; -- according to ISO 8601
   Decimal_Separator: constant Character := '.'; -- according to ISO 8601 (not preferred, but preferred by me!)

   ------------------------------
   -- =##= Evaluating a time =##=

   generic
      with procedure Look_Ahead (Item: out Character; End_of_Line: out Boolean) is <>;
      with procedure Skip_Character is <>;

   procedure Generic_Get_Time (Result:      out Ada.Calendar.Time;
                               Width:       in  Field;
                               Time_Prefix: in  Character);

   procedure Generic_Get_Time (Result:      out Ada.Calendar.Time;
                               Width:       in  Field;
                               Time_Prefix: in  Character) is

      procedure Skip_Repeatedly is new Generic_Skip_Repeatedly;
      procedure Skip_Exactly    is new Generic_Skip_Exactly;
      procedure Get_Decimal     is new Generic_Get_Decimal;
      procedure Get_Fraction    is new Generic_Get_Fraction;

      Year:     Year_Number;
      Month:    Month_Number;
      Day:      Day_Number;
      Hour:     Hour_of_Day        := 0;
      Minute:   Minute_of_Hour     := 0;
      Second:   Second_of_Minute   := 0;
      Fraction: Fraction_of_Second := 0.0;

      Count: Field := 0; -- number of characters parsed so far

   begin

      if Width > 0 and Width < 5 then raise Layout_Error; end if;

      for Phase in 1..7 loop
         exit when Phase > 3 and Width /= 0 and Count = Width;
         case Phase is
            when 1     => Skip_Repeatedly( Space, Count, Width );
            when 2 | 3 => Skip_Exactly( ( 1 => Date_Separator    ), Count, Width );
            when 4     => Skip_Exactly( ( 1 => Time_Prefix       ), Count, Width );
            when 5 | 6 => Skip_Exactly( ( 1 => Time_Separator    ), Count, Width );
            when 7     => Skip_Exactly( ( 1 => Decimal_Separator ), Count, Width );
         end case;
         if Width /= 0 and Count = Width then raise Data_Error; end if;
         case Phase is
            when 1 => Get_Decimal( Year,   Year_Number'First,      Year_Number'Last,      4, Count, Width );
            when 2 => Get_Decimal( Month,  Month_Number'First,     Month_Number'Last,     2, Count, Width );
            when 3 => Get_Decimal( Day,    Day_Number'First,       Day_Number'Last,       2, Count, Width );
            when 4 => Get_Decimal( Hour,   Hour_of_Day'First,      Hour_of_Day'Last,      2, Count, Width );
            when 5 => Get_Decimal( Minute, Minute_of_Hour'First,   Minute_of_Hour'Last,   2, Count, width );
            when 6 => Get_Decimal( Second, Second_of_Minute'First, Second_of_Minute'Last, 2, Count, Width );
            when 7 => Get_Fraction( Fraction, Field'Max(Width-Count,0), Count, Width );
         end case;
      end loop;

      Result := Ada.Calendar.Time_of( Year, Month, Day, Hour*60.0*60.0 + Minute*60.0 + Second*1.0 + Fraction );

   end Generic_Get_Time;

   ---------------------------
   -- =##= Imaging a time =##=

   generic
      with procedure Put (Item: in Character) is <>;

   procedure Generic_Put_Time (Value:       in Ada.Calendar.Time;
                               Format:      in Time_Format       := Default_Format;
                               Seconds_Aft: in Ada.Text_IO.Field := Default_Seconds_Aft;
                               Time_Prefix: in Character         := Default_Time_Prefix);

   procedure Generic_Put_Time (Value:        in Ada.Calendar.Time;
                               Format:       in Time_Format       := Default_Format;
                               Seconds_Aft:  in Ada.Text_IO.Field := Default_Seconds_Aft;
                               Time_Prefix:  in Character         := Default_Time_Prefix) is

      procedure Put_Decimal  is new Generic_Put_Decimal;
      procedure Put_Fraction is new Generic_Put_Fraction;

      subtype Phase_Number is Integer range 1..7; -- 1 => year ... 7 => fraction of second

      Phase_End: constant array (Time_Format) of Phase_Number := (YMD => 3, YMDH => 4, YMDHM => 5, YMDHMS => 7 );

      Year:     Year_Number;
      Month:    Month_Number;
      Day:      Day_Number;
      Hour:     Hour_of_Day        := 0;
      Minute:   Minute_of_Hour     := 0;
      Second:   Second_of_Minute   := 0;
      Fraction: Fraction_of_Second := 0.0;

      Day_Secs, Hour_Secs, Min_Secs: Day_Duration;

      procedure Divide (Dividend:  in  Day_Duration;
                        Divisor:   in  Day_Duration;
                        Quotient:  out Natural;
                        Remainder: out Day_Duration) is
      begin
         Quotient  := Integer( Day_Duration'( Dividend / Divisor ) - 0.5 );
         Remainder := Dividend - Divisor*Quotient;
      end;

      procedure Split (Number:   in  Day_Duration;
                       Whole:    out Natural;
                       Fraction: out Day_Duration) is
      begin
         Whole    := Integer( Number - 0.5 );
         Fraction := Number - Day_Duration(Whole);
      end;

--$[]      package Int_IO is new Ada.Text_IO.Integer_IO(Integer); --$D
--$[]      package Dur_IO is new Ada.Text_IO.Fixed_IO(Duration);  --$D

   begin

      Ada.Calendar.Split( Value, Year, Month, Day, Day_Secs );

--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Year,  5 ); --$D
--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Month, 3 ); --$D
--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Day,   3 ); --$D

--$[]      Dur_IO.Put( Ada.Text_IO.Current_Error, Day_Secs, 10, 10 ); --$D

      Divide( Day_Secs, 60*60.0, Hour, Hour_Secs );

--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Hour,  3 ); --$D
--$[]      Dur_IO.Put( Ada.Text_IO.Current_Error, Hour_Secs, 10, 10 ); --$D

      Divide( Hour_Secs, 60.0, Minute, Min_Secs );

--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Minute, 3 ); --$D
--$[]      Dur_IO.Put( Ada.Text_IO.Current_Error, Min_Secs, 10, 10 ); --$D

      Split( Min_Secs, Second, Fraction );

--$[]      Int_IO.Put( Ada.Text_IO.Current_Error, Second, 3 ); --$D
--$[]      Dur_IO.Put( Ada.Text_IO.Current_Error, Fraction, 10, 10 ); --$D

      for Phase in Phase_Number loop
         exit when Phase > Phase_End(Format) or (Phase = 7 and Seconds_Aft = 0);
         case Phase is
            when 1     => null;
            when 2 | 3 => Put( Date_Separator    );
            when 4     => Put( Time_Prefix       );
            when 5 | 6 => Put( Time_Separator    );
            when 7     => Put( Decimal_Separator );
         end case;
         case Phase is
            when 1 => Put_Decimal( Year,   4 );
            when 2 => Put_Decimal( Month,  2 );
            when 3 => Put_Decimal( Day,    2 );
            when 4 => Put_Decimal( Hour,   2 );
            when 5 => Put_Decimal( Minute, 2 );
            when 6 => Put_Decimal( Second, 2 );
            when 7 => Put_Fraction( Fraction, Seconds_Aft );
         end case;
      end loop;

   end Generic_Put_Time;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Inputting absolute times =###=

   -------------------------------
   -- =##= From current input =##=

   procedure Get (Item:        out Ada.Calendar.Time;
                  Width:       in  Field     := 0;
                  Time_Prefix: in  Character := Default_Time_Prefix) is
   begin
      Get( Ada.Text_IO.Current_Input, Item, Width, Time_Prefix );
   end;

   ------------------------
   -- =##= From a file =##=

   procedure Get (File:        in  Ada.Text_IO.File_Type;
                  Item:        out Ada.Calendar.Time;
                  Width:       in  Field     := 0;
                  Time_Prefix: in  Character := Default_Time_Prefix) is

      procedure Look_Ahead (Item:        out Character;
                            End_of_Data: out Boolean) is
      begin
         Ada.Text_IO.Look_Ahead( File, Item, End_of_Data );
      end;

      procedure Skip_Character is
         Dummy: Character;
      begin
         Ada.Text_IO.Get( File, Dummy );
      end;

      procedure Get_Time is new Generic_Get_Time;

   begin
      Get_Time( Item, Width, Time_Prefix );
   end;

   --------------------------
   -- =##= From a string =##=

   procedure Get (From:        in  String;
                  Item:        out Ada.Calendar.Time;
                  Last:        out Positive;
                  Time_Prefix: in  Character := Default_Time_Prefix) is

      Cursor: Natural range From'First .. From'Last := From'First;
      At_End: Boolean := From'Last < From'First;

      procedure Look_Ahead (Item:        out Character;
                            End_of_Data: out Boolean) is
      begin
         if At_End then
            End_of_Data := True;
         else
            End_of_Data := False;
            Item := From(Cursor);
         end if;
      end;

      procedure Skip_Character is
      begin
         Ensure( not At_End ); --$D
         Cursor := Cursor+1;
      end;

      procedure Get_Time is new Generic_Get_Time;

   begin
      Get_Time( Item, From'Length, Time_Prefix );
      Last := Cursor;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Outputting absolute times =###=

   ------------------------------
   -- =##= To current output =##=

   procedure Put (Item:         in Ada.Calendar.Time;
                  Format:       in Time_Format       := Default_Format;
                  Seconds_Aft:  in Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in Character         := Default_Time_Prefix) is
   begin
      Put( Ada.Text_IO.Current_Output, Item, Format, Seconds_Aft, Time_Prefix );
   end;

   ----------------------
   -- =##= To a file =##=

   procedure Put (File:         in Ada.Text_IO.File_Type;
                  Item:         in Ada.Calendar.Time;
                  Format:       in Time_Format       := Default_Format;
                  Seconds_Aft:  in Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in Character         := Default_Time_Prefix) is

      procedure Put (Item: in Character) is begin Ada.Text_IO.Put( File, Item ); end;

      procedure Put_Time is new Generic_Put_Time;

   begin
      Put_Time( Item, Format, Seconds_Aft, Time_Prefix );
   end;

   ------------------------
   -- =##= To a string =##=

   procedure Put (To:           out String;
                  Item:         in  Ada.Calendar.Time;
                  Format:       in  Time_Format       := Default_Format;
                  Seconds_Aft:  in  Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in  Character         := Default_Time_Prefix) is

      Cursor: Positive range To'Range := To'First;
      At_End: Boolean := To'Last < To'First;

      procedure Put (Item: in Character) is
      begin
         if At_End then raise Layout_Error; end if;
         To(Cursor) := Item;
         if Cursor = To'Last then
            At_End := True;
         else
            Cursor := Cursor+1;
         end if;
      end;

      procedure Put_Time is new Generic_Put_Time;

   begin
      Put_Time( Item, Format, Seconds_Aft, Time_Prefix );
   end;


end Tenet.Calendar_IO;


-------------------------------------------------------------------------------------------------------------------
-- =###= LEGAL INFORMATION =###=

-- The "Tenet Container Library", or "Tenet", is a "Program" as defined in clause 0 of the GPL, and its source code
-- exactly comprises the contents of the accompanying files named in the accompanying file "manifest.txt".

-- "Tenet" is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
-- License as published by the Free Software Foundation; either version 2, or (at your option) any later version.
-- Tenet is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details. You should have received a copy of the GNU General Public License distributed with Tenet; see file
-- "GPL.TXT". If not, write to:

--    Free Software Foundation
--    59 Temple Place - Suite 330
--    Boston, MA 02111-1307, USA

-- or visit the web site:

--    http://www.gnu.org/copyleft/

-- As a special exception, if other files instantiate generics from this unit, or you link this unit with other
-- files to produce an executable, this unit does not by itself cause the resulting executable to be covered by
-- the GNU General Public License. This exception does not however invalidate any other reasons why the executable
-- file might be covered by the GNU General Public License.                                     


-------------------------------------------------------------------------------------------------------------------
-- Tenet Container Library

-- This library is intended to provide a small selection of the most useful kinds of container. A 'container' is an
-- abstract data type whose main purpose is to hold a collection of data items specifically organised in a way most
-- suitable to the way the program uses the data. Each Tenet container is designed to be as general-purpose and
-- easy to use as possible, whilst working efficiently in what is anticipated to be typical usage.

-- Please see the accompanying documentation for more information about this software.


-------------------------------------------------------------------------------------------------------------------
-- =###= Repository Data =###=

-- $Id: tenet-calendar_io.adb,v 1.2 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-calendar_io.adb,v $
-- $RCSfile: tenet-calendar_io.adb,v $

-- $Log: tenet-calendar_io.adb,v $
-- Revision 1.2  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.1  2003/08/06 21:31:48  debater
-- Added Tenet.Calendar_IO package.
--

-- Historical note: I have adapted this package implementation from an old Borland TurboPascal module I wrote many
-- moons ago. This is why some of the identifiers remain in their original all-upper case. [Nick Roberts]


-------------------------------------------------------------------------------------------------------------------
-- End of File.


