-------------------------------------------------------------------------------------------------------------------
-- Debugging

-- Package Body

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.Finalization, Ada.Calendar, Ada.Command_Line;
with Ada.Text_IO, Ada.Integer_Text_IO; use Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Bounded, Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


-------------------------------------------------------------------------------------------------------------------
package body Debugging is


   ----------------------------------------------------------------------------------------------------------------
   -- Debugging state:

   type Debugging_State is new Ada.Finalization.Limited_Controlled with
      record
         Log:   Ada.Text_IO.File_Type;
         Locus: Unbounded_String;
      end record;

   procedure Finalize (State: in out Debugging_State);

   -- Using a controlled type for the state facilitates finalization (closing the log).

   The_State: Debugging_State;


   ----------------------------------------------------------------------------------------------------------------
   -- Current locus:

   function Current_Locus return String is
   begin
      return To_String( The_State.Locus );
   end;

   procedure Set_Current_Locus (Locus: in String) is
   begin
      The_State.Locus := To_Unbounded_String(Locus);
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Putting a timestamped message on the log:

   procedure Put_Timestamped (File:    in out Ada.Text_IO.File_Type;
                              Message: in     String) is

      use Ada.Calendar, Ada.Text_IO, Ada.Strings.Bounded;

      package DL_IO is new Ada.Text_IO.Integer_IO(Debugging_Level);

      Stamp: constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Year:          Year_Number;
	   Month:         Month_Number;
	   Day_of_Month:  Day_Number;
	   Time_of_Day:   Day_Duration;
	   Sec_of_Day:    Natural range 0 .. 24*60*60;
	   Hour:          Natural range 0..23;
	   Sec_of_Hour:   Natural range 0 .. 60*60;
	   Minute:        Natural range 0..59;
	   Sec_of_Minute: Natural range 0..59;

   begin

      Split(Stamp,Year,Month,Day_of_Month,Time_of_Day);
      Sec_of_Day := Natural(Time_of_Day);
      if Sec_of_Day = 24*60*60 then Sec_of_Day := Sec_of_Day-1; end if; -- if 23:59:60, simply reduce to 59 ;-)
      Hour := Sec_of_Day / (60*60);
      Sec_of_Hour := Sec_of_Day - Hour*60*60;
      Minute := Sec_of_Hour / 60;
      Sec_of_Minute := Sec_of_Hour - Minute*60;

      Put( File, Year, 4 );
      Put( File, '-' );
      Put( File, Month, 2 );
      Put( File, '-' );
      Put( File, Day_of_Month, 2 );
      Put( File, ' ' );
      Put( File, Hour, 2 );
      Put( File, ':' );
      Put( File, Minute, 2 );
      Put( File, ':' );
      Put( File, Sec_of_Minute, 2 );
      Put( File, " " );

      Put( File, "[<Level> " );
      DL_IO.Put( File, Current_Level );
      Put( File, "] " );

      if Current_Locus /= Null_Unbounded_String then
         Put( File, "(<Locus> " );
         Put( File, To_String(The_State.Locus) );
         Put( File, ") " );
      end if;

      Put( File, "--- " );
      Put( File, Message );
      Put( File, " ---" );
      New_Line( File );

   end Put_Timestamped;


   ----------------------------------------------------------------------------------------------------------------
   -- Using a default string if necessary:

   function Defaulted (Value: in String; Default: in String) return String is
   begin
      if Value = "" then
         return Default;
      else
         return Value;
      end if;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Concatenating the program arguments into one string:

   function Catenate_Program_Arguments return String is

      package Buffer_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(4096);
      use Ada.Strings, Buffer_Strings;

      Buffer: Buffer_Strings.Bounded_String;

   begin

      for i in 1..Ada.Command_Line.Argument_Count loop
         if i > 1 then Append( Buffer, ' ', Drop => Right ); end if;
         Append( Buffer, '{', Drop => Right );
         Append( Buffer, Ada.Command_Line.Argument(i), Drop => Right );
         Append( Buffer, '}', Drop => Right );
      end loop;

      return To_String(Buffer);

   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Default log file name and form:

   Default_Log_Name: constant String := "errorlog.txt";
   Default_Log_Form: constant String := "";


   ----------------------------------------------------------------------------------------------------------------
   -- Error log management:

   procedure Open_Log (Name: in String := ""; Form: in String := "") is

      Defaulted_Name: constant String := Defaulted(Name,Default_Log_Name);
      Defaulted_Form: constant String := Defaulted(Name,Default_Log_Form);

   begin

      -- Try to open an existing file to append, otherwise create a new one:
      begin
         Open( File => The_State.Log,
               Name => Defaulted_Name,
               Mode => Append_File,
               Form => Defaulted_Form );
      exception
         when Name_Error =>
            Create( File => The_State.Log,
                    Name => Defaulted_Name,
                    Mode => Append_File, -- or Out_File?
                    Form => Defaulted_Form );
      end;

      New_Line(The_State.Log); -- start with a blank line (in case we are appending)

      Put_Timestamped( The_State.Log,
                       "Log opened [" &
                       Ada.Command_Line.Command_Name &
                       "] " &
                       Catenate_Program_Arguments &
                       " ... " );

   end Open_Log;


   procedure Close_Log is
   begin
      Put_Timestamped( The_State.Log, "Log closed (explicitly). " );
      Close(The_State.Log);
   end;


   function Log_is_Open return Boolean is begin return is_Open(The_State.Log); end;

   function Log_Name return String is
   begin
      if is_Open(The_State.Log) then
         return Name(The_State.Log);
      else
         return "";
      end if;
   end;


   function Log_Form return String is
   begin
      if is_Open(The_State.Log) then
         return Form(The_State.Log);
      else
         return "";
      end if;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Finalization of debugging state:

   procedure Finalize (State: in out Debugging_State) is
   begin

      if is_Open(State.Log) then
         Put_Timestamped( State.Log, "Log closed (automatically). " );
         Close(State.Log);
      end if;

      Ada.Finalization.Finalize( Ada.Finalization.Limited_Controlled(State) ); -- to be ultra-pedantic

   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Ensuring the log is open:

   procedure Ensure_Log_is_Open is
   begin
      if not Log_is_Open then Open_Log; end if;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- Reporting an error:

   procedure Error (Message: in String := "Unspecified";
                    Level:   in Debugging_Level := 0) is
   begin

      if Current_Level >= Level then
         Ensure_Log_is_Open;
         Put_Timestamped( The_State.Log, "ERROR *** " & Message );
         raise Debugging_Error;
      end if;

   end Error;
   

   ----------------------------------------------------------------------------------------------------------------
   -- Checking for errors:

   procedure Ensure (Condition: in Boolean;
                     Message:   in String := "Unspecified";
                     Level:     in Debugging_Level := 1) is
   begin

      if Current_Level >= Level then
         if not Condition then
            Error(Message,Level);
         end if;
      end if;

   end;
   

   ----------------------------------------------------------------------------------------------------------------
   -- Log messages:

   procedure Note (Message: in String;
                   Level:   in Debugging_Level := 1) is
   begin

      if Current_Level >= Level then
         Ensure_Log_is_Open;
         Put_Timestamped( The_State.Log, Message );
      end if;

   end;


end Debugging;


-------------------------------------------------------------------------------------------------------------------
-- LEGAL INFORMATION

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
-- Repository Data

-- $Id: debugging.adb,v 1.2 2003/08/02 04:08:40 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2003/08/02 04:08:40 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/Attic/debugging.adb,v $
-- $RCSfile: debugging.adb,v $

-- $Log: debugging.adb,v $
-- Revision 1.2  2003/08/02 04:08:40  debater
-- First successful test run
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



