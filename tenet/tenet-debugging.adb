-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Debugging package body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.Finalization, Ada.Calendar, Ada.Command_Line;
with Ada.Text_IO, Ada.Integer_Text_IO; use Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Bounded, Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


-------------------------------------------------------------------------------------------------------------------
package body Tenet.Debugging is


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Simple string stacks =###=

   type String_Array is array (Positive range <>) of Unbounded_String;

   type String_Stack (Max: Positive) is
      record
         ToS: Natural := 0;
         Arr: String_Array(1..Max);
      end record;

   procedure Push (Stk: in out String_Stack;
                   Str: in     String) is
   begin
      if Stk.ToS = Stk.Max then raise Program_Error; end if;
      Stk.ToS := Stk.ToS+1;
      Stk.Arr(Stk.ToS) := To_Unbounded_String(Str);
   end;


   procedure Pop_and_Discard (Stk: in out String_Stack) is
   begin
      if Stk.ToS = 0 then raise Program_Error; end if;
      Stk.Arr(Stk.ToS) := Null_Unbounded_String; -- to free memory (?)
      Stk.ToS := Stk.ToS-1;
   end;


   function Top (Stk: in String_Stack) return String is
   begin
      if Stk.ToS = 0 then
         return "";
      else
         return To_String( Stk.Arr(Stk.ToS) );
      end if;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Debugging state =###=

   type Debugging_State is new Ada.Finalization.Limited_Controlled with
      record
         Log:  Ada.Text_IO.File_Type;
         Loci: String_Stack(20);
      end record;

   procedure Finalize (State: in out Debugging_State);

   -- Using a controlled type for the state facilitates finalization (closing the log).

   The_State: Debugging_State;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Current locus =###=

   function Current_Locus return String is
   begin
      return Top(The_State.Loci);
   end;

   procedure Set_Current_Locus (Locus: in String) is
   begin
      Push( The_State.Loci, Locus );
   end;

   procedure End_Current_Locus is
   begin
      Pop_and_Discard( The_State.Loci );
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Putting a timestamped message on the log =###=

   -- NB: timestamping removed for time being, to help testing, and because I think Tenet.Calendar_IO should be used instead [NJR 2aug03]

   procedure Put_Timestamped (File:    in out Ada.Text_IO.File_Type;
                              Message: in     String) is

--      use Ada.Calendar;
      use Ada.Text_IO, Ada.Strings.Bounded;

      package DL_IO is new Ada.Text_IO.Integer_IO(Debugging_Level);

--      Stamp: constant Ada.Calendar.Time := Ada.Calendar.Clock;
--      Year:          Year_Number;
--      Month:         Month_Number;
--      Day_of_Month:  Day_Number;
--      Time_of_Day:   Day_Duration;
--      Sec_of_Day:    Natural range 0 .. 24*60*60;
--      Hour:          Natural range 0..23;
--      Sec_of_Hour:   Natural range 0 .. 60*60;
--      Minute:        Natural range 0..59;
--      Sec_of_Minute: Natural range 0..59;

   begin

--      Split(Stamp,Year,Month,Day_of_Month,Time_of_Day);
--      Sec_of_Day := Natural(Time_of_Day);
--      if Sec_of_Day = 24*60*60 then Sec_of_Day := Sec_of_Day-1; end if; -- if 23:59:60, simply reduce to 59 ;-)
--      Hour := Sec_of_Day / (60*60);
--      Sec_of_Hour := Sec_of_Day - Hour*60*60;
--      Minute := Sec_of_Hour / 60;
--      Sec_of_Minute := Sec_of_Hour - Minute*60;

--      Put( File, Year, 4 );
--      Put( File, '-' );
--      Put( File, Month, 2 );
--      Put( File, '-' );
--      Put( File, Day_of_Month, 2 );
--      Put( File, ' ' );
--      Put( File, Hour, 2 );
--      Put( File, ':' );
--      Put( File, Minute, 2 );
--      Put( File, ':' );
--      Put( File, Sec_of_Minute, 2 );
--      Put( File, " " );

      Put( File, "[<Level> " );
      DL_IO.Put( File, Current_Level, 0 );
      Put( File, "] " );

      if Current_Locus /= Null_Unbounded_String then
         Put( File, "(<Locus> " );
         Put( File, Current_Locus );
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
   -- =###= Concatenating the program arguments into one string =###=

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
   -- =###= Default log file name and form =###=

   Default_Log_Name: constant String := "errorlog.txt";
   Default_Log_Form: constant String := "";


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Error log management =###=

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
                       Catenate_Program_Arguments );

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
   -- =###= Finalization of debugging state =###=

   procedure Finalize (State: in out Debugging_State) is
   begin

      if is_Open(State.Log) then
         Put_Timestamped( State.Log, "Log closed (automatically). " );
         Close(State.Log);
      end if;

      Ada.Finalization.Finalize( Ada.Finalization.Limited_Controlled(State) ); -- to be ultra-pedantic

   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Ensuring the log is open =###=

   procedure Ensure_Log_is_Open is
   begin
      if not Log_is_Open then Open_Log; end if;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Reporting an error =###=

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
   -- =###= Checking for errors =###=

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
   -- =###= Log messages =###=

   procedure Note (Message: in String;
                   Level:   in Debugging_Level := 1) is
   begin

      if Current_Level >= Level then
         Ensure_Log_is_Open;
         Put_Timestamped( The_State.Log, Message );
      end if;

   end;


end Tenet.Debugging;


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

-- $Id: tenet-debugging.adb,v 1.3 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.3 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-debugging.adb,v $
-- $RCSfile: tenet-debugging.adb,v $

-- $Log: tenet-debugging.adb,v $
-- Revision 1.3  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.2  2003/08/10 17:49:49  debater
-- Added bounded stacks package.
--
--
-- Revision 1.1  2003/08/03 19:03:47  debater
-- Still just populating the module. Early days.
--
-- Revision 1.3  2003/08/02 22:25:28  debater
-- Improved 'Debugging' package, and testing.
-- Added my own test framework (for Windows 95).
-- Added the readme and maint files.
-- Made various small improvements.
--
-- Revision 1.2  2003/08/02 04:08:40  debater
-- First successful test run
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.


