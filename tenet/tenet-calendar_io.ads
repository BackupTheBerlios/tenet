-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Calendar_IO package specification =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.Calendar, Ada.Text_IO;


-------------------------------------------------------------------------------------------------------------------
package Tenet.Calendar_IO is

   -- Provides text input and output of values of the type Standard.Duration in ISO 8601 extended calendar format,
   -- excluding any timezone ("CCYY-MM-DDThh:mm:ss.sss"). Input is moderately permissive.

   -- The package Tenet.Calendar_IO.Editing provides facilities for outputting values of the type Ada.Calendar.Time
   -- in a format controlled by a picture string. The package Tenet.Duration_IO provides facilities for inputting
   -- and outputting values of the type Standard.Duration.

   -- See the accompanying Tenet documentation for more details.


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Formats and Default values =###=

   subtype Field is Ada.Text_IO.Field;

   type Time_Format is (YMD, YMDH, YMDHM, YMDHMS);

   Default_Format:       Time_Format       := YMDHMS;
   Default_Seconds_Aft:  Ada.Text_IO.Field := 3;   -- preceding '.' omitted if 0
   Default_Time_Prefix:  Character         := 'T'; -- should be either 'T' or ' ' ('T' preferred)


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Inputting absolute times =###=

   -- =##= From current input =##=

   procedure Get (Item:        out Ada.Calendar.Time;
                  Width:       in  Field     := 0;
                  Time_Prefix: in  Character := Default_Time_Prefix);

   -- =##= From a file =##=

   procedure Get (File:        in  Ada.Text_IO.File_Type;
                  Item:        out Ada.Calendar.Time;
                  Width:       in  Field     := 0;
                  Time_Prefix: in  Character := Default_Time_Prefix);

   -- =##= From a string =##=

   procedure Get (From:        in  String;
                  Item:        out Ada.Calendar.Time;
                  Last:        out Positive;
                  Time_Prefix: in  Character := Default_Time_Prefix);


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Outputting absolute times =###=

   -- =##= To current output =##=

   procedure Put (Item:         in Ada.Calendar.Time;
                  Format:       in Time_Format       := Default_Format;
                  Seconds_Aft:  in Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in Character         := Default_Time_Prefix);

   -- =##= To a file =##=

   procedure Put (File:         in Ada.Text_IO.File_Type;
                  Item:         in Ada.Calendar.Time;
                  Format:       in Time_Format       := Default_Format;
                  Seconds_Aft:  in Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in Character         := Default_Time_Prefix);

   -- =##= To a string =##=

   procedure Put (To:           out String;
                  Item:         in  Ada.Calendar.Time;
                  Format:       in  Time_Format       := Default_Format;
                  Seconds_Aft:  in  Ada.Text_IO.Field := Default_Seconds_Aft;
                  Time_Prefix:  in  Character         := Default_Time_Prefix);


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Exceptions =###=

   Data_Error:   exception renames Ada.Text_IO.Data_Error;
   Layout_Error: exception renames Ada.Text_IO.Layout_Error;


-------------------------------------------------------------------------------------------------------------------
-- =###= Private part =###=

   -- no private part

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

-- $Id: tenet-calendar_io.ads,v 1.2 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-calendar_io.ads,v $
-- $RCSfile: tenet-calendar_io.ads,v $

-- $Log: tenet-calendar_io.ads,v $
-- Revision 1.2  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.1  2003/08/06 21:31:48  debater
-- Added Tenet.Calendar_IO package.
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



