-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Debugging package specification =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
package Tenet.Debugging is

   -- Provides basic aids in debugging Ada programs.

   -- See the accompanying Tenet documentation for more details.


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Debugging levels and locus =###=

   subtype Debugging_Level is Integer range 0..7;

   -- 0 means none, 7 means highest level of debugging
   
   Current_Level: Debugging_Level := Debugging_Level'Last;

   function Current_Locus return String;

   procedure Set_Current_Locus (Locus: in String);

   procedure End_Current_Locus; -- restores previous


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Reporting an error =###=

   procedure Error (Message: in String := "Unspecified";
                    Level:   in Debugging_Level := 0);

   -- if Current_Level >= Level, logs message and raises Debugging_Error
   

   ----------------------------------------------------------------------------------------------------------------
   -- =###= Checking for errors =###=

   procedure Ensure (Condition: in Boolean;
                     Message:   in String := "Unspecified";
                     Level:     in Debugging_Level := 1);

   -- if Current_Level >= Level, if Condition=True then Error(Message,Level)
   
   pragma Inline(Ensure);
   

   ----------------------------------------------------------------------------------------------------------------
   -- =###= Log messages =###=

   procedure Note (Message: in String;
                   Level:   in Debugging_Level := 1);

   -- if Current_Level >= Level, writes Message into log
   

   ----------------------------------------------------------------------------------------------------------------
   -- =###= Error log management =###=

   procedure Open_Log (Name: in String := "";  -- "" for default name
                       Form: in String := ""); -- "" for default form

   -- log is opened automatically when necessary

   procedure Close_Log;

   -- log is closed automatically at termination of program
   
   function Log_is_Open return Boolean;

   function Log_Name return String;
   function Log_Form return String;

   ----------------------------------------------------------------------------------------------------------------
   -- =###= Exceptions =###=

   Debugging_Error: exception;
   

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

-- $Id: tenet-debugging.ads,v 1.2 2003/08/10 17:49:49 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2003/08/10 17:49:49 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-debugging.ads,v $
-- $RCSfile: tenet-debugging.ads,v $

-- $Log: tenet-debugging.ads,v $
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


