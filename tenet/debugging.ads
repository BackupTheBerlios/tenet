-------------------------------------------------------------------------------------------------------------------
-- Debugging

-- Package Specification

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.Strings.Unbounded;


-------------------------------------------------------------------------------------------------------------------
package Debugging is


   ----------------------------------------------------------------------------------------------------------------
   -- Debugging levels and locus:

   subtype Debugging_Level is Integer range 0..7;

   -- 0 means none, 7 means highest level of debugging
   
   Current_Level: Debugging_Level := Debugging_Level'Last;
   Current_Locus: Ada.Strings.Unbounded.Unbounded_String; -- initially null
   

   ----------------------------------------------------------------------------------------------------------------
   -- Reporting an error:

   procedure Error (Message: in String := "Unspecified";
                    Level:   in Debugging_Level := 0);

   -- if Current_Level >= Level, logs message and raises Debugging_Error
   

   ----------------------------------------------------------------------------------------------------------------
   -- Checking for errors:

   procedure Ensure (Condition: in Boolean;
                     Message:   in String := "Unspecified";
                     Level:     in Debugging_Level := 1);

   -- if Current_Level >= Level, if Condition=True then Error(Message,Level)
   
   pragma Inline(Ensure);
   

   ----------------------------------------------------------------------------------------------------------------
   -- Log messages:

   procedure Note (Message: in String;
                   Level:   in Debugging_Level := 1);

   -- if Current_Level >= Level, writes Message into log
   

   ----------------------------------------------------------------------------------------------------------------
   -- Error log management:

   procedure Open_Log (Name: in String := "";  -- "" for default name
                       Form: in String := ""); -- "" for default form

   -- log is opened automatically when necessary

   procedure Close_Log;

   -- log is closed automatically at termination of program
   
   function Log_is_Open return Boolean;

   function Log_Name return String;
   function Log_Form return String;

   ----------------------------------------------------------------------------------------------------------------
   -- Exceptions:

   Debugging_Error: exception;
   

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

-- $Id: debugging.ads,v 1.1 2003/08/01 21:21:32 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/01 21:21:32 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/Attic/debugging.ads,v $
-- $RCSfile: debugging.ads,v $

-- $Log: debugging.ads,v $
-- Revision 1.1  2003/08/01 21:21:32  debater
-- Initial revision
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



