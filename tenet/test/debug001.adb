-------------------------------------------------------------------------------------------------------------------
-- debug001

-- Procedure Body

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Debugging; use Debugging;
with Ada.Text_IO; use Ada.Text_IO;

procedure debug001 is
begin
   Set_Current_Locus( "debug001" );
   Note( "About to test Debugging.Ensure" );
   Ensure( True, "failed (1)" );
   begin
      Ensure( False, "triggered correctly" );
   exception
      when Debugging_Error =>
         Put( "Debugging.Debugging_Error raised correctly" );
         return;
   end;
   Error( "failed (2)" );
end;


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

-- $Id: debug001.adb,v 1.1 2003/08/02 22:25:28 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/02 22:25:28 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/test/debug001.adb,v $
-- $RCSfile: debug001.adb,v $

-- $Log: debug001.adb,v $
-- Revision 1.1  2003/08/02 22:25:28  debater
-- Improved 'Debugging' package, and testing.
-- Added my own test framework (for Windows 95).
-- Added the readme and maint files.
-- Made various small improvements.
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



