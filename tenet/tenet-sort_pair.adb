-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Sort_Pair procedure body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Tenet.Debugging; use Tenet.Debugging; --$D

with Tenet.Exchange_Pair;


-------------------------------------------------------------------------------------------------------------------
procedure Tenet.Sort_Pair (Item_1, Item_2: in out Element_Type) is

   procedure Swap is new Tenet.Exchange_Pair( Element_Type );

begin

   Set_Current_Locus("Tenet.Sort_Pair"); -- $D

   if Item_2 < Item_1 then Swap( Item_1, Item_2 ); end if;

   End_Current_Locus; -- $D

end Tenet.Sort_Pair;


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

-- $Id: tenet-sort_pair.adb,v 1.1 2003/08/11 02:16:57 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/11 02:16:57 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-sort_pair.adb,v $
-- $RCSfile: tenet-sort_pair.adb,v $

-- $Log: tenet-sort_pair.adb,v $
-- Revision 1.1  2003/08/11 02:16:57  debater
-- Added various small utilities plus the iteration base package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



