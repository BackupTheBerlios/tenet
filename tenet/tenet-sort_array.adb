-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Sort_Array procedure body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Tenet.Debugging; use Tenet.Debugging; --$D


-------------------------------------------------------------------------------------------------------------------
with Tenet.Sort_Pair, Tenet.Sort_Triple;


procedure Tenet.Sort_Array (Source: in out Array_Type) is

   procedure Sort is new Tenet.Sort_Pair(Element_Type);
   procedure Sort is new Tenet.Sort_Triple(Element_Type);
   procedure Swap is new Tenet.Exchange_Pair(Element_Type);

   subtype XT is Index_Type; -- convenient renaming

   Midpoint, Upper, Lower: XT;

begin

   Set_Current_Locus("Tenet.Sort_Array"); --$D

   ----------------------------------------------------------------------------------------------------------------
   -- =###= Different tactics used for array sizes shorter than 2, 2, 3, or longer =###=

   if Source'Length < 2 then
      null; -- nothing to do if array is a singleton or empty
   elsif Source'Length = 2 then
      Sort( Source(Source'First), Source(Source'Last) ); -- sort as a pair
   elsif Source'Length = 3 then
      Sort( Source(Source'First), Source(XT'Succ(Source'First)), Source(Source'Last) ); -- sort as a triple
   else
      ---------------------------------------
      -- =##= Apply merge-sort algorithm =##=

      -- =#= First, divide the array into two halves =#=
      Midpoint := XT'Val( (XT'Pos(Source'First)+XT'Pos(Source'Last))/2 + XT'Pos(Source'First) );
      -- Second, sort each half (recursively):
      Lower := Source'First;
      Upper := XT'Succ(Midpoint);

      -- =#= Sort the two halves =#=
      Sort_Array( Source(Lower..Midpoint) );
      Sort_Array( Source(Upper..Source'Last) );

      -- =#= Finally, merge the two halves =#=
      loop
         if Source(Upper) < Source(Lower) then
            Swap( Source(Lower), Source(Upper) );
            exit when Upper = Source'Last;
            Upper := XT'Succ(Upper);
         else
            exit when Lower = Midpoint;
            Lower := XT'Succ(Lower);
         end if;
      end loop;
      ---------------------
      -- =##= All done =##=

   end if;

   End_Current_Locus; --$D

end Tenet.Sort_Array;


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

-- $Id: tenet-sort_array.adb,v 1.2 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-sort_array.adb,v $
-- $RCSfile: tenet-sort_array.adb,v $

-- $Log: tenet-sort_array.adb,v $
-- Revision 1.2  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.1  2003/08/11 02:16:57  debater
-- Added various small utilities plus the iteration base package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.


