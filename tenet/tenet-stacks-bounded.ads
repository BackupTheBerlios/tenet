-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Stacks.Bounded package specification =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.Streams; -- for private part only


-------------------------------------------------------------------------------------------------------------------
package Tenet.Stacks.Bounded is


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Generic bounded-length stacks package =###=

   generic
      type Element_Type is private;
      Max: Positive;

   package Generic_Bounded_Length is

      type Stack is limited private;

      function Depth (Source: in Stack) return Natural;

      function is_Null (Source: in Stack) return Boolean;

      function Top (Source: in Stack) return Element_Type;

      procedure Clear (Source: in out Stack);

      procedure Push (Source: in out Stack; Item: in  Element_Type);
      procedure Pop  (Source: in out Stack; Item: out Element_Type);

      procedure Pop_and_Discard (Source: in out Stack; How_Many: in Positive := 1);
      procedure Copy_Top        (Source: in out Stack; How_Many: in Positive := 1);

      procedure Copy_Top_Slice (Source: in out Stack; Length: in Positive);

      Stack_Error: exception renames Tenet.Stacks.Stack_Error;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Private part of Tenet.Stacks.Bounded =###=

   private

      subtype Depth_Count is Positive range 0..Max;
      subtype Array_Index is Positive range 1..Max;

      type Stack_Array is array (Array_Index) of Element_Type;

      type Stack is
         record
            Data:  Stack_Array;
            Depth: Depth_Count := 0; -- number on stack
         end record;

      procedure Write_Stack (Stream:    access Ada.Streams.Root_Stream_Type'Class;
                             Container: in     Stack);

      procedure Read_Stack (Stream:    access Ada.Streams.Root_Stream_Type'Class;
                            Container: out    Stack);

      for Stack'Write use Write_Stack;
      for Stack'Read  use Read_Stack;

   end Generic_Bounded_Length;

end Tenet.Stacks.Bounded;


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

-- $Id: tenet-stacks-bounded.ads,v 1.3 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.3 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-stacks-bounded.ads,v $
-- $RCSfile: tenet-stacks-bounded.ads,v $

-- $Log: tenet-stacks-bounded.ads,v $
-- Revision 1.3  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.2  2003/08/10 17:49:49  debater
-- Added bounded stacks package.
--
-- Revision 1.1  2003/08/03 19:03:47  debater
-- Still just populating the module. Early days.
--
-- Revision 1.2  2003/08/02 22:25:28  debater
-- Improved 'Debugging' package, and testing.
-- Added my own test framework (for Windows 95).
-- Added the readme and maint files.
-- Made various small improvements.
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



