-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Stacks.Bounded package body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Tenet.Debugging; use Tenet.Debugging; --$D


-------------------------------------------------------------------------------------------------------------------
package body Tenet.Stacks.Bounded is


   package body Generic_Bounded_Length is


      function Depth (Source: in Stack) return Natural is
      begin
         return Source.Depth;
      end;


      function is_Null (Source: in Stack) return Boolean is
      begin
         return Source.Depth = 0;
      end;


      function Top (Source: in Stack) return Element_Type is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Top"); --$D
         if Source.Depth = 0 then raise Stack_Error; end if;
         End_Current_Locus; --$D
         return Source.Data(Source.Depth);
      end;


      procedure Clear (Source: in out Stack) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Clear"); --$D
         Source.Depth := 0;
         End_Current_Locus; --$D
      end;


      procedure Push (Source: in out Stack; Item: in  Element_Type) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Push"); --$D
         if Source.Depth = Max then raise Stack_Error; end if;
         Source.Depth := Source.Depth + 1;
         Source.Data(Source.Depth) := Item;
         End_Current_Locus; --$D
      end;


      procedure Pop  (Source: in out Stack; Item: out Element_Type) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Pop"); --$D
         if Source.Depth = 0 then raise Stack_Error; end if;
         Item := Source.Data(Source.Depth);
         Source.Depth := Source.Depth - 1;
         End_Current_Locus; --$D
      end;
         

      procedure Pop_and_Discard (Source: in out Stack; How_Many: in Positive := 1) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Pop_and_Discard"); --$D
         if How_Many > Source.Depth then raise Stack_Error; end if;
         Source.Depth := Source.Depth - How_Many;
         End_Current_Locus; --$D
      end;


      procedure Copy_Top (Source: in out Stack; How_Many: in Positive := 1) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Copy_Top"); --$D
         if Source.Depth > Max - How_Many then raise Stack_Error; end if;
         Source.Data( Source.Depth+1 .. Source.Depth+How_Many ) := (others => Item);
         Source.Depth := Source.Depth + How_Many;
         End_Current_Locus; --$D
      end;


      procedure Copy_Top_Slice (Source: in out Stack; Length: in Positive) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Copy_Top_Slice"); --$D
         if (Length > Source.Depth) or (Source.Depth > Max - Length) then raise Stack_Error; end if;
         Source.Data( Source.Depth+1 .. Source.Depth+Length ) := Source.Data( Source.Depth-Length+1 .. Source.Depth );
         Source.Depth := Source.Depth + Length;
         End_Current_Locus; --$D
      end;


      procedure Write_Stack (Stream:    access Ada.Streams.Root_Stream_Type'Class;
                             Container: in     Stack) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Write_Stack"); --$D
         Integer'Write(Stream,Container.Depth);
         for i in reverse 1..Container.Depth loop
            Element_Type'Write(Stream,Container.Data(i));
         end loop;
         End_Current_Locus; --$D
      end;


      procedure Read_Stack (Stream:    access Ada.Streams.Root_Stream_Type'Class;
                            Container: out    Stack) is
      begin
         Set_Current_Locus("Tenet.Stacks.Bounded.Read_Stack"); --$D
         Integer'Read(Stream,Container.Depth);
         for i in reverse 1..Container.Depth loop
            Element_Type'Read(Stream,Container.Data(i));
         end loop;
         End_Current_Locus; --$D
      end;


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

-- $Id: tenet-stacks-bounded.adb,v 1.1 2003/08/10 17:49:49 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/10 17:49:49 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-stacks-bounded.adb,v $
-- $RCSfile: tenet-stacks-bounded.adb,v $

-- $Log: tenet-stacks-bounded.adb,v $
-- Revision 1.1  2003/08/10 17:49:49  debater
-- Added bounded stacks package.
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.


