-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Iteration package specification =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
generic
   type Element_Type is private; 


-------------------------------------------------------------------------------------------------------------------
package Tenet.Iteration is

   -- Defines a set of abstract iterator types.

   -- See the accompanying Tenet documentation for more details.

   pragma Pure;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Non-terminating, terminating, & reproducible sequential sources =###=

   ----------------------------------------------
   -- =##= Non-terminating sequential source =##=

   type Indefinite_Producer is abstract tagged limited private;

   procedure Read (Source: in out Indefinite_Producer;
                   Item:   out    Element_Type) is abstract;

   ------------------------------------------
   -- =##= Terminating sequential source =##=

   type Sequence_Producer is abstract new Indefinite_Producer with private;

   function End_Of_Data (Source: in Sequence_Producer) return Boolean is abstract;

   -------------------------------------------
   -- =##= Reproducible sequential source =##=

   type Sequence_Reproducer is abstract new Sequence_Producer with private;

   procedure Restart (Source: in out Sequence_Reproducer) is abstract;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Non-terminating, terminating, & re-writable sequential sinks =###=

   -------------------------------------------
   -- =##= Non-terminating sequential sink =##=

   type Indefinite_Consumer is abstract tagged limited private; 

   procedure Write (Target: in out Indefinite_Consumer;
                    Item:   in      Element_Type) is abstract;

   ----------------------------------------
   -- =##= Terminating sequential sink =##=

   type Sequence_Consumer is abstract new Indefinite_Consumer with private; 

   procedure Terminate_Data (Target: in out Sequence_Consumer) is abstract;

   ----------------------------------------
   -- =##= Re-writable sequential sink =##=

   type Sequence_Recorder is abstract new Sequence_Consumer with private; 

   procedure Rewrite (Target: in out Sequence_Recorder) is abstract;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Basic utility operations (which never call Restart or Rewrite) =###=

   -------------------------------------------------------------------
   -- =##= Copy items from source to sink until source terminates =##=

   procedure Copy_All (Source: in out Sequence_Producer'Class;
                       Target: in out Indefinite_Consumer'Class);

   ---------------------------------------------------------------------------------------
   -- =##= Copy until source terminates or a certain number of items have been copied =##=

   procedure Copy (Source: in out Indefinite_Producer'Class;
                   Target: in out Indefinite_Consumer'Class;
                   Count:  in     Natural);

   ------------------------------------------------------------------------
   -- =##= Read (and forget) items from source until source terminates =##=

   procedure Discard_All (Source: in out Sequence_Producer'Class);

   ----------------------------------------------------------------------------------------------------------
   -- =##= Read (and forget) items until source terminates or a certain number of items have been copied =##=

   procedure Discard (Source: in out Indefinite_Producer'Class;
                      Count:  in     Natural := 1);

   -----------------------------------------------------------------------
   -- =##= Write a certain number of a single given value into a sink =##=

   procedure Fill (Target: in out Indefinite_Consumer'Class;
                   Item:   in     Element_Type;
                   Count:  in     Natural);


-------------------------------------------------------------------------------------------------------------------
-- =###= Private part =###=

private

   type Indefinite_Producer is abstract tagged limited null record; 
   type Indefinite_Consumer is abstract tagged limited null record; 

   type Sequence_Producer   is abstract new Indefinite_Producer with null record; 
   type Sequence_Consumer   is abstract new Indefinite_Consumer with null record; 
   type Sequence_Reproducer is abstract new Sequence_Producer   with null record; 
   type Sequence_Recorder   is abstract new Sequence_Consumer   with null record; 

end Tenet.Iteration;


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

-- $Id: tenet-iteration.ads,v 1.1 2003/08/11 02:16:57 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/11 02:16:57 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-iteration.ads,v $
-- $RCSfile: tenet-iteration.ads,v $

-- $Log: tenet-iteration.ads,v $
-- Revision 1.1  2003/08/11 02:16:57  debater
-- Added various small utilities plus the iteration base package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



