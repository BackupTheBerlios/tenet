-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Interchange_IO generic package specification =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Ada.IO_Exceptions, Ada.Strings.Maps, Ada.Characters.Latin_1;

use Ada.Strings.Maps, Ada.Characters.Latin_1;


-------------------------------------------------------------------------------------------------------------------
generic
   type Element_Type is private;

   type Field_Index is (<>);

   with function Field_Name (Field: in Field_Index) return String is Field_Index'Image;

   Max_Fields_in_File: in Positive := 100;
   Max_Field_Width:    in Positive := 1000;


-------------------------------------------------------------------------------------------------------------------
package Tenet.Interchange_IO is

   -- Provides input and output facilities for an interchange file.

   -- See the accompanying Tenet documentation for more details.


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Interchange file type  and file modes =###=

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Interchange file specifications =###=

   type Mapping_Mode  is (By_Name, By_Position);
   type Quoting_Mode  is (No_Quoting, Minimal_Quoting, Quote_All);
   type Escaping_Mode is (Doubled_Quotes, Escaped_Quotes);

   type Determinable_Item is (Escaping, Delimiter, Quote);
   type Determined_Set is array (Determinable_Item) of Boolean;

   type Specification is
      record
         Mapping:    Mapping_Mode   := By_Name;
         Quoting:    Quoting_Mode   := Minimal_Quoting; -- irrelevant for input
         Trimming:   Boolean        := True;            -- irrelevant for output
         Escaping:   Escaping_Mode  := Doubled_Quotes;  -- determined automatically on input
         Delimiter:  Character      := ',';             -- determined automatically on input
         Quote:      Character      := '"';             -- determined automatically on input
         Escape:     Character      := '\';             -- determined automatically on input
         Determined: Determined_Set := (others => False); -- False items can be set True during input
         Delimiters: Character_Set  := To_Set(',') or To_Set(';'); -- for determining Delimiter on input
         Quotes:     Character_Set  := To_Set('"') or To_Set('''); -- for determining Quote on input
         Escapes:    Character_Set  := To_Set('\');                -- for determining Escape on input
         Whitespace: Character_Set  := To_Set(' ') or To_Set(HT);  -- for trimming unquoted fields on input
      end record;

   Default_Spec: Specification; -- initially takes component default values


   ----------------------------------------------------------------------------------------------------------------
   -- =###= File management =###=

   procedure Create (File: in out File_Type;
                     Mode: in     File_Mode     := Out_File;
                     Name: in     String        := "";
                     Form: in     String        := "";
                     Spec: in     Specification := Default_Spec);

   procedure Open (File: in out File_Type;
                   Mode: in     File_Mode;
                   Name: in     String;
                   Form: in     String        := "";
                   Spec: in     Specification := Default_Spec);

   procedure Reset  (File: in out File_Type);
   procedure Close  (File: in out File_Type);
   procedure Delete (File: in out File_Type);

   function Mode (File: in File_Type) return File_Mode;
   function Name (File: in File_Type) return String;
   function Form (File: in File_Type) return String;
   function Spec (File: in File_Type) return Specification;

   function is_Open (File: in File_Type) return Boolean;

   procedure Flush (File: in out File_Type);


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Detecting end of file =###=

   function End_of_File (File: in File_Type) return Boolean;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Generic package for input =###=

   generic
      with procedure Set_Field (Source: in out Element_Type;
                                Field:  in     Field_Index;
                                Image:  in     String) is <>;

   package Generic_Interchange_Input is

      procedure Read (File: in     File_Type;
                      Item: in out Element_Type);

   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Generic package for output =###=

   generic
      with function Get_Field (Source: in Element_Type;
                               Field:  in Field_Index) return String is <>;

   package Generic_Interchange_Output is

      procedure Write (File: in File_Type;
                       Item: in Element_Type);

   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Exceptions =###=

   Field_Error: exception;

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
	Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
	Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
	Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
	Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
	End_Error    : exception renames Ada.IO_Exceptions.End_Error;
	Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;
	Layout_Error : exception renames Ada.IO_Exceptions.Layout_Error;


-------------------------------------------------------------------------------------------------------------------
-- =###= Private part =###=

private

   type File_Descriptor;

   type Descriptor_Access is access File_Descriptor;

   type File_Type is limited
      record
         Desc: Descriptor_Access := null; -- null when closed
      end record;


-------------------------------------------------------------------------------------------------------------------
end Tenet.Interchange_IO;


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

-- $Id: tenet-interchange_io.ads,v 1.1 2003/08/23 04:10:15 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/23 04:10:15 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-interchange_io.ads,v $
-- $RCSfile: tenet-interchange_io.ads,v $

-- $Log: tenet-interchange_io.ads,v $
-- Revision 1.1  2003/08/23 04:10:15  debater
-- Added Tenet.Interchange_IO package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



