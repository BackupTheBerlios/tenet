-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Interchange_IO package body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
with Tenet.Debugging; use Tenet.Debugging; --$D

with Ada.Unchecked_Deallocation;

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Characters.Handling, Ada.Characters.Latin_1;
use  Ada.Characters.Handling, Ada.Characters.Latin_1;

with Ada.Strings.Maps, Ada.Strings.Maps.Constants, Ada.Strings.Fixed;
use  Ada.Strings.Maps, Ada.Strings.Maps.Constants, Ada.Strings.Fixed;

with Tenet.Cyclic_Succ;

with Ada.Text_IO; use Ada.Text_IO;


-------------------------------------------------------------------------------------------------------------------
package body Tenet.Interchange_IO is


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Field numbering and mapping =###=


   Field_Name_Characters: constant Character_Set := Letter_Set or Decimal_Digit_Set or To_Set('_');

   subtype Field_Count  is Natural  range 0 .. Max_Fields_in_File;
   subtype Field_Number is Positive range 1 .. Field_Count'Last;

   type Number_to_Index_Mapping is array (Field_Number) of Field_Index;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= File descriptors =###=


   type Field_Set_by_Index  is array (Field_Index)  of Boolean;
   type Field_Set_by_Number is array (Field_Number) of Boolean;

   Null_Field_Set_by_Index: constant Field_Set_by_Index := (others => False);


   type File_Descriptor is limited
      record
         File: Ada.Text_IO.File_Type;
         Mode: File_Mode;
         Spec: Specification;
         FinF: Field_Count;             -- number of fields in input file
         Used: Field_Set_by_Number;     -- fields in input file which are mapped to internal fields
         NtoI: Number_to_Index_Mapping; -- used for input file only
      end record;


   procedure Free is new Ada.Unchecked_Deallocation( File_Descriptor, Descriptor_Access );


   procedure Check_is_Open (File: in File_Type) is
   begin
      if File.Desc = null then raise Status_Error; end if;
   end;

   pragma Inline(Check_is_Open);


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Exception information =###=


   function ATIOCI (N: in Ada.Text_IO.Count) return String renames Ada.Text_IO.Count'Image;


   function File_Info (File: in File_Type) return String is

      Name: constant String := "file " & '"' & Ada.Text_IO.Name(File.Desc.File) & '"';
      Page: constant String := "page" & ATIOCI( Ada.Text_IO.Page(File.Desc.File) );
      Line: constant String := "line" & ATIOCI( Ada.Text_IO.Line(File.Desc.File) );

   begin
      if Ada.Text_IO.Page(File.Desc.File) /= 1 then
         return Name & ", " & Page & ", " & Line;
      else
         return Name & ", " & Line;
      end if;
   end;


   procedure Raise_Data_Error (File:    in File_Type;
                               Message: in String := "Invalid data format") is
   begin
      Raise_Exception( Data_Error'Identity, Message & " [" & File_Info(File) & "]" );
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= String functions =###=


   function Contains_Any (Source: in String;
                          Set:    in Character_Set) return Boolean is
   begin
      for i in Source'Range loop
         if is_in( Source(i), Set ) then return True; end if;
      end loop;
      return False;
   end;


   function is_Trimmable (Source:  in String;
                          Discard: in Character_Set) return Boolean is
   begin
      return Source'Length /= 0 and then
         ( is_in( Source(Source'First), Discard ) or is_in( Source(Source'Last), Discard ) );
   end;


   function Are_Disjunct (Left, Right: in Character_Set) return Boolean is
   begin
      return (Left and Right) = Null_Set;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Dealing with whitespace =###=


   procedure Skip_Whitespace (File: in File_Type) is

      Char: Character;
      EoL:  Boolean; -- end of line flag

   begin
      loop
         Look_Ahead( File.Desc.File, Char, EoL );
         exit when EoL or not is_in( Char, File.Desc.Spec.Whitespace );
         Get( File.Desc.File, Char ); -- consume character
      end loop;
   end;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Putting and getting field values =###=


   ------------------------------
   -- =##= Get a field value =##=

   procedure Get_Field_Value (File:     in  File_Type;
                              Item:     out String;
                              Last:     out Natural;
                              Was_Last: out Boolean) is

      FDS:  Specification renames File.Desc.Spec;
      FDF:  Ada.Text_IO.File_Type renames File.Desc.File;
      i:    Natural := Item'First-1;
      Char: Character;
      EoL:  Boolean; -- end of line flag

      -- =#= Consume an escape character, and check end of line not reached =#=
      procedure Get_Escape is
      begin
         Get( FDF, Char );
         if End_of_Line(FDF) then Raise_Data_Error( File, "Character expected after escape" ); end if;
      end;

      -- =#= Get the remainder of a quoted field =#=
      procedure Get_Quoted is
      begin

         loop

            if End_of_Line(FDF) then Raise_Data_Error( File, "Quoted field not completed" ); end if;
            Get( FDF, Char );

            -- =#= Handle a special character =#=
            if FDS.Determined(Escaping) then

               case FDS.Escaping is
                  when Doubled_Quotes =>
                     if Char = FDS.Quote then
                        Look_Ahead( FDF, Char, EoL );
                        exit when EoL or Char /= FDS.Quote; -- end of field unless doubled quote
                        Get( FDF, Char ); -- consume one of the quotes
                     end if;
                  when Escaped_Quotes => exit; -- end of field
               end case;

            elsif Char = FDS.Quote then

               Look_Ahead( FDF, Char, EoL );
               exit when EoL or Char /= FDS.Quote; -- end of field unless doubled quote
               FDS.Determined(Escaping) := True;
               FDS.Escaping := Doubled_Quotes;
               Get( FDF, Char ); -- consume one of the quotes

            elsif is_in( Char, FDS.Escapes ) then

               FDS.Determined(Escaping) := True;
               FDS.Escaping := Escaped_Quotes;
               FDS.Escape := Char;
               Get_Escape; -- consume escape character

            end if;

            -- =#= Add character to result string =#=
            i := i+1;
            Item(i) := Char;

         end loop;

         -- =#= Skip remaining whitespace =#=
         Skip_Whitespace( File );

         -- =#= Skip delimiter, if not end of line =#=
         if not End_of_Line(FDF) then
            Get( FDF, Char );
            if FDS.Determined(Delimiter) then
               if Char /= FDS.Delimiter then Raise_Data_Error( File, "Delimiter expected" ); end if;
            elsif is_in( Char, FDS.Delimiters ) then
               FDS.Determined(Delimiter) := True;
               FDS.Delimiter := Char;
            else
               Raise_Data_Error( File, "Delimiter expected" );
            end if;
         end if;

      end Get_Quoted;

      -- =#= Get the remainder of an unquoted field =#=
      procedure Get_Unquoted is
      begin

         loop

            -- =#= Test for delimiter =#=
            if FDS.Determined(Delimiter) then
               exit when Char = FDS.Delimiter;
            elsif is_in( Char, FDS.Delimiters ) then
               FDS.Determined(Delimiter) := True;
               FDS.Delimiter := Char;
               exit;
            end if;

            -- =#= Handle any escaping =#=
            if FDS.Determined(Escaping) then
               if Char = FDS.Escape then
                  Get_Escape; -- consume escape character
               end if;
            elsif is_in( Char, FDS.Escapes ) then
               FDS.Determined(Escaping) := True;
               FDS.Escaping := Escaped_Quotes;
               FDS.Escape := Char;
               Get_Escape; -- consume escape character
            end if;

            -- =#= Add character to result string =#=
            i := i+1;
            Item(i) := Char;

            -- =#= Get next character, if not end of line =#=
            exit when End_of_Line(FDF);
            Get( FDF, Char );

         end loop;

         -- =#= Remove trailing whitespace =#=
         if FDS.Trimming then
            while i > Item'First and then is_in( Item(i), FDS.Whitespace ) loop
               i := i-1;
            end loop;
         end if;

      end Get_Unquoted;

   begin

      -- =#= Skip initial whitespace =#=
      if FDS.Trimming then Skip_Whitespace( File ); end if;

      -- =#= Check for empty field at end of line =#=
      if End_of_File(FDF) then
         Skip_Line( FDF );
         Was_Last := True;
         Last := 0;
         return;
      end if;

      -- =#= Get first character of field =#=
      Get( FDF, Char );

      -- =#= Get field value (quoted or unquoted) =#=
      if FDS.Determined(Quote) then
         if Char = FDS.Quote then
            Get_Quoted;
         else
            Get_Unquoted;
         end if;
      else
         if is_in( Char, FDS.Quotes ) then
            FDS.Determined(Quote) := True;
            FDS.Quote := Char;
            Get_Quoted;
         else
            Get_Unquoted;
         end if;
      end if;

      -- =#= Determine if was last field in line (and if so, skip to next line) =#=
      if End_of_Line(FDF) then
         Skip_Line( FDF );
         Was_Last := True;
      else
         Was_Last := False;
      end if;

      -- =#= Set the out parameter which gives how many characters are in the result string =#=
      Last := i;

      Put( Standard_Error, "GOT '" & Item(1..Last) & ''' );         --$D
      if Was_Last then Put( Standard_Error, " WAS LAST" ); end if;  --$D
      New_Line( Standard_Error );                                   --$D

   end Get_Field_Value;
         

   ------------------------------
   -- =##= Put a field value =##=

   procedure Put_Field_Value (File:    in File_Type;
                              Item:    in String;
                              is_Last: in Boolean) is

      -- is_Last indicates whether this value is the last in the record (so EoL instead of delimiter needed at end)

      FDS: Specification renames File.Desc.Spec;
      FDF: Ada.Text_IO.File_Type renames File.Desc.File;

      -- =#= Test for a special character (delimiter, quote, or escape) =#=
      function is_Special (Char: in Character) return Boolean is
      begin
         return Char = FDS.Delimiter or Char = FDS.Quote or Char = FDS.Escape;
      end;

      -- =#= Test if a strings contains a special character anywhere =#=
      function Contains_Special (Str: in String) return Boolean is
      begin
         for i in Str'Range loop
            if is_Special(Str(i)) then return True; end if;
         end loop;
         return False;
      end;

      -- =#= Put the field value unquoted =#=
      procedure Put_Unquoted is
      begin
         for i in Item'Range loop
            if is_Special(Item(i)) then Put( FDF, FDS.Escape ); end if;
            Put( FDF, Item(i) );
         end loop;
      end;

      -- =#= Put the field value quoted =#=
      procedure Put_Quoted is
      begin
         Put( FDF, FDS.Quote );
         for i in Item'Range loop
            if Item(i) = FDS.Quote then
               case FDS.Escaping is
                  when Doubled_Quotes => Put( FDF, FDS.Quote  );
                  when Escaped_Quotes => Put( FDF, FDS.Escape );
               end case;
            elsif Item(i) = FDS.Escape and FDS.Escaping = Escaped_Quotes then
               Put( FDF, FDS.Escape );
            end if;
            Put( FDF, Item(i) );
         end loop;
         Put( FDF, FDS.Quote );
      end;

      -- =#= Test the field value, and put it quoted if it needs to be, otherwise unquoted =#=
      procedure Put_Quoted_if_Necessary is
      begin
         if Contains_Special(Item) or else is_Trimmable(Item,FDS.Whitespace) then
            Put_Quoted;
         else
            Put_Unquoted;
         end if;
      end;

   begin

      -- =#= Put the field according to the quoting mode =#=
      case FDS.Quoting is
         when No_Quoting      => Put_Unquoted;
         when Minimal_Quoting => Put_Quoted_if_Necessary;
         when Quote_All       => Put_Quoted;
      end case;

      -- =#= Put the final delimiter or line break =#=
      if is_Last then
         New_Line( FDF );
      else
         Put( FDF, FDS.Delimiter );
      end if;

   end Put_Field_Value;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Package body for input =###=

   package body Generic_Interchange_Input is

      procedure Read (File: in     File_Type;
                      Item: in out Element_Type) is

         Image:    String(1..Max_Field_Width);
         Last:     Natural range 0..Image'Last;
         Was_Last: Boolean;

      begin

         Check_is_Open( File );
         for n in Field_Number range 1..File.Desc.FinF loop
            Get_Field_Value( File, Image, Last, Was_Last );
            if Was_Last /= (n = File.Desc.FinF) then Raise_Data_Error( File, "more fields than expected" ); end if;
            if File.Desc.Used(n) then Set_Field( Item, File.Desc.NtoI(n), Image(1..Last) ); end if;
         end loop;

      end Read;

   end Generic_Interchange_Input;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Package body for output =###=

   package body Generic_Interchange_Output is

      procedure Write (File: in File_Type;
                       Item: in Element_Type) is
      begin
         Check_is_Open( File );
         for i in Field_Index loop
            Put_Field_Value( File, Get_Field( Item, i ), is_Last => i = Field_Index'Last );
         end loop;
      end Write;

   end Generic_Interchange_Output;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Field names =###=


   -----------------------------------
   -- =##= Normalize a field name =##=
   -- Remove weird characters, and convert to upper case

   function Normalize_Field_Name (Original: in String) return String is

      Result: String(1..Original'Length);
      j: Natural := 0;

   begin

      for i in Original'Range loop
         if is_in( Original(i), Field_Name_Characters ) then
            j := j+1;
            Result(j) := To_Upper(Original(i));
         end if;
      end loop;
      return Result(1..j);

   end Normalize_Field_Name;


   --------------------------------------------------------
   -- =##= Check that programmed field names are legal =##=

   procedure Check_Internal_Field_Names is
   begin
      for i in Field_Index'Range loop
         if Normalize_Field_Name( Field_Name(i) ) = "" then raise Program_Error; end if;
      end loop;
   end;


   ----------------------------------------
   -- =##= Output a row of field names =##=

   procedure Put_Field_Names (File: in File_Type) is
   begin

      Set_Current_Locus("Put_Field_Names"); --$D

      Check_Internal_Field_Names;

      for i in Field_Index loop
         Put_Field_Value( File, Field_Name(i), is_Last => i = Field_Index'Last );
      end loop;

      End_Current_Locus; --$D

   end Put_Field_Names;


   -----------------------------------------------------------------
   -- =##= Set up 1-to-1 mapping of internal to external fields =##=

   procedure Map_Fields_By_Position (File: in out File_Type) is

      n: Field_Count := 0;

   begin

      for i in Field_Index loop
         n := n+1;
         File.Desc.NtoI(n) := i;
      end loop;

      File.Desc.FinF := n;

   end Map_Fields_By_Position;


   -----------------------------------------------------------------------------
   -- =##= Read field names from first row, and map them to internal fields =##=

   procedure Map_Fields_By_Name (File: in out File_Type) is

      Mapped: Field_Set_by_Index := Null_Field_Set_by_Index; -- fields which have been mapped

      Name:     String(1..999);
      Last:     Natural range 0..Name'Last;
      Was_Last: Boolean;

      n: Field_Count := 0;

   begin

      Check_Internal_Field_Names;

      loop

         Get_Field_Value( File, Name, Last, Was_Last );
         n := n+1;

         if Normalize_Field_Name( Name(1..Last) ) = "" then
            Raise_Data_Error( File, "Illegal field name '" & Name(1..Last) & ''' );
         end if;

         for i in Field_Index loop
            if Normalize_Field_Name( Field_Name(i) ) = Normalize_Field_Name( Name(1..Last) ) then
               if Mapped(i) then Raise_Data_Error( File, "duplicate field name" ); end if;
               Mapped(i) := True;
               File.Desc.Used(n) := True;
               File.Desc.NtoI(n) := i;
               goto Next_Field;
            end if;
         end loop;

         -- unrecognised field name (ignore)
         File.Desc.Used(n) := False;

      <<Next_Field>>
         exit when Was_Last;

      end loop;

      File.Desc.FinF := n;

      if Mapped = Null_Field_Set_by_Index then Raise_Data_Error( File, "No fields mapped" ); end if;

   end Map_Fields_By_Name;


   ---------------------------------------------------------
   -- =##= Map fields by name or position, as specified =##=

   procedure Map_Fields (File: in out File_Type) is
   begin

      Set_Current_Locus("Map_Field_Names"); --$D

      case File.Desc.Spec.Mapping is
         when By_Name     => Map_Fields_By_Name    ( File );
         when By_Position => Map_Fields_By_Position( File );
      end case;

      Put_Line( Standard_Error, "FinF =" & Integer'Image(File.Desc.FinF) ); --$D

      End_Current_Locus; --$D

   end Map_Fields;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= File management =###=


   ------------------------------------------------
   -- =##= Is a file mode for input or output? =##=

   function is_Any_Output_Mode (Mode: in File_Mode) return Boolean is
   begin
      return (Mode = Out_File) or (Mode = Append_File);
   end;

   function is_Any_Input_Mode (Mode: in File_Mode) return Boolean is
   begin
      return Mode = In_File;
   end;


   --------------------------------------------------------------------
   -- =##= Conversion of file mode to native Ada.Text_IO.File_Mode =##=

   To_ATIOM: constant array (File_Mode) of Ada.Text_IO.File_Mode := (In_File     => Ada.Text_IO.In_File,
                                                                     Out_File    => Ada.Text_IO.Out_File,
                                                                     Append_File => Ada.Text_IO.Append_File);


   ------------------------------------------
   -- =##= Creation and opening of files =##=

   procedure Create (File: in out File_Type;
                     Mode: in     File_Mode     := Out_File;
                     Name: in     String        := "";
                     Form: in     String        := "";
                     Spec: in     Specification := Default_Spec) is
   begin

      Set_Current_Locus("Tenet.Interchange_IO.Create"); --$D

      if not is_Any_Output_Mode(Mode) then raise Use_Error; end if;
      if File.Desc /= null then raise Status_Error; end if; -- already open
      File.Desc := new File_Descriptor;
      Create( File.Desc.File, To_ATIOM(Mode), Name, Form );
      File.Desc.Spec := Spec;
      if Spec.Mapping = By_Name then Put_Field_Names(File); end if;

      End_Current_Locus; --$D

   end Create;

   procedure Open (File: in out File_Type;
                   Mode: in     File_Mode; -- must be In_File
                   Name: in     String;
                   Form: in     String        := "";
                   Spec: in     Specification := Default_Spec) is
   begin

      Set_Current_Locus("Tenet.Interchange_IO.Open"); --$D

      if not is_Any_Input_Mode(Mode) then raise Use_Error; end if;
      if File.Desc /= null then raise Status_Error; end if; -- already open
      File.Desc := new File_Descriptor;
      Open( File.Desc.File, To_ATIOM(Mode), Name, Form );
      File.Desc.Spec := Spec;
      Map_Fields( File );

      End_Current_Locus; --$D

   end Open;


   ----------------------------------------------------
   -- =##= Detecting end of file =##=

   function End_of_File (File: in File_Type) return Boolean is
   begin
      Check_is_Open( File );
      return End_of_File( File.Desc.File );
   end;

   -- Gotcha: EOF detection will be foxed by a line with just whitespace in it at the end of the file.


   ----------------------------------------------------
   -- =##= Resetting, closing, and deleting a file =##=

   procedure Reset (File: in out File_Type) is
   begin
      Check_is_Open( File );
      Reset( File.Desc.File );
   end;

   procedure Close (File: in out File_Type) is
   begin
      Check_is_Open( File );
      Close( File.Desc.File );
      Free( File.Desc );
   end;

   procedure Delete (File: in out File_Type) is
   begin
      Delete( File.Desc.File );
   end;


   ----------------------------
   -- =##= File properties =##=

   function Mode (File: in File_Type) return File_Mode is
   begin
      Check_is_Open( File );
      return File.Desc.Mode;
   end;

   function Name (File: in File_Type) return String is
   begin
      Check_is_Open( File );
      return Name(File.Desc.File);
   end;

   function Form (File: in File_Type) return String is
   begin
      Check_is_Open( File );
      return Form(File.Desc.File);
   end;

   function Spec (File: in File_Type) return Specification is
   begin
      Check_is_Open( File );
      return File.Desc.Spec;
   end;

   function is_Open (File: in File_Type) return Boolean is
   begin
      return is_Open(File.Desc.File);
   end;


   ----------------------------------------------------------
   -- =##= Ensuring data has reached its end destination =##=

   procedure Flush (File: in out File_Type) is
   begin
      Check_is_Open( File );
      Flush(File.Desc.File);
   end;


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

-- $Id: tenet-interchange_io.adb,v 1.1 2003/08/23 04:10:15 debater Exp $
-- $Name:  $

-- $Revision: 1.1 $
-- $Author: debater $
-- $Date: 2003/08/23 04:10:15 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-interchange_io.adb,v $
-- $RCSfile: tenet-interchange_io.adb,v $

-- $Log: tenet-interchange_io.adb,v $
-- Revision 1.1  2003/08/23 04:10:15  debater
-- Added Tenet.Interchange_IO package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.



