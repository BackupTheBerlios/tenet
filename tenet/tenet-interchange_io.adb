-------------------------------------------------------------------------------------------------------------------
-- =####= Tenet.Interchange_IO package body =####=

-- Copyright (C) 2003 Nicholas James Roberts (South Croydon, Surrey, UK).
-- Part of the Tenet Container Library. See the bottom (end) of this file for important legal information.


-------------------------------------------------------------------------------------------------------------------
--$X with Tenet.Debugging; use Tenet.Debugging;

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
   -- =###= Utility string functions =###=


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
   -- =###= Normalize a field name =###=
   -- Remove weird characters, and convert to upper case.


   Field_Name_Characters: constant Character_Set := Letter_Set or Decimal_Digit_Set or To_Set('_');

   function Normalize_Field_Name (Original: in String) return String is

      Result: String(1..Original'Length);
      j: Natural := 0; -- running index into Result, also serving as final length count

   begin

      for i in Original'Range loop
         if is_in( Original(i), Field_Name_Characters ) then
            j := j+1;
            Result(j) := To_Upper(Original(i));
         end if;
      end loop;
      return Result(1..j);

   end Normalize_Field_Name;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Low-level (string based) I/O =###=

   package body Low_Level is

      ---------------------------------
      -- =##= File descriptor type =##=

      type File_Descriptor is limited
         record
            Text: Ada.Text_IO.File_Type;  -- underlying text file
            Mode: File_Mode;              -- input, output, or append
            Spec: Specification;          -- format specification
            EndR: Boolean;                -- end of record flag (used for both input and output)
         end record;

      procedure Free is new Ada.Unchecked_Deallocation( File_Descriptor, File_Type );

      ----------------------------------
      -- =##= Exception information =##=

      -- =#= Render an integer of type Ada.Text_IO.Count as a string =#=

      function ATIOCI (N: in Ada.Text_IO.Count) return String renames Ada.Text_IO.Count'Image;

      -- =#= Construct a string which contains relevant error information =#=

      function File_Info (File: in File_Type) return String is

         Name: constant String := "file " & '"' & Ada.Text_IO.Name(File.Text) & '"';
         Page: constant String := "page" & ATIOCI( Ada.Text_IO.Page(File.Text) );
         Line: constant String := "line" & ATIOCI( Ada.Text_IO.Line(File.Text) );

      begin
         if Ada.Text_IO.Page(File.Text) /= 1 then
            return Name & ", " & Page & ", " & Line;
         else
            return Name & ", " & Line;
         end if;
      end;

      -- =#= Raise an exception with relevant error information in it =#=

      procedure Raise_Data_Error (File:    in File_Type;
                                  Message: in String := "Unspecified error") is

         Info: constant String := File_Info(File); -- subtlety: the Skip_Line just below changes this

      begin
         Skip_Line( File.Text );
         Raise_Exception( Data_Error'Identity, Message & " [" & Info & "]" );
      end;

      ------------------------------------------------------------------------------------------------------
      -- =##= Conversion from Tenet.Interchange_IO.Low_Level.File_Mode to Ada.Text_IO.File_Mode (ATIOM) =##=

      To_ATIOM: constant array (File_Mode) of Ada.Text_IO.File_Mode := (In_File     => Ada.Text_IO.In_File,
                                                                        Out_File    => Ada.Text_IO.Out_File,
                                                                        Append_File => Ada.Text_IO.Append_File);

      ---------------------------------------------------------------------
      -- =##= Check that a file is open or not open, in the right mode =##=

      function is_Open (File: in File_Type) return Boolean is
      begin
         return File /= null;
         -- File /= null and not is_Open(File.Text) should never happen (in theory ;-)
      end;

      procedure Check_is_Open (File: in File_Type) is
      begin
         if not is_Open(File) then raise Status_Error; end if;
      end;

      procedure Check_is_Not_Open (File: in File_Type) is
      begin
         if is_Open(File) then raise Status_Error; end if;
      end;

      procedure Check_is_Open_For_Input (File: in File_Type) is
      begin
         Check_is_Open( File );
         if File.Mode /= In_File then raise Mode_Error; end if;
      end;

      procedure Check_is_Open_For_Output (File: in File_Type) is
      begin
         Check_is_Open( File );
         if File.Mode = In_File then raise Mode_Error; end if;
         -- okay if mode is for output (Out_File) or appending (Append_File)
      end;

      pragma Inline(is_Open,Check_is_Open,Check_is_Not_Open,Check_is_Open_For_Input,Check_is_Open_For_Output);

      ------------------------------------------
      -- =##= Creation and opening of files =##=

      -- =#= Create a new file and open it for output =#=

      procedure Create (File: in out File_Type;
                        Mode: in     File_Mode     := Out_File;
                        Name: in     String        := "";
                        Form: in     String        := "";
                        Spec: in     Specification := Default_Spec) is
      begin

--$X          Set_Current_Locus("Tenet.Interchange_IO.Create");

         if Mode = In_File then raise Use_Error; end if;
         Check_is_Not_Open( File );
         File := new File_Descriptor;
         File.Mode := Mode;
         File.Spec := Spec;
         File.EndR := True; -- indicating being at the beginning of a record
         Create( File.Text, To_ATIOM(Mode), Name, Form );

--$X          End_Current_Locus;

      end Create;

      -- =#= Open an existing file for input or appending =#=

      procedure Open (File: in out File_Type;
                      Mode: in     File_Mode; -- must be In_File or Append_File
                      Name: in     String;
                      Form: in     String        := "";
                      Spec: in     Specification := Default_Spec) is
      begin

--$X          Set_Current_Locus("Tenet.Interchange_IO.Open");

         if Mode = Out_File then raise Use_Error; end if;
         Check_is_Not_Open( File );
         File := new File_Descriptor;
         File.Mode := Mode;
         File.Spec := Spec;
         File.EndR := True; -- indicating being at the beginning of a record
         Open( File.Text, To_ATIOM(Mode), Name, Form );

--$X          End_Current_Locus;

      end Open;

      ----------------------------------------------------
      -- =##= Detecting end of file =##=

      function End_of_File (File: in File_Type) return Boolean is
      begin
         Check_is_Open( File );
         return End_of_File( File.Text );
      end;

      -- Gotcha: EOF detection will be foxed by a line with just whitespace in it at
      -- the end of the file. Actually, perhaps that's correct anyway.

      ----------------------------------------------------
      -- =##= Resetting, closing, and deleting a file =##=

      procedure Reset (File: in out File_Type) is
      begin
         Check_is_Open( File );
         File.EndR := True; -- indicating being at the beginning of a record
         Reset( File.Text );
      end;

      procedure Reset (File: in out File_Type;
                       Mode: in File_Mode;
                       Spec: in Specification) is
      begin
         Check_is_Open( File );
         File.Mode := Mode;
         File.Spec := Spec;
         File.EndR := True; -- indicating being at the beginning of a record
         Reset( File.Text, To_ATIOM(Mode) );
      end;

      procedure Close (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Close( File.Text );
         Free( File );
      end;

      procedure Delete (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Delete( File.Text );
         Free( File );
      end;

      ----------------------------
      -- =##= File properties =##=

      function Mode (File: in File_Type) return File_Mode is
      begin
         Check_is_Open( File );
         return File.Mode;
      end;

      function Name (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Name(File.Text);
      end;

      function Form (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Form(File.Text);
      end;

      function Spec (File: in File_Type) return Specification is
      begin
         Check_is_Open( File );
         return File.Spec;
      end;

      ----------------------------------------------------------
      -- =##= Ensuring data has reached its end destination =##=

      procedure Flush (File: in out File_Type) is
      begin
         Check_is_Open_For_Output( File );
         Flush( File.Text );
      end;

      ------------------------------------
      -- =##= Finish writing a record =##=

      procedure New_Record (File: in File_Type) is
      begin
         Check_is_Open_For_Output( File );
         New_Line( File.Text );
         File.EndR := True;
      end;

--       ------------------------------------
--       -- =##= Finish reading a record =##=
-- 
--       procedure Skip_Record (File: in File_Type) is
--       begin
--          Check_is_Open_For_Input( File );
--          Skip_Line( File.Text );
--          File.EndR := True;
--       end;

      ----------------------------------------------------------
      -- =##= Test if all fields of a record have been read =##=

      function End_of_Record (File: in File_Type) return Boolean is
      begin
         Check_is_Open_For_Input( File );
         return File.EndR;
      end;


      ------------------------------------------
      -- =##= Get a field value as a string =##=

      procedure Get_Field (File: in  File_Type;
                           Item: out String;
                           Last: out Natural) is

         -- =#= Global variables within the Get_Field procedure =#=

         Spec: Specification renames File.Spec;
         Text: Ada.Text_IO.File_Type renames File.Text;
         Char: Character; -- character currently being processed
         EoL:  Boolean; -- end of line flag

         -- =#= Consume an escape character, and check end of line not reached =#=

         procedure Get_Escape is
         begin
            Get( Text, Char );

--$X             Put( Standard_Error, " GE<" ); 
--$X             Put( Standard_Error, Char );   
--$X             Put( Standard_Error, "> " );   
--$X             if End_of_Line(Text) then Put( Standard_Error, "[EOF] " ); end if;

            if End_of_Line(Text) then Raise_Data_Error( File, "Character expected after escape" ); end if;
         end;

--          -- =#= Skip whitespace characters =#=

--          procedure Skip_Whitespace is
--          begin
--             while not End_of_Line(Text) and then is_in( Char, Spec.Whitespace ) loop
--                Get( Text, Char ); -- consume whitespace character

--$X --                Put( Standard_Error, " SW<" ); 
--$X --                Put( Standard_Error, Char );   
--$X --                Put( Standard_Error, "> " );   
--$X --                if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

--             end loop;
--          end;

         -- =##= Get the remainder of a quoted field =##=

         procedure Get_Quoted is

            i: Natural := Item'First-1; -- index to add character to Item

         begin

            loop

               -- Check for premature line ending
               if End_of_Line(Text) then Raise_Data_Error( File, "Quoted field not completed" ); end if;

               -- Consume the opening quote character
               Get( Text, Char );

--$X                Put( Standard_Error, " GQ1<" ); 
--$X                Put( Standard_Error, Char );    
--$X                Put( Standard_Error, "> " );    
--$X                if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

               -- Handle a special character
               if Spec.Determined(Escaping) then

                  case Spec.Escaping is
                     when Doubled_Quotes =>
                        if Char = Spec.Quote then
                           Look_Ahead( Text, Char, EoL );
                           exit when EoL or Char /= Spec.Quote; -- end of field unless doubled quote
                           Get( Text, Char ); -- consume one of the quotes

--$X                            Put( Standard_Error, " GQ2<" ); 
--$X                            Put( Standard_Error, Char );    
--$X                            Put( Standard_Error, "> " );    
--$X                            if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

                        end if;
                     when Escaped_Quotes =>
                        exit when Char = Spec.Quote; -- end of field
                        if Char = Spec.Escape then Get_Escape; end if; -- consume escape character
                  end case;

               elsif Char = Spec.Quote then

                  Look_Ahead( Text, Char, EoL );
                  exit when EoL or Char /= Spec.Quote; -- end of field unless doubled quote
                  Spec.Determined(Escaping) := True;
                  Spec.Escaping := Doubled_Quotes;
                  Get( Text, Char ); -- consume one of the quotes

--$X                   Put( Standard_Error, " GQ3<" ); 
--$X                   Put( Standard_Error, Char );    
--$X                   Put( Standard_Error, "> " );    
--$X                   if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

               elsif is_in( Char, Spec.Escapes ) then

                  Spec.Determined(Escaping) := True;
                  Spec.Escaping := Escaped_Quotes;
                  Spec.Escape := Char;
                  Get_Escape; -- consume escape character

               end if;

               -- Add character to result string
               i := i+1;
               Item(i) := Char;

            end loop;

            -- Set the out parameter which gives how many characters are in the result string
            Last := i;

            -- Skip trailing whitespace (yuk); return if end of line
            loop
               if End_of_Line(Text) then
                  File.EndR := True;
                  return;
               else
                  Get( Text, Char ); -- consume character (closing quote or whitespace)

--$X                   Put( Standard_Error, " GQ4<" ); 
--$X                   Put( Standard_Error, Char );    
--$X                   Put( Standard_Error, "> " );    
--$X                   if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

                  exit when not is_in( Char, Spec.Whitespace );
               end if;
            end loop;

            -- Check delimiter
            if Spec.Determined(Delimiter) then
               if Char /= Spec.Delimiter then
                  Raise_Data_Error( File, "Delimiter '" & Spec.Delimiter & "' expected" );
               end if;
            elsif is_in( Char, Spec.Delimiters ) then
               Spec.Determined(Delimiter) := True;
               Spec.Delimiter := Char;
            else
               Raise_Data_Error( File, "Delimiter expected" );
            end if;
            File.EndR := False; -- delimiter parsed, so another field expected

         end Get_Quoted;

         -- =#= Get the remainder of an unquoted field =#=

         procedure Get_Unquoted is

            i: Natural := Item'First-1; -- index to add character to Item

         begin

            loop

               -- Test for delimiter
               if Spec.Determined(Delimiter) then
                  if Char = Spec.Delimiter then
                     File.EndR := False; -- delimiter parsed, so another field expected
                     exit;
                  end if;
               elsif is_in( Char, Spec.Delimiters ) then
                  Spec.Determined(Delimiter) := True;
                  Spec.Delimiter := Char;
                  File.EndR := False; -- delimiter parsed, so another field expected
                  exit;
               end if;

               -- Handle any escaping
               if Spec.Determined(Escaping) then
                  if Char = Spec.Escape then Get_Escape; end if; -- consume escape character
               elsif is_in( Char, Spec.Escapes ) then
                  Spec.Determined(Escaping) := True;
                  Spec.Escaping := Escaped_Quotes;
                  Spec.Escape := Char;
                  Get_Escape; -- consume escape character
               end if;

               -- Add character to result string
               i := i+1;
               Item(i) := Char;

               -- Get next character, if not end of line
               if End_of_Line(Text) then File.EndR := True; exit; end if;
               Get( Text, Char );

--$X                Put( Standard_Error, " GU<" ); 
--$X                Put( Standard_Error, Char );   
--$X                Put( Standard_Error, "> " );   
--$X                if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

            end loop;

            -- Remove trailing whitespace
            if Spec.Trimming then
               while i > Item'First and then is_in( Item(i), Spec.Whitespace ) loop
                  i := i-1;
               end loop;
            end if;

            -- Set the out parameter which gives how many characters are in the result string
            Last := i;

         end Get_Unquoted;

         -- =#= Main Get_Field algorithm =#=

      begin

         Check_is_Open_For_Input( File );

         -- Check for empty field at end of line
         if End_of_Line(Text) then
            File.EndR := True;
            Last := Item'First-1; -- to indicate that no characters are in Item
            goto Finish_Up;
         end if;

         -- Get first character of field (or delimiter if field is empty)
         Get( Text, Char ); 

--$X          Put( Standard_Error, " GFV<" ); 
--$X          Put( Standard_Error, Char );    
--$X          Put( Standard_Error, "> " );    
--$X          if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

         -- Skip initial whitespace if necessary
         if Spec.Trimming then
            while is_in( Char, Spec.Whitespace ) loop
               if End_of_Line(Text) then
                  File.EndR := True;
                  Last := Item'First-1; -- to indicate that no characters are in Item
                  goto Finish_Up;
               end if;
               Get( Text, Char ); -- consume whitespace character

--$X                Put( Standard_Error, " SIW<" ); 
--$X                Put( Standard_Error, Char );    
--$X                Put( Standard_Error, "> " );    
--$X                if End_of_Line(Text) then Put( Standard_Error, "[EOL] " ); end if;

            end loop;
         end if;

         -- Get (remainder of) field value (quoted or unquoted)
         if Spec.Determined(Quote) then
            if Char = Spec.Quote then Get_Quoted; else Get_Unquoted; end if;
         elsif is_in( Char, Spec.Quotes ) then
            Spec.Determined(Quote) := True;
            Spec.Quote := Char;
            Get_Quoted;
         else
            Get_Unquoted;
         end if;

         -- Deal with end-of-record if necessary
      <<Finish_Up>>
         if File.EndR then
            Skip_Line(Text); -- to move us to the beginning of the next line
         end if;

--$X          Put( Standard_Error, "GOT '" & Item(1..Last) & ''' );               
--$X          if File.EndR then Put( Standard_Error, " LAST FIELD" ); end if; 
--$X          New_Line( Standard_Error );                                         

      end Get_Field;
         
      ------------------------------------------
      -- =##= Put a field value as a string =##=

      procedure Put_Field (File: in File_Type;
                           Item: in String) is

         -- =#= Variables global to the Put_Field procedure =#=
         Spec: Specification renames File.Spec;
         Text: Ada.Text_IO.File_Type renames File.Text;

         -- =#= Test for a special character (delimiter, quote, or escape) =#=
         function is_Special (Char: in Character) return Boolean is
         begin
            return Char = Spec.Delimiter or Char = Spec.Quote or Char = Spec.Escape;
         end;

         -- =#= Test if a string contains a special character anywhere =#=
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
               if is_Special(Item(i)) then Put( Text, Spec.Escape ); end if;
               Put( Text, Item(i) );
            end loop;
         end;

         -- =#= Put the field value quoted =#=
         procedure Put_Quoted is
         begin
            Put( Text, Spec.Quote ); -- initial quote character
            for i in Item'Range loop
               if Item(i) = Spec.Quote then
                  case Spec.Escaping is
                     when Doubled_Quotes => Put( Text, Spec.Quote  );
                     when Escaped_Quotes => Put( Text, Spec.Escape );
                  end case;
               elsif Item(i) = Spec.Escape and Spec.Escaping = Escaped_Quotes then
                  Put( Text, Spec.Escape ); -- escape the escape character!
               end if;
               Put( Text, Item(i) );
            end loop;
            Put( Text, Spec.Quote ); -- final quote character
         end;

         -- =#= Test the field value, and put it quoted if it needs to be, otherwise unquoted =#=
         procedure Put_Quoted_if_Necessary is
         begin
            if Contains_Special(Item) or else is_Trimmable(Item,Spec.Whitespace) then
               Put_Quoted;
            else
               Put_Unquoted;
            end if;
         end;

      begin

         Check_is_Open_For_Output( File );

         -- =#= Put a delimiter if necessary =#=
         if File.EndR then
            File.EndR := False; -- indicate we are no longer at the beginning of a record
         else
            Put( Text, Spec.Delimiter ); -- put a delimiter between previous field and the one we now write
         end if;

         -- =#= Put the field according to the quoting mode =#=
         case Spec.Quoting is
            when No_Quoting      => Put_Unquoted;
            when Minimal_Quoting => Put_Quoted_if_Necessary;
            when Quote_All       => Put_Quoted;
         end case;

      end Put_Field;

   end Low_Level;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Package body for generic input =###=

   package body Generic_Input is

      --------------------------------------------------------
      -- =##= Check that programmed field names are legal =##=

      procedure Check_Internal_Field_Names is
      begin
         for i in Field_Index'Range loop
            if Normalize_Field_Name( Field_Name(i) ) = "" then raise Use_Error; end if;
         end loop;
      end;

      ----------------------------------------
      -- =##= Field numbering and mapping =##=

      subtype Field_Count  is Natural  range 0 .. Max_Fields_in_File;
      subtype Field_Number is Positive range 1 .. Field_Count'Last;

      type Field_Set_by_Index  is array (Field_Index)  of Boolean;
      type Field_Set_by_Number is array (Field_Number) of Boolean;

      Null_Field_Set_by_Index: constant Field_Set_by_Index := (others => False);

      type Number_to_Index_Mapping is array (Field_Number) of Field_Index;

      ---------------------------------
      -- =##= File descriptor type =##=

      type File_Descriptor is limited
         record
            LowF: Low_Level.File_Type;     -- underlying low-level file
            FinF: Field_Count;             -- number of fields in input file
            Used: Field_Set_by_Number;     -- fields in input file which are mapped to internal fields
            NtoI: Number_to_Index_Mapping; -- so that out-of-order named fields still work
         end record;

      procedure Free is new Ada.Unchecked_Deallocation( File_Descriptor, File_Type );

      ---------------------------------------------------------------------
      -- =##= Raise an exception with relevant error information in it =##=

      procedure Raise_Data_Error (File:    in File_Type;
                                  Message: in String := "Unspecified error") is
      begin
         Low_Level.Raise_Data_Error( File.LowF, Message );
      end;

      -----------------------------------------------------------------
      -- =##= Set up 1-to-1 mapping of internal to external fields =##=

      procedure Map_Fields_By_Position (File: in File_Type) is

         n: Field_Count := 0;

      begin

         for i in Field_Index loop
            n := n+1;
            File.NtoI(n) := i;
         end loop;

         File.FinF := n;
         File.Used(1..n) := (others => True);

      end Map_Fields_By_Position;

      -----------------------------------------------------------------------------
      -- =##= Read field names from first row, and map them to internal fields =##=

      procedure Map_Fields_By_Name (File: in File_Type) is

         Mapped: Field_Set_by_Index := Null_Field_Set_by_Index; -- fields which have been mapped

         Name: String(1..999);
         Last: Natural range 0..Name'Last;

         n: Field_Count := 0;

      begin

         Check_Internal_Field_Names;

         loop

            Low_Level.Get_Field( File.LowF, Name, Last );
            n := n+1;

            if Normalize_Field_Name( Name(1..Last) ) = "" then
               Raise_Data_Error( File, "Illegal field name '" & Name(1..Last) & ''' );
            end if;

            for i in Field_Index loop
               if Normalize_Field_Name( Field_Name(i) ) = Normalize_Field_Name( Name(1..Last) ) then
                  if Mapped(i) then
                     Raise_Data_Error( File, "duplicate field name '" & Name(1..Last) & ''' );
                  end if;
                  Mapped(i) := True;
                  File.Used(n) := True;
                  File.NtoI(n) := i;
                  goto Next_Field;
               end if;
            end loop;

            -- unrecognised field name (ignore)
            File.Used(n) := False;

         <<Next_Field>>
            exit when Low_Level.End_of_Record(File.LowF);

         end loop;

         File.FinF := n;

         if Mapped = Null_Field_Set_by_Index then
            Raise_Data_Error( File, "No fields mapped" );
         end if;

      end Map_Fields_By_Name;

      ---------------------------------------------------------
      -- =##= Map fields by name or position, as specified =##=

      procedure Map_Fields (File: in File_Type) is
      begin

--$X          Set_Current_Locus("Map_Field_Names");

         case Spec(File).Mapping is
            when By_Name     => Map_Fields_By_Name    ( File );
            when By_Position => Map_Fields_By_Position( File );
         end case;

--$X          Put_Line( Standard_Error, "FinF =" & Integer'Image(File.FinF) );
--$X          End_Current_Locus;

      end Map_Fields;

      --------------------------------------------------
      -- =##= Tests for file being open or not open =##=

      function is_Open (File: in File_Type) return Boolean is
      begin
         return File /= null;
      end;

      procedure Check_is_Open (File: in File_Type) is
      begin
         if not is_Open(File) then raise Status_Error; end if;
      end;

      procedure Check_is_Not_Open (File: in File_Type) is
      begin
         if is_Open(File) then raise Status_Error; end if;
      end;

      pragma Inline(is_Open,Check_is_Open,Check_is_Not_Open);

      -------------------------------
      -- =##= File management =##=

      procedure Open (File: in out File_Type;
                      Name: in     String;
                      Form: in     String        := "";
                      Spec: in     Specification := Default_Spec) is
      begin
         Check_is_Not_Open( File );
         File := new File_Descriptor;
         Low_Level.Open( File.LowF, Low_Level.In_File, Name, Form, Spec );
         Map_Fields( File );
      end;

      procedure Reset (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Reset( File.LowF );
         Map_Fields( File );
      end;

      procedure Reset (File: in out File_Type;
                       Spec: in     Specification) is
      begin
         Check_is_Open( File );
         Low_Level.Reset( File.LowF, Low_Level.In_File, Spec );
         Map_Fields( File );
      end;

      procedure Close (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Close( File.LowF );
         Free( File );
      end;

      procedure Delete (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Delete( File.LowF );
         Free( File );
      end;

      function Name (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Low_Level.Name( File.LowF );
      end;

      function Form (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Low_Level.Form( File.LowF );
      end;

      function Spec (File: in File_Type) return Specification is
      begin
         Check_is_Open( File );
         return Low_Level.Spec( File.LowF );
      end;

      pragma Inline(Name,Form,Spec);

      -------------------------
      -- =##= Reading data =##=

      function End_of_File (File: in File_Type) return Boolean is
      begin
         Check_is_Open( File );
         return Low_Level.End_of_File( File.LowF );
      end;

      procedure Read (File: in     File_Type;
                      Item: in out Element_Type) is

         Image: String(1..Max_Field_Width);
         Last:  Natural range 0..Image'Last;

      begin

         Check_is_Open( File );

         for n in Field_Number range 1..File.FinF loop

            Low_Level.Get_Field( File.LowF, Image, Last );

            if Low_Level.End_of_Record(File.LowF) and (n < File.FinF) then
               Raise_Data_Error( File, "fewer fields than expected" );
            end if;

            if File.Used(n) then Set_Field( Item, File.NtoI(n), Image(1..Last) ); end if;

         end loop;

         if not Low_Level.End_of_Record(File.LowF) then
            Raise_Data_Error( File, "more fields than expected" );
         end if;

         -- Low_Level.Skip_Record( File.LowF ); -- not necessary (and now not implemented ;-)

      end Read;

   end Generic_Input;


   ----------------------------------------------------------------------------------------------------------------
   -- =###= Package body for output =###=

   package body Generic_Output is

      use Low_level;

      --------------------------------------------------------
      -- =##= Check that programmed field names are legal =##=

      procedure Check_Internal_Field_Names is
      begin
         for i in Field_Index'Range loop
            if Normalize_Field_Name( Field_Name(i) ) = "" then raise Use_Error; end if;
         end loop;
      end;

      ----------------------------
      -- =##= Output file type =##=

      type File_Descriptor is limited
         record
            LowF: Low_Level.File_Type; -- underlying low-level file
         end record;

      procedure Free is new Ada.Unchecked_Deallocation( File_Descriptor, File_Type );

      ---------------------------------------------------------------------
      -- =##= Raise an exception with relevant error information in it =##=

      procedure Raise_Data_Error (File:    in File_Type;
                                  Message: in String := "Unspecified error") is
      begin
         Low_Level.Raise_Data_Error( File.LowF, Message );
      end;

      ----------------------------------------
      -- =##= Output a row of field names =##=

      procedure Put_Field_Names (File: in File_Type) is
      begin

--$X          Set_Current_Locus("Put_Field_Names");

         Check_Internal_Field_Names;

         for i in Field_Index loop
            Low_Level.Put_Field( File.LowF, Field_Name(i) );
         end loop;

         Low_Level.New_Record( File.LowF );         

--$X          End_Current_Locus;

      end Put_Field_Names;

      ----------------------------------------------------------
      -- =##= Allocate and construct input file information =##=

      procedure Put_Field_Names_if_Necessary (File: in File_Type) is
      begin
         case Spec(File).Mapping is
            when By_Name     => Put_Field_Names( File );
            when By_Position => null; -- no field names
         end case;
      end;

      --------------------------------------------------
      -- =##= Tests for file being open or not open =##=

      function is_Open (File: in File_Type) return Boolean is
      begin
         return File /= null;
      end;

      procedure Check_is_Open (File: in File_Type) is
      begin
         if not is_Open(File) then raise Status_Error; end if;
      end;

      procedure Check_is_Not_Open (File: in File_Type) is
      begin
         if is_Open(File) then raise Status_Error; end if;
      end;

      pragma Inline(is_Open,Check_is_Open,Check_is_Not_Open);

      -------------------------------
      -- =##= File management =##=

      procedure Create (File: in out File_Type;
                        Name: in     String        := "";
                        Form: in     String        := "";
                        Spec: in     Specification := Default_Spec) is
      begin
         Check_is_Not_Open( File );
         File := new File_Descriptor;
         Low_Level.Create( File.LowF, Low_Level.Out_File, Name, Form, Spec );
         Put_Field_Names_if_Necessary( File );
      end;

      procedure Reset (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Reset( File.LowF );
         Put_Field_Names_if_Necessary( File );
      end;

      procedure Reset (File: in out File_Type;
                       Spec: in     Specification) is
      begin
         Check_is_Open( File );
         Low_Level.Reset( File.LowF, Low_Level.Out_File, Spec );
         Put_Field_Names_if_Necessary( File );
      end;

      procedure Close (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Close( File.LowF );
         Free( File );
      end;

      procedure Delete (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Delete( File.LowF );
         Free( File );
      end;

      function Name (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Low_Level.Name( File.LowF );
      end;

      function Form (File: in File_Type) return String is
      begin
         Check_is_Open( File );
         return Low_Level.Form( File.LowF );
      end;

      function Spec (File: in File_Type) return Specification is
      begin
         Check_is_Open( File );
         return Low_Level.Spec( File.LowF );
      end;

      procedure Flush (File: in out File_Type) is
      begin
         Check_is_Open( File );
         Low_Level.Flush( File.LowF );
      end;

      pragma Inline(Name,Form,Spec);

      -------------------------
      -- =##= Writing data =##=

      procedure Write (File: in File_Type;
                       Item: in Element_Type) is
      begin

         Check_is_Open( File );

         for i in Field_Index loop
            Low_Level.Put_Field( File.LowF, Get_Field( Item, i ) );
         end loop;

         Low_Level.New_Record( File.LowF );         

      end Write;

   end Generic_Output;


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

-- $Id: tenet-interchange_io.adb,v 1.2 2004/03/14 21:07:14 debater Exp $
-- $Name:  $

-- $Revision: 1.2 $
-- $Author: debater $
-- $Date: 2004/03/14 21:07:14 $
-- $State: Exp $

-- $Source: /home/xubuntu/berlios_backup/github/tmp-cvs/tenet/Repository/tenet/tenet-interchange_io.adb,v $
-- $RCSfile: tenet-interchange_io.adb,v $

-- $Log: tenet-interchange_io.adb,v $
-- Revision 1.2  2004/03/14 21:07:14  debater
-- Routine commit.
--
-- Revision 1.1  2003/08/23 04:10:15  debater
-- Added Tenet.Interchange_IO package.
--
--


-------------------------------------------------------------------------------------------------------------------
-- End of File.


