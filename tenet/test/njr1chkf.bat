rem check file "%1" against canonical file "keep\%2\%1"
if exist "keep\%2" goto chkf1
mkdir "keep\%2"
:chkf1
if exist "keep\%2\%1" goto chkf2
copy "%1" "keep\%2\%1"
echo warning: initialization of file "keep\%2\%1"
goto chkf3
:chkf2
fc /l /n "%1" "keep\%2\%1"
if not errorlevel 1 goto chkf3
echo ERROR *** in file "%1" of program "%2"
:chkf3
del "%1"
rem njr1chkf finished

