rem checks output program %1 against canonical output
echo testing program "%1"
\bin\%1 > output.txt
if errorlevel 1 goto chkp1
call d:\cvsroot\tenet\test\njr1chkf output.txt %1
goto chkp2
:chkp1
echo nonzero completion code for program "%1"
:chkp2
del output.txt
rem njr1chkp finished

