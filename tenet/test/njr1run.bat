echo starting Tenet tests
echo rebuilding programs
cd d:\cvsroot\tenet\test
gnatmake -Pnjr1
if errorlevel 1 goto runbadbd
echo running tests ...
cd d:\testdata
call d:\cvsroot\tenet\test\njr1chkp debug001
move errorlog.txt keep\debug001\errorlog.txt
echo all tests completed
goto runfin
:runbadbd
echo rebuild failed (tests not run)
:runfin
del *.txt
cd d:\cvsroot\tenet\test
rem njr1run finished

