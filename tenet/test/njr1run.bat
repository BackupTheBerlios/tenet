echo starting Tenet tests
echo rebuilding programs
d:
cd \cvsroot\tenet\test
gnatmake -Pnjr1
if errorlevel 1 goto runbadbd
echo running tests ...
cd \testdata
call \cvsroot\tenet\test\njr1chkp debug001
call \cvsroot\tenet\test\njr1chkf errorlog.txt debug001
echo all tests completed
goto runfin
:runbadbd
echo rebuild failed (tests not run)
:runfin
del *.txt
cd \cvsroot\tenet\test
rem njr1run finished

