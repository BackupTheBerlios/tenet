@echo off
echo starting Tenet tests
echo rebuilding programs
d:
cd \cvsroot\tenet\test
gnatmake -m -i -Pnjr1
if errorlevel 1 goto runbadb
echo running tests ...
cd \testdata
call \cvsroot\tenet\test\njr1chkp debug001
call \cvsroot\tenet\test\njr1chkf errorlog.txt debug001
call \cvsroot\tenet\test\njr1chkp calio001
call \cvsroot\tenet\test\njr1chkf caliotf1.txt calio001
echo all tests completed
goto runfin
:runbadb
echo rebuild failed (tests not run)
:runfin
del *.txt
cd \cvsroot\tenet\test
rem njr1run finished

