path=%PATH%;C:\Users\dwol3009\bin;C:\Users\dwol3009\AppData\Roaming\PhantomJS
rem production
rem @echo off
e:
cd E:\PBIX\Workings\dwollersheim2020-11-01\epideck_production
echo "********************************************************************************" >>make.log
echo "starting  run" >>make.log
Date /T >>make.log
time /T >>make.log
echo "********************************************************************************" >>make.log
"c:\Program Files\R\R-4.0.3\bin\Rscript.exe"  --vanilla make.R >>make.log 2>&1
echo "********************************************************************************" >>make.log
echo "ending  run" >>make.log
Date /T >>make.log
time /T >>make.log
echo "********************************************************************************" >>make.log


rem development
e:
cd E:\PBIX\Workings\dwollersheim2020-11-01\epideck_testing_slides

make_dev.bat



