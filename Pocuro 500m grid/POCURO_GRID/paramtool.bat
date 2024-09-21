@ECHO OFF
..\bin\prms -C.\control\POCURO_initial.control -print
java -cp ..\dist\oui4.jar oui.paramtool.ParamTool .\input\POCURO_initial.params .\control\POCURO_initial.control.par_name
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
