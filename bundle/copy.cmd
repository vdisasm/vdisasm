set dst=vdisasm
set src=..\pub\bin

rd /s /q %dst%
del vdisasm.7z

::pause

md %dst%\icons
xcopy %src%\icons\*.*  %dst%\icons\

xcopy %src%\local\docklayouts\default.layout %dst%\local\docklayouts\
xcopy %src%\local\ack.txt %dst%\local\

xcopy %src%\typelibs\std.typelib %dst%\typelibs\

xcopy %src%\win32\cpu\arm.dll         %dst%\win32\cpu\
xcopy %src%\win32\cpu\x86_mediana.dll %dst%\win32\cpu\

xcopy %src%\win32\loaders\LoaderBinary.dll %dst%\win32\loaders\
xcopy %src%\win32\loaders\LoaderDOS.dll    %dst%\win32\loaders\
xcopy %src%\win32\loaders\LoaderELF.dll    %dst%\win32\loaders\
xcopy %src%\win32\loaders\LoaderPE.dll     %dst%\win32\loaders\

::xcopy %src%\win32\plugins\benchmarks.dll   %dst%\win32\plugins\
::xcopy %src%\win32\plugins\SimplePlugin.dll %dst%\win32\plugins\

xcopy %src%\win32\typeproviders\tp_std.dll %dst%\win32\typeproviders\

xcopy %src%\win32\core.dll %dst%\win32\
xcopy %src%\win32\gui.exe  %dst%\win32\
xcopy %src%\win32\grphlt.dll %dst%\win32\
xcopy %src%\win32\msvcp120.dll %dst%\win32\
xcopy %src%\win32\msvcr120.dll %dst%\win32\

::
:: ZIP
::
7z a vdisasm vdisasm\

:: Delete tmp dir.
::rd /s /q %dst%