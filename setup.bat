@echo off

set EMACSDIR=%APPDATA%\.emacs.d
set NVIMDIR=%APPDATA%\..\Local\nvim

IF EXIST %EMACSDIR% (
    rmdir %EMACSDIR%
)
mklink /D %EMACSDIR% %~dp0\emacs\.emacs.d

IF EXIST %NVIMDIR% (
    rmdir %NVIMDIR%
)
mklink /D %NVIMDIR% %~dp0\nvim\.config\nvim
