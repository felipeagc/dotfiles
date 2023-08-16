@echo off

echo Setting up Emacs

set EMACSDIR=%APPDATA%\.emacs.d
IF EXIST %EMACSDIR% (
    rmdir %EMACSDIR%
)
mklink /D %EMACSDIR% %~dp0\emacs\.emacs.d

echo Setting up Neovim

set NVIMDIR=%APPDATA%\..\Local\nvim
IF EXIST %NVIMDIR% (
    rmdir %NVIMDIR%
)
mklink /D %NVIMDIR% %~dp0\nvim\.config\nvim

echo Setting up PowerShell

set PSDIR=%USERPROFILE%\Documents\PowerShell
IF EXIST %PSDIR% (
    rmdir %PSDIR%
)
mklink /D %PSDIR% %~dp0\powershell
