!include WinMessages.nsh
# define installer name
OutFile "installer.exe"
 
# set desktop as install directory
InstallDir $Profile\ligo
 
# default section start
Section
  # define output path
  SetOutPath $INSTDIR
  # specify file to go in output path
  File /r *
  # Run postinstall.js
  nsExec::Exec 'node ./postinstall.js'


  EnVar::Check "NULL" "NULL"
  Pop $0
  DetailPrint "EnVar::Check write access HKCU returned=|$0|"

  EnVar::SetHKCU
  EnVar::AddValue "Path" $INSTDIR\bin
  EnVar::Update "HKCU" "Path"
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  # define uninstaller name
  WriteUninstaller $INSTDIR\uninstaller.exe
# default section end
SectionEnd
 
# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall"
  # Delete the uninstaller
  Delete $INSTDIR\uninstaller.exe
  # Delete the directory
  RMDir $INSTDIR
  EnVar::SetHKCU
  EnVar::DeleteValue "Path" $INSTDIR\bin
  EnVar::Update "HKCU" "Path"
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
SectionEnd
