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
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("OCAML_VERSION", "n.00.0000").r0'
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("OCAML_PKG_NAME", "ocaml").r0'
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("ESY_RELEASE_REWRITE_PREFIX", "true").r0'
  StrCmp $0 0 error
  # ExecWait ProgThatReadsEnv.exe
  nsExec::Exec 'node $INSTDIR\esyInstallRelease.js'
  Goto done
  error:
    MessageBox MB_OK "Can't set environment variable"
  done:


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
