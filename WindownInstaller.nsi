# define installer name
OutFile "installer.exe"
 
# set desktop as install directory
InstallDir $ProgramFiles\ligo
 
# default section start
Section
 
# define output path
SetOutPath $INSTDIR
 

# specify file to go in output path
File /r *
 
# Run postinstall.js
nsExec::Exec 'node ./postinstall.js'

# define uninstaller name
WriteUninstaller $INSTDIR\uninstaller.exe
 
 
#-------
# default section end
SectionEnd
 
# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall"
 
# Delete installed file
Delete $INSTDIR\test.txt
 
# Delete the uninstaller
Delete $INSTDIR\uninstaller.exe
 
# Delete the directory
RMDir $INSTDIR
SectionEnd
