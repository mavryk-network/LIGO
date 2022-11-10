Invoke-Expression -Command "esy release"

Copy-Item -Path tools\webide-new\ligo-webide-frontend\ligo-ide\public\favicon.ico -Destination _release
Copy-Item -Path WindowsInstaller.nsi -Destination _release

Set-Location _release

Invoke-Expression -Command makensis.exe WindowsInstaller.nsi

Set-Location ..
