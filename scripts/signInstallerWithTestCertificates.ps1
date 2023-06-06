param([switch]$Elevated)

function Test-Admin {
    $currentUser = New-Object Security.Principal.WindowsPrincipal $([Security.Principal.WindowsIdentity]::GetCurrent())
    $currentUser.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

if ((Test-Admin) -eq $false)  {
    if ($elevated) {
        # tried to elevate, did not work, aborting
    } else {
        Start-Process powershell.exe -Verb RunAs -ArgumentList ('-noprofile -noexit -file "{0}" -elevated' -f ($myinvocation.MyCommand.Definition))
    }
    exit
}


$cert = New-SelfSignedCertificate -DnsName ligolang -CertStoreLocation cert:\LocalMachine\My -type CodeSigning
$pwd = ConvertTo-SecureString -String $env:TEST_CERTIFICATE_PASSWORD -Force -AsPlainText
Export-PfxCertificate -cert $cert -FilePath ligolang-certs.pfx -Password $pwd
$env:PATH = $env:PATH + ';C:\Program Files (x86)\Windows Kits\10\App Certification Kit\'
signtool.exe sign /a /f ligolang-certs.pfx /p $env:TEST_CERTIFICATE_PASSWORD /fd SHA256 ligo_installer.exe
