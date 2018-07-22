##Additional AppVeyor script to overcome problems while installing JAGS on the VM

Function InstallJags {
  [CmdletBinding()]
  Param()

if ((Test-Path "C:\projects\JAGS-4.3.0.exe")) {
    Progress "Running JAGS installer"
    Start-Process -FilePath 'C:\projects\JAGS-4.3.0.exe'

  } Else {
    Progress "JAGS not yet available, trying to download  ..."
    Start-FileDownload 'https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/JAGS-4.3.0.exe' -FileName 'C:\projects\JAGS-4.3.0.exe'
    Start-Process -FilePath 'C:\projects\JAGS-4.3.0.exe'
  }

}
