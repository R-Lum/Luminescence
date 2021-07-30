## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   AppVeyor powershell script to overcome problems while installing JAGS on the VM
## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@aber.ac.uk
## Date:    Sun Jul 22 12:34:53 2018
## Info:    The successful download of JAGS from the remote server is fragile and often enough
##          pure luck. If the download fails, the build fails. This script provides a function
##          that first tries to grep JAGS from the internal cache before attempting to download
##          JAGS from the remote server.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function InstallJAGS {
  [CmdletBinding()]
  Param()

if ((Test-Path "C:\projects\JAGS-4.3.0.exe")) {
    Start-Process -FilePath "C:\projects\JAGS-4.3.0.exe" -ArgumentList "/S" -NoNewWindow -Wait

  } Else {
    Start-FileDownload 'https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/JAGS-4.3.0.exe' -FileName 'C:\projects\JAGS-4.3.0.exe'
    Start-Process -FilePath "C:\projects\JAGS-4.3.0.exe" -ArgumentList "/S" -NoNewWindow -Wait
  }

}
