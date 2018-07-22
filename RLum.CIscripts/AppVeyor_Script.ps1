## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   AppVeyor powershell script to overcome problems while installing JAGS on the VM
## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
## Date:    Sun Jul 22 12:34:53 2018
## Info:    The successful download of JAGS from the remote server very fragile and is often enough
##          pure luck. If the download fails, the build fails. This script provides a function
##          that first tries to use JAGS from the internal cache before attempting to dowload
##          JAGS from the remove server.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function InstallJAGS {
  [CmdletBinding()]
  Param()

if ((Test-Path "C:\projects\JAGS-4.3.0.exe")) {
    Progress "JAGS found in cache, running JAGS installer ..."
    Start-Process -FilePath C:\projects\JAGS-4.3.0.exe -NoNewWindow -Wait

  } Else {
    Progress "JAGS not available, attempting download ..."
    Start-FileDownload 'https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/JAGS-4.3.0.exe' -FileName 'C:\projects\JAGS-4.3.0.exe'
    Progress "Running JAGS installer ... "
    Start-Process -FilePath C:\projects\JAGS-4.3.0.exe -NoNewWindow -Wait
  }

}
