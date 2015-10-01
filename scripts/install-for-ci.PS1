#|
function Expand-ZIPFile($file, $destination) {
  $shell = new-object -com shell.application
  $zip = $shell.NameSpace($file)
  foreach($item in $zip.items()) {
    $shell.Namespace($destination).copyhere($item,20)
  }
}

if($env:ROSWELL_TARBALL_PATH -eq $nul){Set-Item env:ROSWELL_TARBALL_PATH $env:HOMEDRIVE$env:HOMEPATH\roswell.zip}
if($env:ROSWELL_URI -eq $nul) {Set-Item env:ROSWELL_URI "https://ci.appveyor.com/api/buildjobs/vk58iojwkgq51nw4/artifacts/Roswell-i686.zip"}
if($env:ROSWELL_DIR -eq $nul) {Set-Item env:ROSWELL_DIR $env:HOMEDRIVE$env:HOMEPATH\.roswell}
if($env:ROSWELL_INSTALL_DIR -eq $nul) {Set-Item env:ROSWELL_INSTALL_DIR $env:ROSWELL_DIR}
Set-Item env:LISP_IMPLS_DIR $env:ROSWELL_DIR"\impls\system"
Set-Item env:LISP_IMPLS_BIN $env:ROSWELL_INSTALL_DIR"\bin"

$cli = New-Object System.Net.WebClient
$uri = New-Object System.Uri($env:ROSWELL_URI)
$cli.DownloadFile($uri, $env:ROSWELL_TARBALL_PATH)
Write-Host "downloaded" $uri
Expand-ZIPFile -File $env:ROSWELL_TARBALL_PATH -Destination $env:ROSWELL_DIR
&$env:ROSWELL_DIR\roswell\ros $MyInvocation.MyCommand.Definition
<# |#
(defun main (&rest argv)
  (declare (ignorable argv))
  (print "Hello, World!"))
;#>
