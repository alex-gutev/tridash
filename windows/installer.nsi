## NSIS Windows Installer Script

!define APPNAME "Tridash"
!define COMPANYNAME "TridSoft"
!define DESCRIPTION "Compiler for the Tridash Programming language"

!define VERSIONMAJOR 0
!define VERSIONMINOR 4
!define VERSIONBUILD 1

!define HELPURL "https://github.com/alex-gutev/tridash/wiki"
!define UPDATEURL "https://github.com/alex-gutev/tridash/releases/latest"
!define ABOUTURL "https://github.com/alex-gutev/tridash/wiki"

!include "winmessages.nsh"

!define env_hklm 'HKLM "System\CurrentControlSet\Control\Session Manager\Environment"'
!define env_hkcu 'HKCU "Environment"'

RequestExecutionLevel admin

InstallDir "$PROGRAMFILES\${APPNAME}"

LicenseData "..\LICENSE.txt"

Name "${APPNAME}"
outFile "tridash-${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONBUILD}-setup.exe"

page license
page directory
page instfiles

Section "install"
  # Copy Compiler
  setOutPath $INSTDIR\bin

  File "..\tridashc.exe"

  # Copy Core Modules
  setOutpath $INSTDIR\modules

  File "..\modules\core.trd"
  File "..\modules\core.yml"

  # Copy Runtime Libraries
  setOutPath $INSTDIR\backends\javascript

  File "..\src\backends\javascript\runtime\tridash.js"

  # Create Uninstaller
  writeUninstaller "$INSTDIR\uninstall.exe"

  # Create Start Menu Shortcuts
  createDirectory "$SMPROGRAMS\${APPNAME}"
  createShortCut "$SMPROGRAMS\${APPNAME}\Uninstall.lnk" "$INSTDIR\uninstall.exe"

  # Add Registry information for add/remove programs
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayName" "${APPNAME} - ${DESCRIPTION}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "QuietUninstallString" "$INSTDIR\uninstall.exe /S"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "Publisher" "Alexander Gutev"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "HelpLink" "${HELPURL}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "URLUpdateInfo" "${UPDATEURL}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "URLInfoAbout" "${ABOUTURL}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayVersion" "${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONBUILD}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "VersionMajor" ${VERSIONMAJOR}
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "VersionMinor" ${VERSIONMINOR}
  # No Option for modifying or repairing install
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "NoRepair" 1

  # Set Environment Variables

  WriteRegExpandStr ${env_hklm} TRIDASH_MODULE_PATHS "$INSTDIR\modules"
  WriteRegExpandStr ${env_hkcu} TRIDASH_MODULE_PATHS "$INSTDIR\modules"

  WriteRegExpandStr ${env_hklm} TRIDASH_JS_RUNTIME "$INSTDIR\backends\javascript\tridash.js"
  WriteRegExpandStr ${env_hkcu} TRIDASH_JS_RUNTIME "$INSTDIR\backends\javascript\tridash.js"

  # Add compiler to path

  EnVar::SetHKLM
  EnVar::AddValue "path" "$INSTDIR\bin"

  EnVar::SetHKCU
  EnVar::AddValue "path" "$INSTDIR\bin"

  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=500
SectionEnd

Function un.onInit
  MessageBox MB_OKCANCEL "Are you sure you want to uninstall ${APPNAME}?" IDOK next
      Abort
  next:
FunctionEnd

Section "uninstall"
  # Remove shortcuts from Start Menu
  rmDir /r "$SMPROGRAMS\${APPNAME}"

  # Delete Compiler
  rmDir /r $INSTDIR\bin

  # Delete core modules, leave any other modules in-place
  delete $INSTDIR\modules\core.trd
  delete $INSTDIR\modules\core.yml

  # Delete modules directory if empty
  rmDir $INSTDIR\modules
  # Delete runtime libraries
  rmDir /r $INSTDIR\backends

  # Delete uninstaller
  delete $INSTDIR\uninstall.exe

  # Delete install directory if empty
  rmDir $INSTDIR

  # Remove uninstaller information from the registry
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}"

  # Unset Environment Variables
  DeleteRegValue ${env_hklm} TRIDASH_MODULE_PATHS
  DeleteRegValue ${env_hkcu} TRIDASH_MODULE_PATHS

  DeleteRegValue ${env_hklm} TRIDASH_JS_RUNTIME
  DeleteRegValue ${env_hkcu} TRIDASH_JS_RUNTIME

  # Remove Compiler from path
  EnVar::SetHKLM
  EnVar::DeleteValue "path" "$INSTDIR\bin"

  EnVar::SetHKCU
  EnVar::DeleteValue "path" "$INSTDIR\bin"

  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=500
SectionEnd
