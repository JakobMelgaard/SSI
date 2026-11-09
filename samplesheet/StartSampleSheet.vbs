' Starts the sample sheet builder: finds R, installs the packages the app needs (first time only,
' needs internet) and opens the app in its own window. Put this file in the same folder as app.R
' (and options.R / indexDB.xlsx) and double-click it. Closing the app window stops the app.
Option Explicit

Const PACKAGES = "'shiny','readxl','xml2','jsonlite','openxlsx'"
Const TITLE = "Sample sheet builder"

Dim fso, sh, appDir, rexe
Set fso = CreateObject("Scripting.FileSystemObject")
Set sh  = CreateObject("WScript.Shell")
appDir  = fso.GetParentFolderName(WScript.ScriptFullName)

If Not fso.FileExists(appDir & "\app.R") Then
  MsgBox "app.R was not found next to this file (" & appDir & ").", 16, TITLE
  WScript.Quit 1
End If

rexe = FindRscript()
If rexe = "" Then
  MsgBox "R was not found on this PC. Install R from https://cloud.r-project.org and run this file again.", 16, TITLE
  WScript.Quit 1
End If

sh.CurrentDirectory = appDir

' --- 1. Are all packages installed? If not, install them (visible window, takes a few minutes) ---
If Not PackagesOk() Then
  MsgBox "The first time, some R packages have to be installed (needs internet, takes a few minutes)." & vbCrLf & _
         "A black window shows the progress. Click OK to start.", 64, TITLE
  sh.Run Quote(rexe) & " -e " & Quote(InstallCode()), 1, True
  If Not PackagesOk() Then
    MsgBox "The packages could not be installed. Check the internet connection and try again.", 16, TITLE
    WScript.Quit 1
  End If
End If

' --- 2. Write the little R script that starts the app, and run it without a console window ---
Dim launcher, f
launcher = sh.ExpandEnvironmentStrings("%TEMP%") & "\samplesheet_launch.R"
Set f = fso.CreateTextFile(launcher, True)
WriteLauncher f
f.Close

Dim rc, logPath
logPath = appDir & "\app.log"
rc = sh.Run(Quote(rexe) & " --vanilla " & Quote(launcher) & " " & Quote(appDir), 0, True)
If rc <> 0 Then
  MsgBox "The app stopped with an error. See the end of app.log:" & vbCrLf & vbCrLf & LogTail(logPath, 12), 16, TITLE
End If

' ---------------------------------------------------------------------------------------------

Function Quote(s)
  Quote = Chr(34) & s & Chr(34)
End Function

' R code that makes the user package folder known (it is created when packages are installed)
Function LibCode()
  LibCode = "lib <- Sys.getenv('R_LIBS_USER'); dir.create(lib, recursive = TRUE, showWarnings = FALSE); .libPaths(c(lib, .libPaths())); "
End Function

Function PackagesOk()
  Dim rc
  rc = sh.Run(Quote(rexe) & " --vanilla -e " & Quote(LibCode() & "p <- c(" & PACKAGES & "); " & _
              "q(status = if (all(sapply(p, requireNamespace, quietly = TRUE))) 0 else 1)"), 0, True)
  PackagesOk = (rc = 0)
End Function

Function InstallCode()
  InstallCode = LibCode() & "p <- c(" & PACKAGES & "); p <- p[!sapply(p, requireNamespace, quietly = TRUE)]; " & _
                "if (length(p)) install.packages(p, lib = lib, repos = 'https://cloud.r-project.org')"
End Function

' Writes the R script that starts the app (every time, to the temp folder)
Sub WriteLauncher(f)
  f.WriteLine "appdir <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = '/')"
  f.WriteLine "lib <- Sys.getenv('R_LIBS_USER'); .libPaths(c(lib, .libPaths()))"
  f.WriteLine "log <- file(file.path(appdir, 'app.log'), open = 'wt'); sink(log); sink(log, type = 'message')"
  f.WriteLine "message('R ', as.character(getRversion()))"
  f.WriteLine "library(shiny)"
  f.WriteLine "opt <- file.path(appdir, 'options.R')"
  f.WriteLine "if (file.exists(opt)) source(opt)   # your dropdown choices, if they are in a file called options.R"
  f.WriteLine "find_browser <- function() {"
  f.WriteLine "  pf <- Sys.getenv('ProgramFiles'); pf86 <- Sys.getenv('ProgramFiles(x86)'); loc <- Sys.getenv('LOCALAPPDATA')"
  f.WriteLine "  cand <- c(file.path(pf86, 'Microsoft', 'Edge', 'Application', 'msedge.exe'),"
  f.WriteLine "            file.path(pf, 'Microsoft', 'Edge', 'Application', 'msedge.exe'),"
  f.WriteLine "            file.path(pf, 'Google', 'Chrome', 'Application', 'chrome.exe'),"
  f.WriteLine "            file.path(pf86, 'Google', 'Chrome', 'Application', 'chrome.exe'),"
  f.WriteLine "            file.path(loc, 'Google', 'Chrome', 'Application', 'chrome.exe'))"
  f.WriteLine "  found <- cand[file.exists(cand)]"
  f.WriteLine "  if (length(found)) found[1] else NULL"
  f.WriteLine "}"
  f.WriteLine "open_window <- function(url) {"
  f.WriteLine "  exe <- find_browser()"
  f.WriteLine "  if (is.null(exe)) utils::browseURL(url) else system2(exe, c(paste0('--app=', url), '--start-maximized'), wait = FALSE)"
  f.WriteLine "}"
  f.WriteLine "options(samplesheet.autostop = TRUE)"
  f.WriteLine "shiny::runApp(appdir, host = '127.0.0.1', launch.browser = open_window)"
End Sub

Function FindRscript()
  Dim keys, k, p, base, bases, folder, sub1, best
  FindRscript = ""
  ' 1. the install path R writes into the registry
  keys = Array("HKLM\SOFTWARE\R-core\R\InstallPath", "HKCU\SOFTWARE\R-core\R\InstallPath", _
               "HKLM\SOFTWARE\R-core\R64\InstallPath")
  On Error Resume Next
  For Each k In keys
    p = ""
    p = sh.RegRead(k)
    If p <> "" Then
      If fso.FileExists(p & "\bin\Rscript.exe") Then FindRscript = p & "\bin\Rscript.exe": Exit Function
    End If
  Next
  On Error GoTo 0
  ' 2. the usual folders; the newest version wins
  bases = Array(sh.ExpandEnvironmentStrings("%ProgramFiles%") & "\R", _
                sh.ExpandEnvironmentStrings("%ProgramFiles(x86)%") & "\R", _
                sh.ExpandEnvironmentStrings("%LOCALAPPDATA%") & "\Programs\R")
  For Each base In bases
    If fso.FolderExists(base) Then
      best = ""
      For Each sub1 In fso.GetFolder(base).SubFolders
        If fso.FileExists(sub1.Path & "\bin\Rscript.exe") Then
          If StrComp(sub1.Name, best, vbTextCompare) > 0 Then best = sub1.Name
        End If
      Next
      If best <> "" Then FindRscript = base & "\" & best & "\bin\Rscript.exe": Exit Function
    End If
  Next
End Function

' The last n lines of a text file
Function LogTail(path, n)
  Dim ts, txt, lines, i, first
  LogTail = "(no log)"
  If Not fso.FileExists(path) Then Exit Function
  Set ts = fso.OpenTextFile(path, 1)
  If ts.AtEndOfStream Then ts.Close: Exit Function
  txt = ts.ReadAll
  ts.Close
  lines = Split(txt, vbLf)
  first = UBound(lines) - n
  If first < 0 Then first = 0
  LogTail = ""
  For i = first To UBound(lines)
    LogTail = LogTail & lines(i) & vbCrLf
  Next
End Function
