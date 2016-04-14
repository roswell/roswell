/* -*- tab-width : 2 -*- */
#include "util.h"

#ifdef HAVE_WINDOWS_H

void DisplayError(char *pszAPI) {
  LPVOID lpvMessageBuffer;
  CHAR szPrintBuffer[512];
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM,
                NULL, GetLastError(),
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR)&lpvMessageBuffer, 0, NULL);

  wsprintf(szPrintBuffer,
           "ERROR: API    = %s.\n   error code = %d.\n   message    = %s.\n",
           pszAPI, GetLastError(), (char *)lpvMessageBuffer);
  fprintf(stderr,"%s",szPrintBuffer);
  LocalFree(lpvMessageBuffer);
  ExitProcess(GetLastError());
}

char* system_(char* cmd) {
  HANDLE hOutputReadTmp,hOutputRead,hOutputWrite;
  HANDLE hInputWriteTmp,hInputRead,hInputWrite;
  HANDLE hErrorWrite;
  SECURITY_ATTRIBUTES sa;
  CHAR lpBuffer[256];
  DWORD nBytesRead;
  DWORD ExitCode;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char* ret=q("");
  sa.nLength= sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  if (!CreatePipe(&hOutputReadTmp,&hOutputWrite,&sa,0))
    DisplayError("CreatePipe");

  if (!DuplicateHandle(GetCurrentProcess(),hOutputWrite,
                       GetCurrentProcess(),&hErrorWrite,0,
                       TRUE,DUPLICATE_SAME_ACCESS))
    DisplayError("DuplicateHandle");

  if (!CreatePipe(&hInputRead,&hInputWriteTmp,&sa,0))
    DisplayError("CreatePipe");

  if (!DuplicateHandle(GetCurrentProcess(),hOutputReadTmp,
                       GetCurrentProcess(),
                       &hOutputRead,0,FALSE,
                       DUPLICATE_SAME_ACCESS))
    DisplayError("DupliateHandle");

  if (!DuplicateHandle(GetCurrentProcess(),hInputWriteTmp,
                       GetCurrentProcess(),
                       &hInputWrite,0,FALSE,
                       DUPLICATE_SAME_ACCESS))
    DisplayError("DupliateHandle");

  if (!CloseHandle(hOutputReadTmp)) DisplayError("CloseHandle");
  if (!CloseHandle(hInputWriteTmp)) DisplayError("CloseHandle");
  ZeroMemory(&si,sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdOutput = hOutputWrite;
  si.hStdInput  = hInputRead;
  si.hStdError  = hErrorWrite;
  if (!CreateProcess(NULL,cmd,NULL,NULL,TRUE,0,NULL,NULL,&si,&pi))
    DisplayError("CreateProcess");
  if (!CloseHandle(pi.hThread)) DisplayError("CloseHandle");
  if (!CloseHandle(hOutputWrite)) DisplayError("CloseHandle");
  if (!CloseHandle(hInputRead )) DisplayError("CloseHandle");
  if (!CloseHandle(hErrorWrite)) DisplayError("CloseHandle");
  while(1) {
    if (!ReadFile(hOutputRead,lpBuffer,sizeof(lpBuffer),
                  &nBytesRead,NULL) || !nBytesRead) {
      if (GetLastError() == ERROR_BROKEN_PIPE)
        break;
      else
        DisplayError("ReadFile");
    }
    lpBuffer[nBytesRead]='\0';
    ret=s_cat(ret,q(lpBuffer),NULL);
  }
  if (!CloseHandle(hOutputRead)) DisplayError("CloseHandle");
  if (!CloseHandle(hInputWrite)) DisplayError("CloseHandle");
  if (!GetExitCodeProcess(pi.hProcess,&ExitCode) || ExitCode) {
    s(ret);
    ret=NULL;
  }
  return ret;
}

int system_redirect(const char* cmd,char* filename) {
}

int System(const char* command) {
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  DWORD ExitCode;
  cond_printf(1,"System:'%s'\n",command);
  ZeroMemory(&si,sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  if(!CreateProcess(NULL,(char*)command,NULL,NULL,FALSE,0,NULL,NULL,&si,&pi))
    DisplayError("CreateProcess");
  if(!CloseHandle(pi.hThread)) DisplayError("CloseHandle");
  DWORD r=WaitForSingleObject(pi.hProcess, INFINITE);
  if(WAIT_OBJECT_0!=r)
    return 1;
  if(!GetExitCodeProcess(pi.hProcess,&ExitCode)||ExitCode)
    return ExitCode||1;
  return 0;
}

BOOL WINAPI ConsoleCtrlHandler(DWORD ctrlChar) {
  if(CTRL_C_EVENT == ctrlChar)
    return TRUE;
  return FALSE;
}

void exec_arg(char** arg) {
  int i;
  char* cmd=q(arg[0]);
  for(i=1;arg[i]!=NULL;++i) {
    cmd=s_cat(cmd,q(" "),q("\""),escape_string(arg[i]),q("\""),NULL);
  }
  SetConsoleCtrlHandler(ConsoleCtrlHandler, TRUE);
  exit(System(cmd));
  s(cmd);
}
#endif
