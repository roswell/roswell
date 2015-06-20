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
#else

char* system_(char* cmd) {
  FILE *fp;
  char buf[256];
  char* s=q("");
  if((fp=popen(cmd,"r")) ==NULL) {
    printf("Error:%s\n",cmd);
    exit(EXIT_FAILURE);
  }
  while(fgets(buf,256,fp) !=NULL) {
    s=s_cat2(s,q(buf));
  }
  (void)pclose(fp);
  return s;
}
#endif


int system_redirect(const char* cmd,char* filename) {
#ifndef HAVE_WINDOWS_H
  pid_t pid;
  int fd[2];
  int c;
  if (pipe(fd)==-1) {
    perror("pipe");
    return -1;
  }
  pid=fork();
  if(pid==-1) {
    perror("fork");
    return -1;
  }
  if(pid==0) {
    int argc;
    char** argv=parse_cmdline((char*)cmd,&argc);
    /* standard output */
    close(fd[0]);
    close(1),close(2);
    dup2(fd[1],1),dup2(fd[1],2);
    close(fd[1]);
    execvp(argv[0],argv);
  }else {
    FILE *in,*out;
    close(fd[1]);
    if((out=fopen(filename,"a"))!=NULL) {
      if((in=fdopen(fd[0], "r"))!=NULL) {
        while((c = fgetc(in)) != EOF) {
          if (fputc(c, out) == EOF) {
            fclose(in);
            fclose(out);
            return 0;
          }
        }
        fclose(in);
      }
      fclose(out);
    }
  }
  return(0);
#endif
}

int system_redirect_function(const char* cmd,Function1 f) {
#ifndef HAVE_WINDOWS_H
  pid_t pid;
  int fd[2];
  if (pipe(fd)==-1) {
    perror("pipe");
    return -1;
  }
  pid=fork();
  if(pid==-1) {
    perror("fork");
    return -1;
  }
  if(pid==0) {
    int argc;
    char** argv=parse_cmdline((char*)cmd,&argc);
    /* standard output */
    close(fd[0]);
    close(1),close(2);
    dup2(fd[1],1),dup2(fd[1],2);
    close(fd[1]);
    execvp(argv[0],argv);
  }else {
    FILE *in;
    close(fd[1]);
    if((in=fdopen(fd[0], "r"))!=NULL) {
      f((LVal)in);
      fclose(in);
    }
  }
  return(0);
#endif
}

int System(const char* command) {
#ifndef HAVE_WINDOWS_H
  return system(command);
#else
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  DWORD ExitCode;
  if(verbose)
    fprintf(stderr,"System:'%s'\n",command);
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
#endif
}
