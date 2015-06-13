#include "util.h"

void* alloc(size_t bytes) {
  void* p=malloc(bytes);
  //  printf("**%d\n",p);
  return p;
}

void dealloc(void* f) {
  free(f);
}

char* q_(const char* orig) {
  char* ret= (char*)alloc(strlen(orig)+1);
  strcpy(ret,orig);
  return ret;
}

char* q_internal(const char* orig,char* file,int line) {
  if(verbose>1)
    fprintf(stderr,"%s %d q(%s) %lu \n",file,line,orig,(intptr_t)orig);
  char* ret= (char*)alloc(strlen(orig)+1);
  strcpy(ret,orig);
  return ret;
}

void s_internal(char* f,char* name,char* file,int line) {
  if(verbose>1)
    fprintf(stderr,"%s %d s(%s) %lu \n",file,line,name,(intptr_t)f);
  dealloc(f);
}

#ifdef _WIN32
setenv(const char* name,const char* value,int overwrite) {
  char* s=cat((char*)name,"=",(char*)value,NULL);
  _putenv(s);
  s(name);
}
#endif

int delete_file(char* pathspec) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  cmd=s_cat2(q("rm -f "),q(pathspec));
  ret=system(cmd);
  s(cmd);
  return ret==0;
#else
//  #error not implemented delete_file
#endif  
}

int rename_file(char* file,char* new_name) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  cmd=s_cat(q("mv "),q(file),q(" "),q(new_name),NULL);
  ret=system(cmd);
  s(cmd);
  return ret==0;
#else
  return MoveFileEx(file,new_name,MOVEFILE_REPLACE_EXISTING);
#endif
}

void touch(char* path) {
  int ret;
#ifndef HAVE_WINDOWS_H
  char* cmd=s_cat2(q("touch "),q(path));
#else
  char* cmd=q("");
#endif
  ret=system(cmd);
  s(cmd);
}

#ifdef HAVE_WINDOWS_H
void DisplayError(char *pszAPI) {
  LPVOID lpvMessageBuffer;
  CHAR szPrintBuffer[512];
  DWORD nCharsWritten;
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
  HANDLE hThread;
  DWORD ThreadId;
  SECURITY_ATTRIBUTES sa;
  CHAR lpBuffer[256];
  DWORD nBytesRead;
  DWORD nCharsWritten;
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
                       &hOutputRead, // Address of new handle.
                       0,FALSE, // Make it uninheritable.
                       DUPLICATE_SAME_ACCESS))
    DisplayError("DupliateHandle");

  if (!DuplicateHandle(GetCurrentProcess(),hInputWriteTmp,
                       GetCurrentProcess(),
                       &hInputWrite, // Address of new handle.
                       0,FALSE, // Make it uninheritable.
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
  if (!CreateProcess(NULL,cmd,NULL,NULL,TRUE,CREATE_NO_WINDOW,NULL,NULL,&si,&pi))
    DisplayError("CreateProcess");
  if (!CloseHandle(pi.hThread)) DisplayError("CloseHandle");
  if (!CloseHandle(hOutputWrite)) DisplayError("CloseHandle");
  if (!CloseHandle(hInputRead )) DisplayError("CloseHandle");
  if (!CloseHandle(hErrorWrite)) DisplayError("CloseHandle");
  while(1) {
    if (!ReadFile(hOutputRead,lpBuffer,sizeof(lpBuffer),
                  &nBytesRead,NULL) || !nBytesRead) {
      if (GetLastError() == ERROR_BROKEN_PIPE)
        break; // pipe done - normal exit path.
      else
        DisplayError("ReadFile"); // Something bad happened.
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

char* s_decode(char* str) {
  int count,i,write,escape=0;
  char* ret;
  for(write=0;write<2;++write) {
    for(i=0,count=0;str[i]!='\0';++i,++count) {
      if(!escape && str[i]=='\\' && str[i+1]!='\0')
        escape=1,--count;
      else {
        if(write) {
          ret[count]=str[i];
          if(escape) {
            switch(str[i]) {
            case 'n':
              ret[count]='\n';
              break;
            case 'r':
              ret[count]='\r';
            }
          }
        }
        escape=0;
      }
    }
    if(!write) {
      ret=alloc(sizeof(char)*(count+1));
      ret[count]='\0';
    }
  }
  s(str);
  return ret;
}

char** parse_cmdline(char* cmdline,int *argc) {
  int i,write,mode=0;
  int count,last;
  char** ret;
  for(write=0;write<2;++write) {
    last=0,mode=0,count=0;
    for(i=0;cmdline[i]!='\0';++i) {
      if(cmdline[i]==' '||cmdline[i]=='\t') {
        if(i!=0) {
          if(write) {
            ret[count]=subseq(cmdline,last,i-mode);
            if(mode) {
              ret[count]=s_decode(ret[count]);
              mode=0;
            }
          }
          ++count;
        }
        for(;cmdline[i]==' '||cmdline[i]=='\t'||cmdline[i]=='\0';++i);
        last=i--;
      }else if(cmdline[i]=='"') {
        last=++i;
        mode=1;
        for(;cmdline[i]!='"'&&cmdline[i]!='\0';++i)
          if(cmdline[i]=='\\'&&cmdline[i+1]!='\0')
            ++i;
      }
    }
    if(last+1!=i) {
      if(write) {
        ret[count]=subseq(cmdline,last,i-mode);
        if(mode) {
          ret[count]=s_decode(ret[count]);
          mode=0;
        }
      }
      ++count;
    }
    if(!write)
      ret=alloc(sizeof(char**)*(count+1));
  }
  ret[count]=NULL;
  *argc=count;
  return ret;
}

int free_cmdline(char** argv) {
  char** p;
  for(p=argv;*p!=NULL;++p) {
    dealloc(*p);
  }
  dealloc(argv);
  return 1;
}

int system_redirect(const char* cmd,char* filename) {
#ifndef HAVE_WINDOWS_H
  pid_t pid;
  int fd[2];
  char c;
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
  char c;
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
    if((in=fdopen(fd[0], "r"))!=NULL) {
      f((LVal)in);
      fclose(in);
    }
  }
  return(0);
#endif
}

char* uname(void) {
#ifdef __CYGWIN__
  return q("windows");
#endif
#ifndef HAVE_WINDOWS_H
  char *p=system_("uname");
  char *p2;
  p2=remove_char("\r\n",p);
  s(p);
  return downcase(p2);
#else
  return q("windows");
#endif
}

char* uname_m(void) {
#if defined(_WIN64)
  return q("x86-64");
#endif
#ifdef __CYGWIN__
  return q("x86");
#endif
#ifndef HAVE_WINDOWS_H
  char *p=system_("uname -m");
  char *p2;
  p2=remove_char("\r\n",p);
  s(p);
  if(strcmp(p2,"i686")==0) {
    s(p2);
    return q("x86");
  }
  if(strcmp(p2,"amd64")==0) {
    s(p2);
    return q("x86-64");
  }
  if(strcmp(p2,"armv6l")==0 ||
     strcmp(p2,"rmv7l")==0) {
    char* result=system_("readelf -A /proc/self/exe |grep Tag_ABI_VFP_args|wc -l");
    char* result2=remove_char("\r\n",result);
    s(result);
    if(strcmp(result2,"0")!=0) {
      s(result2);
      return q("armel");
    }else {
      s(result2);
      return q("armhf");
    }
  }
  return substitute_char('-','_',p2);
#else
  /*TBD check x86 or x86-64 */
  return q("x86");
#endif
}

char* which(char* cmd) {
#ifndef HAVE_WINDOWS_H
  char* which_cmd=cat("command -v \"",cmd,"\"",NULL);
#else
  if((cmd[0]=='.' && cmd[1]=='/')|| /* relative path */
     position_char("/:",cmd)!=-1) { /* have no path element */
    cmd=substitute_char('\\','/',q(cmd));
    return truename(cmd);
  }
  char* which_cmd=cat("cmd /c where ",cmd,"",NULL);
#endif
  char* p=system_(which_cmd);
  if(verbose>1)
    fprintf(stderr,"system_ result:%s\n",p);
  p=substitute_char('\0','\r',substitute_char('\0','\n',p));
  char* p2=p?remove_char("\r\n",p):q("");
  s(p),s(which_cmd);
  return p2;
}

LVal directory(char* path) {
  LVal ret=0;
#ifndef HAVE_WINDOWS_H
  DIR* dir=opendir(path);
  struct dirent *dirent;

  if(dir==NULL)
    return 0;
  while((dirent=readdir(dir))!=0) {
    char* str=q(dirent->d_name);
    if(dirent->d_type&DT_DIR)
      str=s_cat2(str,q("/"));
    ret=conss(str,ret);
  }
  closedir(dir);
#else
  WIN32_FIND_DATA fd;
  char *p=cat(path,"*.*",NULL);
  HANDLE dir=FindFirstFile(p,&fd);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  do {
    if(!(strcmp(fd.cFileName,".")==0 ||
         strcmp(fd.cFileName,"..")==0)) {
      char* str=cat(fd.cFileName,NULL);
      if(fd.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY)
        str=s_cat2(str,q(SLASH));
      ret=conss(str,ret);
    }
  }while(FindNextFile(dir,&fd)!=0);
  s(p);
  FindClose(dir);
#endif
  return ret;
}

void signal_callback_handler(int signum) {
  printf("Caught signal %d\n",signum);
  exit(1);
}

char* atexit_delete=NULL;

void atexit_handler(void) {
  delete_file(atexit_delete);
  s(atexit_delete);
}

void setup_signal_handler (char* file_to_delete) {
#ifndef HAVE_WINDOWS_H
  atexit_delete=q(file_to_delete);
  signal(SIGHUP,  signal_callback_handler);
  signal(SIGINT,  signal_callback_handler);
  signal(SIGPIPE, signal_callback_handler);
  signal(SIGQUIT, signal_callback_handler);
  signal(SIGTERM, signal_callback_handler);
  atexit(atexit_handler);
#endif
}
