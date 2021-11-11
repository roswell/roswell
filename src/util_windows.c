#include "util.h"

#ifdef HAVE_WINDOWS_H
int setenv(const char* name,const char* value,int overwrite) {
  char* s_=cat((char*)name,"=",(char*)value,NULL);
  _putenv(s_);
  s(s_);
}

int unsetenv(const char* name) {
  char* s_=cat((char*)name,"=",NULL);
  _putenv(s_);
  s(s_);
}

char* uname_s(void) {
  return q("windows");
}

char* uname_m(void) {
  char* carch=getenv("MSYSTEM_CARCH");
  if(carch && strcmp(carch,"x86_64")==0)
    return q("x86-64");
  if(carch && strcmp(carch,"i686")==0)
    return q("x86");
  char* msystem=getenv("MSYSTEM");
  if(msystem && strcmp(msystem,"MINGW64")==0)
    return q("x86-64");
  if(msystem && strcmp(msystem,"MINGW32")==0)
    return q("x86");
#if defined(_WIN64)
  return q("x86-64");
#elif defined(_WIN32)
  BOOL isWow64 = FALSE;
  LPFN_ISWOW64PROCESS fnIsWow64Process  = (LPFN_ISWOW64PROCESS)
    GetProcAddress(GetModuleHandle(TEXT("kernel32")),"IsWow64Process");
  if(fnIsWow64Process) {
    if(!fnIsWow64Process(GetCurrentProcess(), &isWow64))
      return q("x86");
    if(isWow64)
      return q("x86-64");
    else
      return q("x86");
  }
  else
    return q("x86");
#endif
}

char* which(char* cmd) {
  if((cmd[0]=='.' && cmd[1]=='/')|| /* relative path */
     position_char("/:",cmd)!=-1) { /* have no path element */
    cmd=substitute_char('\\','/',q(cmd));
    return truename(cmd);
  }
  char* which_cmd=cat("cmd /c where ",cmd,"",NULL);
  cond_printf(1,"which cmd:%s\n",which_cmd);
  char* p=system_(which_cmd);
  cond_printf(1,"which result:%s\n",p);
  p=p?substitute_char('\0','\r',substitute_char('\0','\n',p)):NULL;
  char* p2=p?remove_char("\r\n",p):q("");
  s(p),s(which_cmd);
  return p2;
}

LVal directory(char* path) {
  LVal ret=0;
  WIN32_FIND_DATA fd;
  char *p=cat(path,"*.*",NULL);
  HANDLE dir=FindFirstFile(p,&fd);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  do {
    if(!(strcmp(fd.cFileName,".")==0 ||
         strcmp(fd.cFileName,"..")==0)) {
      char* str=q(fd.cFileName);
      if(fd.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY)
        str=s_cat2(str,q(DIRSEP));
      ret=conss(str,ret);
    }
  }while(FindNextFile(dir,&fd)!=0);
  s(p);
  FindClose(dir);
  return ret;
}

void delete_at_exit(char* file_to_delete) {
}

void setup_uid(int euid_or_uid) {
}

int mklockdir(char* path) {
  return SHCreateDirectoryEx(NULL,path,NULL)!=ERROR_SUCCESS;
}

#endif
