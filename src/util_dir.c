#include "util.h"

char* homedir(void) {
  char *postfix="_HOME";
  char *c=s_cat2(upcase(q_(PACKAGE)),q_(postfix));
  char *env=NULL;
  env=getenv(c);
  s(c);
  if(env)
    return append_trail_slash(q(env));
#ifdef HAVE_WINDOWS_H
  TCHAR szAppData[MAX_PATH];
  if(SUCCEEDED(SHGetFolderPath(NULL, CSIDL_PROFILE, NULL, 0, szAppData)))
    c=q_(szAppData);
#else
  char* user=getenv("SUDO_USER");
  struct passwd *pwd= getpwuid(getuid());
  if(user)
    pwd=getpwnam(user);
  if(pwd)
    c=q_(pwd->pw_dir);
#endif
  else
    return NULL; /* error? */
  return append_trail_slash(c);
}

char* configdir(void) {
  char* home=homedir();
  if(home)
    return s_cat(home,q_("."),q(PACKAGE),q(SLASH),NULL);
  return NULL;
}

char* subcmddir(void) {
  return lispdir();
}

char* truename(const char* path) {
#ifndef HAVE_WINDOWS_H
  char* ret=realpath(path,NULL);
  if(ret)
    return ret;
#else
  char ret[MAX_PATH];
  DWORD dwret;
  dwret=GetFullPathName(path,sizeof(ret)/sizeof(ret[0]),ret,NULL);
  if(dwret)
    return q(ret);
#endif
  return which((char*)path);
}

char* pathname_directory(char* path) {
  int i;
  char* ret;
  for(i=strlen(path)-1;i>=0&&path[i]!=SLASH[0];--i);
  ret=append_trail_slash(subseq(path,0,i));
  s(path);
  return ret;
}

char* file_namestring(char* path) {
  int i;
  char* ret;
  for(i=strlen(path)-1;i>=0&&path[i]!='/';--i);
  if(path[i]=='/') {
    ret=subseq(path,i+1,0);
    s(path);
  }else
    ret=path;
  return ret;
}

int ensure_directories_exist (char* path) {
  int len = strlen(path);
  cond_printf(1,"ensure_directories_exist:%s\n",path);
  if(len) {
    for(--len;(path[len]!=SLASH[0]||len==-1);--len);
    path=subseq(path,0,len+1);
  }else path=q(path);
  if(!directory_exist_p(path)) {
#ifndef HAVE_WINDOWS_H
  pid_t  pid=fork();
  if(pid==-1) {
    perror("fork");
    return 0;
  }
  if(pid==0) {
    char* cmd=cat("mkdir -p ",path,NULL);
    setup_uid(0);
    if(System(cmd)!=0) {
      fprintf(stderr,"failed:%s\n",cmd);
      exit(1);
    }
    s(cmd);
    exit(0);
  }else {
    int status;
    s(path);
    waitpid(pid,&status,0);
    return WEXITSTATUS(status);
  }
#else
    SHCreateDirectoryEx(NULL,path,NULL);
#endif
    s(path);
  }
  return 1;
}

int directory_exist_p (char* path) {
#ifndef HAVE_WINDOWS_H
  struct stat sb;
  int ret=0;
  if (stat(path, &sb) == 0 && S_ISDIR(sb.st_mode)) {
    ret=1;
  }
  return ret;
#else
  WIN32_FIND_DATA fd;
  char *p=cat(path,"*.*",NULL);
  HANDLE dir=FindFirstFile(p,&fd);
  s(p);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  FindClose(dir);
  return 1;
#endif
}

int file_exist_p (char* path) {
#ifndef HAVE_WINDOWS_H
  struct stat sb;
  int ret=0;
  if (stat(path, &sb) == 0 && S_ISREG(sb.st_mode)) {
    ret=1;
  }
  return ret;
#else
  WIN32_FIND_DATA fd;
  HANDLE dir=FindFirstFile(path,&fd);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  FindClose(dir);
  return 1;
#endif
}

int change_directory(const char* path) {
#ifndef _WIN32
  return chdir(path);
#else
  return _chdir(path);
#endif
}

int delete_directory(char* pathspec,int recursive) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  if(recursive) {
    cmd=s_cat2(q("rm -rf "),q(pathspec));
  }else {
    cmd=s_cat2(q("rmdir "),q(pathspec));
  }
  ret=System(cmd);
  s(cmd);
  return ret==0;
#else
  if(!recursive) {
    return(!!RemoveDirectory(pathspec));
  }else {
    SHFILEOPSTRUCT fs;
    ZeroMemory(&fs, sizeof(SHFILEOPSTRUCT));
    fs.hwnd = NULL;
    fs.wFunc = FO_DELETE;
    fs.pFrom = pathspec;
    fs.pTo = NULL;
    fs.fFlags=FOF_SIMPLEPROGRESS|FOF_NOCONFIRMATION;
    return (SHFileOperation(&fs) == 0);
  }
#endif  
}
