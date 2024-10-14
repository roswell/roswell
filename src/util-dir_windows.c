#include "util.h"

#ifdef HAVE_WINDOWS_H

char* homedir(void) {
  TCHAR szAppData[MAX_PATH];
  return (SUCCEEDED(SHGetFolderPath(NULL, CSIDL_PROFILE, NULL, 0, szAppData)))?q_(szAppData):NULL;
}

char* truename(const char* path) {
  char ret[MAX_PATH];
  DWORD dwret;
  dwret=GetFullPathName(path,sizeof(ret)/sizeof(ret[0]),ret,NULL);
  return dwret?q(ret):which((char*)path);
}

int ensure_directories_exist (char* path) {
  int len = strlen(path);
  cond_printf(1,"ensure_directories_exist:%s\n",path);
  if(len) {
    for(--len;(path[len]!=DIRSEP[0]||len==-1);--len);
    path=subseq(path,0,len+1);
  }else path=q(path);
  if(!directory_exist_p(path)) {
    SHCreateDirectoryEx(NULL,path,NULL);
    s(path);
  }
  return 1;
}

int directory_exist_p (char* path) {
  WIN32_FIND_DATA fd;
  char *p=cat(path,"*.*",NULL);
  HANDLE dir=FindFirstFile(p,&fd);
  s(p);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  FindClose(dir);
  return 1;
}


int change_directory(const char* path) {
  cond_printf(1,"change_directory:%s\n",path);
  return _chdir(path);
}

int delete_directory(char* pathspec,int recursive) {
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
}

char* currentdir(void) {
  char buf[2048];
  return append_trail_slash(q_(_getcwd(buf,2048)));
}

int is_valid_path(const char *path) {
  //
  // On Windows, an absolute path can technically
  // start with a drive letter (i.e. c:\roswell),
  // but could also be a UNC path (i.e. \\OTHERMACHINE\roswell).
  //
  // But, if invoked inside an msys2 shell we could get a UNIX-style path.
  //
  // It appears however that the latter two scenarios are not supported
  // by other parts of the codebase, so we shall enforce here that only
  // regular Windows-style paths are allowed.
  //

  return (isalpha(path[0]) && path[1] == ':'  && (path[2] == '\\' || path[2] == '/'));
}

#endif
