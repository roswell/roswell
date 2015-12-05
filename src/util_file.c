#include "util.h"

int delete_file(char* pathspec) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  cmd=s_cat2(q("rm -f "),q(pathspec));
  ret=System(cmd);
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
  ret=System(cmd);
  s(cmd);
  return ret==0;
#else
  return MoveFileEx(file,new_name,MOVEFILE_REPLACE_EXISTING);
#endif
}

void touch(char* path) {
  int ret;
  cond_printf(1,"%s\n",path);
#ifndef HAVE_WINDOWS_H
  char* cmd=s_cat2(q("touch "),q(path));
  ret=System(cmd);
  s(cmd);
#else
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

int file_newer_p(char * a,char* b) {
#ifndef HAVE_WINDOWS_H
  struct stat as,bs;
  if(stat(b, &bs) != 0)
    return 1;
  return (stat(a, &as) == 0 && as.st_mtime >= bs.st_mtime);
#else
  return 0;
#endif
}
