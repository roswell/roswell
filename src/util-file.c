/* -*- tab-width : 2 -*- */
#include "util.h"

#ifndef HAVE_WINDOWS_H

int delete_file(char* pathspec) {
  char* cmd;
  int ret;
  cmd=s_cat2(q("rm -f "),q(pathspec));
  ret=System(cmd);
  s(cmd);
  return ret==0;
}

int rename_file(char* file,char* new_name) {
  char* cmd;
  int ret;
  cmd=s_cat(q("mv "),q(file),q(" "),q(new_name),NULL);
  ret=System(cmd);
  s(cmd);
  return ret==0;
}

void touch(char* path) {
  int ret;
  cond_printf(1,"%s\n",path);
  char* cmd=s_cat2(q("touch "),q(path));
  ret=System(cmd);
  s(cmd);
}

int file_exist_p(char* path) {
  struct stat sb;
  return (stat(path, &sb) == 0 && S_ISREG(sb.st_mode))?1:0;
}

long file_mtime(char* path) {
  struct stat sb;
  return stat(path, &sb) == 0 ? sb.st_mtime:0;
}

int file_newer_p(char* a,char* b) {
  long at=file_mtime(a),bt=file_mtime(b);
  return bt==0?1:(at!=0&& at>=bt);
}
#endif

