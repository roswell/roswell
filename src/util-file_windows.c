#include "util.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_WINDOWS_H

int delete_file(char* pathspec) {
  /*#error not implemented delete_file*/
}

int rename_file(char* file,char* new_name) {
  return MoveFileEx(file,new_name,MOVEFILE_REPLACE_EXISTING);
}

void touch(char* path) {
  int ret;
  cond_printf(1,"%s\n",path);
}
int file_exist_p (char* path) {
  WIN32_FIND_DATA fd;
  HANDLE dir=FindFirstFile(path,&fd);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  FindClose(dir);
  return 1;
}

long file_mtime(char* path) {
  return 0;
}

int file_newer_p(char * a,char* b) {
  return 0;
}

int file_write_data(char* path, char* data, size_t size) {
  FILE* file = fopen(path, "wb");
  if (file == NULL) {
    return -1;
  }
  size_t out = fwrite(data, sizeof(char), size, file);
  fclose(file);
  return out!=size?-1:0;
}
#endif
