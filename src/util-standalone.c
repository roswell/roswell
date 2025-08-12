#include "util.h"
#include "gend.h"
#include <stdio.h>

void init_standalone_dir() {
  if (!directory_exist_p(LISP_PATH)) {
    char *dir = append_trail_slash(q(LISP_PATH));
    ensure_directories_exist(dir);
    s(dir);
    init_standalone_lisp_files();
  }
  if (!directory_exist_p(PATCH_PATH)) {
    char *dir = append_trail_slash(q(PATCH_PATH));
    ensure_directories_exist(dir);
    s(dir);
    init_standalone_patch_files();
  }
}

void write_standalone_file(const char *dir, const char* name, char *data, unsigned int size) {
  char *file_name = s_cat(append_trail_slash(q(dir)), q(name), NULL);
  if (!file_exist_p(file_name)) {
    file_write_data(file_name, data, size);
  }
  s(file_name);
}
