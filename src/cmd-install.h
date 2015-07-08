#ifndef __PULL_H__
#define __PULL_H__

struct install_options {
  char* impl;
  char* version;
  char* os;
  char* arch;
  int arch_in_archive_name;
  void* opt;
  char* expand_path; /*expand dist */
};

typedef int (*install_cmds)(struct install_options* param);
typedef char* (*install_cmd_string)(struct install_options* param);

int start(struct install_options* param);
int download(struct install_options* param);
int expand(struct install_options* param);

extern install_cmds install_sbcl_bin_full[];

struct install_impls {
  const char* name;
  install_cmds *call;
  char* uri;
  install_cmd_string extention;
  int util;
};

extern struct install_impls impls_sbcl_bin;
extern struct install_impls utils_quicklisp;

char* download_archive_name(struct install_options* param);
#endif
