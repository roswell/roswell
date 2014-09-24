#ifndef __PULL_H__
#define __PULL_H__

struct install_options
{
  char* impl;
  char* version;
  char* arch;
};

typedef int (*install_cmds)(struct install_options* param);
typedef char* (*install_cmd_string)(struct install_options* param);

int start(struct install_options* param);
int download(struct install_options* param);
int expand(struct install_options* param);

extern install_cmds install_sbcl_full[];
extern install_cmds install_sbcl_bin_full[];

struct install_impls
{
  const char* name;
  install_cmds *call;
  install_cmd_string uri;
  install_cmd_string extention;

};

extern struct install_impls impls_sbcl_bin;
extern struct install_impls impls_sbcl;

#endif
