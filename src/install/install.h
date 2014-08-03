#ifndef __INSTALL_H__
#define __INSTALL_H__

typedef int (*install_cmds)(char* impl,char* version);
typedef char* (*install_cmd_string)(char* impl,char* version);
int start(char* impl,char* version);
int download(char* impl,char* version);
int expand(char* impl,char* version);

extern install_cmds install_sbcl_full[];
extern install_cmds install_sbcl_bin_full[];

struct install_impls
{
  const char* name;
  install_cmds *call;
  install_cmd_string version;
  install_cmd_string uri;
  install_cmd_string extention;

};

extern struct install_impls impls_sbcl_bin;
extern struct install_impls impls_sbcl;

#endif
