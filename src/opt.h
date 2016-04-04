/* -*- tab-width : 2 -*- */
#ifndef __OPT_H__
#define __OPT_H__
#include "util.h"
struct sub_command;
typedef int (*sub_command_fnc)(int argc,char **argv,struct sub_command* cmd);

struct opts {
  const char* name;
  const char* value;
  struct opts* next;
};

enum opt_type {
  OPT_VOID,
  OPT_INT,
  OPT_STRING,
  OPT_BOOL
};

struct sub_command {
  const char* name;
  const char* short_name;
  sub_command_fnc call;
  int show_opt;
  int terminating;
};

#define OPT_SHOW_NONE (0)
#define OPT_SHOW_HELP (1)
//#define OPT_SHOW_

extern LVal top_commands;
extern LVal top_options;
extern struct opts* global_opt;
extern struct opts* local_opt;
extern int quicklisp;
extern int rc;

LVal add_command(LVal cmd,const char* name,const char* short_name,sub_command_fnc call,int show_opt,int terminating);
struct opts* load_opts(const char* path);
int save_opts(const char* path,struct opts* opt);
int set_opt(struct opts** opts,const char* name,char* value);
int unset_opt(struct opts** opts,const char* name);
char* get_opt(const char* name,int env);
char* _get_opt(struct opts* opt,const char* name);
char* sexp_opts(struct opts* opt);
void free_opts(struct opts* opt);
int cmd_tar(int argc,char **argv,struct sub_command* cmd);
#endif
