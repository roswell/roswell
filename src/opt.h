/* -*- tab-width : 2 -*- */
#ifndef __OPT_H__
#define __OPT_H__
#include "util.h"
struct sub_command;
typedef LVal (*sub_command_fnc)(LVal arg,struct sub_command* cmd);
#define DEF_SUBCMD(fname) LVal fname(LVal arg_,struct sub_command* cmd)

struct opts {
  const char* name;
  int type;
  const char* value;
  struct opts* next;
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

extern struct opts* global_opt;
extern struct opts* local_opt;
extern int quicklisp;
extern int module;
extern int rc;
DEF_SUBCMD(opt_version);
DEF_SUBCMD(cmd_internal);

LVal add_command(LVal cmd,const char* name,const char* short_name,sub_command_fnc call,int show_opt,int terminating);
struct opts* load_opts(const char* path);
int save_opts(const char* path,struct opts* opt);
int set_opt(struct opts** opts,const char* name,char* value);
int unset_opt(struct opts** opts,const char* name);
char* get_opt(const char* name,int env);
char* _get_opt(struct opts* opt,const char* name);
char* sexp_opts(struct opts* opt);
void free_opts(struct opts* opt);
DEF_SUBCMD (cmd_tar);

struct proc_opt {
  char* name;
  LVal option;
  LVal command;
  LVal alias;
  LVal top;
};

void dispatch_init(struct proc_opt *popt,char* name_);
LVal dispatch(LVal arg,struct proc_opt *popt);
void register_top(struct proc_opt* opt);
extern struct proc_opt top;

#endif
