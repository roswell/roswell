#ifndef  __OPT_H__
#define __OPT_H__
#include "util.h"
struct sub_command;
typedef int (*sub_command_fnc)(int argc,char **argv,struct sub_command* cmd);

struct opts
{
  const char* name;
  int type;
  const char* value;
  struct opts* next;
};

enum opt_type {
  OPT_VOID,
  OPT_INT,
  OPT_STRING,
  OPT_BOOL
};

struct sub_command
{
  const char* name;
  const char* short_name;
  sub_command_fnc call;
  int show_opt;
  int terminating;
  char* description;
  char* arg_example;
};
#define OPT_SHOW_NONE (0)
#define OPT_SHOW_HELP (1)
//#define OPT_SHOW_
struct command_help
{
  const char* name;
  const char* usage;
  LVal commands;
  LVal opts;
  const char* header;
  const char* footer;
};

extern LVal top_helps;
extern LVal top_commands;
extern LVal top_options;
extern struct opts* global_opt;
extern struct opts* local_opt;
extern LVal subcommand_name;
extern int verbose;
extern int quicklisp;

LVal add_help(LVal help,const char* name,const char* usage,LVal commands,LVal opts,const char* header,const char* footer);
LVal add_command(LVal cmd,const char* name,const char* short_name,sub_command_fnc call,int show_opt,int terminating,char* description,char* arg_example);
struct opts* load_opts(const char* path);
int save_opts(const char* path,struct opts* opt);
int set_opt(struct opts** opts,const char* name,char* value,int type);
int unset_opt(struct opts** opts,const char* name);
char* get_opt(const char* name);
void print_opts(struct opts* opt);
char* sexp_opts(struct opts* opt);
void free_opts(struct opts* opt);
#endif
