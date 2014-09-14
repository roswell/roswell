#ifndef  __OPT_H__
#define __OPT_H__

typedef int (*sub_command_fnc)(int argc,char **argv);

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
  sub_command_fnc call;
  int show_opt;
  char* path;
};

extern struct opts* global_opt;
extern struct opts* local_opt;
struct opts* load_opts(const char* path);
int set_opt(struct opts** opts,const char* name,char* value,int type);
char* get_opt(const char* name);
#endif
