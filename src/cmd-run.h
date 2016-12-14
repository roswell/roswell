/* -*- tab-width : 2 -*- */
#ifndef __CMD_RUN_H__
#define __CMD_RUN_H__

#include "opt.h"

#define ROS_RUN_REPL "run"
#define DEFAULT_IMPL "sbcl-bin"

typedef char** (*cmd_run_impl)(int argc,char** argv,struct sub_command* cmd);

struct run_impl_t {
  char* name;
  cmd_run_impl impl;
};

extern char** cmd_run_sbcl(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_ccl(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_clisp(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_ecl(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_abcl(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_cmu(int argc,char** argv,struct sub_command* cmd);
extern char** cmd_run_acl(int argc,char** argv,struct sub_command* cmd);
extern LVal register_runtime_options();
int setup(char* target);
#define SETUP_SYSTEM(sys,msg) {\
    cond_printf(0,"%s",msg);   \
    cond_printf(1,"%s\n",sys); \
    int ret=System(sys);       \
    s(sys);                    \
    if(ret) {                  \
      lock_apply("setup",1);   \
      return ret;              \
    }                          \
  }
#endif
