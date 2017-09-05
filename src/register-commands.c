#include "opt.h"

struct proc_opt* register_cmd_run(struct proc_opt* top);
struct proc_opt* register_cmd_install(struct proc_opt* top);
struct proc_opt* register_cmd_internal(struct proc_opt* top_);

#define OPT_SETVAL(sym,init,rexp)               \
  int sym=init;                                 \
  DEF_SUBCMD(opt_##sym) {                       \
    sym=rexp;                                   \
    cond_printf(1,"opt:%s:%d\n",cmd->name,sym); \
    return 1;}

OPT_SETVAL(verbose,  0,(strcmp(cmd->name,"verbose")==0)?1|verbose<<1:verbose>>1)
OPT_SETVAL(testing,  0,1+testing)
OPT_SETVAL(rc,       1,(strcmp(cmd->name,"rc")==0)?1:0)
OPT_SETVAL(quicklisp,1,(strcmp(cmd->name,"quicklisp")==0)?2:0)
OPT_SETVAL(asdf     ,1,(strcmp(cmd->name,"asdf")==0)?1:0)
OPT_SETVAL(module   ,0,1)

DEF_SUBCMD(opt_program0) {
  if(cmd->name) {
    char* current=get_opt("program",0);
    current=cat(current?current:"","(:",cmd->name,")",NULL);
    set_opt(&local_opt,"program",current);
  }
  return 1;
}

DEF_SUBCMD(opt_take1) {
  int argc=length(arg_);
  const char* arg=cmd->name;
  if(arg && argc>1) {
    char* current=get_opt(arg,0);
    cond_printf(1,"take1:%s:%s,%s\n",arg,firsts(nthcdr(1,arg_)),current);
    if(current)
      set_opt(&local_opt,cat("*",arg,NULL),current);
    set_opt(&local_opt,arg,firsts(nthcdr(1,arg_)));
    return 2;
  }
  return -1;
}

#define OPT_APPEND(sym)                                    \
  DEF_SUBCMD(opt_##sym) {                                  \
    int argc=length(arg_);                                 \
                                                           \
    if(cmd->name && argc>1) {                              \
      char* current=get_opt(#sym,0);                       \
      current=s_cat(current?q(current):q(""),              \
                    q("(:"),q(cmd->name),q(" \""),         \
                    escape_string(firsts(nthcdr(1,arg_))), \
                    q("\")"),NULL);                        \
      set_opt(&local_opt,#sym,current);                    \
      return 2;                                            \
    }                                                      \
    return -1;                                             \
  }

OPT_APPEND(program)
OPT_APPEND(restart)
OPT_APPEND(final)

struct proc_opt* register_runtime_options(struct proc_opt* cmd) {
  LVal opt=cmd->option;
  opt=add_command(opt,"version" ,NULL,opt_version,1,1);
  opt=add_command(opt,"wrap","-w",opt_take1,1,0);
  opt=add_command(opt,"image","-m",opt_take1,1,0);
  opt=add_command(opt,"module","-M",opt_module,1,0);
  opt=add_command(opt,"lisp","-L",opt_take1,1,0);
  opt=add_command(opt,PACKAGE_NAME"env","-N",opt_take1,1,0);

  /*opt=add_command(opt,"file","-f",opt_program,1,0,"include lisp FILE while building","FILE");*/
  opt=add_command(opt,"load","-l",opt_program,1,0);
  opt=add_command(opt,"source-registry","-S",opt_program,1,0);
  opt=add_command(opt,"system","-s",opt_program,1,0);
  opt=add_command(opt,"load-system",NULL,opt_program,1,0);
  opt=add_command(opt,"package","-p",opt_program,1,0);
  opt=add_command(opt,"system-package","-sp",opt_program,1,0);
  opt=add_command(opt,"eval","-e",opt_program,1,0);
  opt=add_command(opt,"require",NULL,opt_program,1,0);
  opt=add_command(opt,"quit","-q",opt_program0,1,0);

  opt=add_command(opt,"restart","-r",opt_restart,1,0);
  opt=add_command(opt,"entry","-E",opt_restart,1,0);
  opt=add_command(opt,"init","-i",opt_restart,1,0);
  opt=add_command(opt,"print","-ip",opt_restart,1,0);
  opt=add_command(opt,"write","-iw",opt_restart,1,0);

  opt=add_command(opt,"final","-F",opt_final,1,0);

  opt=add_command(opt,"rc","-R",opt_rc,1,0);
  opt=add_command(opt,"no-rc","+R",opt_rc,1,0);
  opt=add_command(opt,"quicklisp","-Q",opt_quicklisp,1,0);
  opt=add_command(opt,"no-quicklisp","+Q",opt_quicklisp,1,0);
  opt=add_command(opt,"asdf","-A",opt_asdf,1,0);
  opt=add_command(opt,"no-asdf","+A",opt_asdf,1,0);
  opt=add_command(opt,"verbose","-v",opt_verbose,1,0);
  opt=add_command(opt,"quiet",NULL,opt_verbose,1,0);
  opt=add_command(opt,"test",NULL,opt_testing,1,0);
  opt=add_command(opt,"stdin",NULL,opt_program,0,0);
  cmd->option=nreverse(opt);
  return cmd;
}

struct proc_opt* register_runtime_commands(struct proc_opt* top_) {
  top_=register_cmd_install(top_);
  top_=register_cmd_internal(top_);
  top_=register_cmd_run(top_);
  top_->command=nreverse(top_->command);
  return top_;
}

void register_top(struct proc_opt* top_) {
  dispatch_init(top_,"top");
  top_=register_runtime_options(top_);
  top_=register_runtime_commands(top_);
  top_->alias=cons((void*)stringlist("-V","version",NULL),top_->alias);
  top_->alias=cons((void*)stringlist("-h","help",NULL),top_->alias);
  top_->alias=cons((void*)stringlist("-?","help",NULL),top_->alias);
  /*top_->alias=cons((void*)stringlist("build","dump","executable",NULL),top_->alias);*/
  top_->top=(LVal)top_;
}
