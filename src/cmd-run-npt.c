#include "cmd-run.h"

char** cmd_run_npt(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for npt] --noinit --eval init.lisp
    [terminating NULL] that total 5 are default. */
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  LVal ret=0;
  ret=conss((strcmp("system",version)==0)?truename(which("npt")):cat(home,impl_path,SLASH,"bin",SLASH,"npt",EXE_EXTENTION,NULL),ret)
;
  s(arch),s(os),s(impl_path);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);
#if 0
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),
                  s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);
#endif
  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
