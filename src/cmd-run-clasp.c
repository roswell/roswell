/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_clasp(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for clasp] -r -n --eval init.lisp */ /* don't know very much about image yet.*/
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* program=get_opt("program",0);

  LVal ret=0;

  char *bin=cat(home,impl_path,SLASH,"clasp",EXE_EXTENTION,NULL);
  s(arch),s(os);
  ret=conss(bin,ret);
  s(impl_path);
  if(help)
    ret=conss(q("--help"),ret);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);
  ret=conss(q("--norc"),ret);
  ret=conss(q("--noinit"),ret); /* ? */
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);

  if(program || script) {
    ret=conss(q("--eval"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }
  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);
  return stringlist_array(nreverse(ret));
}
