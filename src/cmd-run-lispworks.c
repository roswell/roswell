#include "opt.h"

char** cmd_run_lispworks(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();

  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* lw_version=get_opt("version",0);

  LVal ret=0;
  ret=conss(cat(home,"impls",SLASH,arch,SLASH,os,SLASH,"LispWorks",SLASH,"lw-console",NULL), ret);

  if(lw_version) {
    ret=conss(q("-eval"),ret);
    ret=conss(q("(progn (format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))(lw:quit))"),ret);
  }

  ret=conss(q("-eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  if(program || script) {
    ret=conss(q("-eval"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")","(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }

  int i;
  for(;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  ret=nreverse(ret);
  return stringlist_array(ret);
}
