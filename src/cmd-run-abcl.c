/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_abcl(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for abcl] --noinform --noinit --nosystem --eval init.lisp
    [terminating NULL] that total 7 are default. */
  int i;
  char* impl_path=impldir(arch,os,impl, version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* abcl_version=get_opt("version",0);
  LVal ret=0;

  s(arch),s(os);

  ret=conss((strcmp("system",version)==0)?truename(which("abcl")):cat(home,impl_path,SLASH,"abcl",EXE_EXTENTION,NULL),ret);
  /* runtime options from here */
  ret=conss(q("--noinform"),ret);
  ret=conss(q("--noinit"),ret);
  ret=conss(q("--nosystem"),ret);

  if(abcl_version) {
    ret=conss(q("--eval"),ret);
    ret=conss(q("(progn (format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))(extensions:quit))"),ret);
  }
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")","(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  s(impl_path);
  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
