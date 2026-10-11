#include "opt.h"

char** cmd_run_clamiga(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* program=get_opt("program",0);
  char* heap=get_opt("dynamic-space-size",1);
  int batch=script||get_opt("version",0)||(program&&strstr(program,"(:quit)"));
  LVal ret=0;

  ret=conss((strcmp("system",version)==0)?truename(which("clamiga")):cat(home,impl_path,DIRSEP,"bin",DIRSEP,"clamiga",EXE_EXTENTION,NULL),ret);
  s(arch),s(os),s(impl_path);

  ret=conss(q("--heap"),ret);
  ret=conss(heap?cat(heap,"M",NULL):q("512M"),ret);
  ret=conss(q("--no-userinit"),ret);
  if(batch)
    ret=conss(q("--non-interactive"),ret);
  if(get_opt("version",0)) {
    ret=conss(q("--eval"),ret);
    ret=conss(q("(progn(format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))(quit))"),ret);
  }
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())"),
                  q(batch?"(setq *debugger-hook*(lambda(c h)(declare(ignore h))(format *error-output* \"~A~%\" c)(ext:backtrace)(quit 1)))":""),
                  q("#-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
