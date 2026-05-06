#include "opt.h"

char** cmd_run_dotcl(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[apphost binary] --eval init.lisp --eval (ros:run '(...)) [repl] */
  int i;
  char* impl_path=impldir(arch,os,impl, version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* program=get_opt("program",0);
  char* dotcl_version=get_opt("version",0);
  LVal ret=0;

  s(arch),s(os);

  ret=conss(cat(home,impl_path,DIRSEP,"runtime",EXE_EXTENTION,NULL),ret);

  if(dotcl_version) {
    ret=conss(q("--eval"),ret);
    ret=conss(q("(progn (format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version)) (dotcl:quit))"),ret);
  }
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")","(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);

  /* dotcl exits after --eval/--load by default; for interactive (no script)
     append the `repl` subcommand to keep the process alive. */
  if(!script) {
    ret=conss(q("repl"),ret);
  }

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  s(impl_path);
  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
