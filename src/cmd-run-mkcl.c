#include "cmd-run.h"

char** cmd_run_mkcl(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for mkcl] -norc -q -eval init.lisp
    [terminating NULL] that total 6 are default. */
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* lisp_temp_stack_limit=get_opt("lisp-temp-stack-limit",0);
  char* frame_stack_limit=get_opt("frame-stack-limit",0);
  char* binding_stack_limit=get_opt("binding-stack-limit",0);
  char* heap_size_limit=get_opt("heap-size-limit",0);
  LVal ret=0;

  ret=conss((strcmp("system",version)==0)?truename(which("mkcl")):cat(home,impl_path,SLASH,"bin",SLASH,"mkcl",EXE_EXTENTION,NULL),ret);

  s(arch),s(os),s(impl_path);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);

  /* runtime options from here */
  ret=conss(q("-norc"),ret);
  if(lisp_temp_stack_limit)
    ret=conss(q(lisp_temp_stack_limit),conss(q("--lisp-temp-stack-limit"),ret));
  if(frame_stack_limit)
    ret=conss(q(frame_stack_limit),conss(q("--frame-stack-limit"),ret));
  if(binding_stack_limit)
    ret=conss(q(binding_stack_limit),conss(q("--binding-stack-limit"),ret));
  if(heap_size_limit)
    ret=conss(q(heap_size_limit),conss(q("--heap-size-limit"),ret));
  ret=conss(q("-q"),ret);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);

  ret=conss(q("-eval"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),
                  s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  ret=conss(q("-eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);
  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
