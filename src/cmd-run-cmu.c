/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_cmu(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;

  /*[binpath for lisp] -quiet -core param -eval init.lisp
    -noinit -nositeinit [terminating NULL] that total 9 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* dynamic_space_size=get_opt("dynamic-space-size",0);
  char* control_stack_size=get_opt("control-stack-size",0);
  char* cmu_version=get_opt("version",0);
  char *bin;
  LVal ret=0;

  if(strcmp(impl,"cmu"))
    impl="cmucl";

  if(strcmp("system",version)==0) {
    bin=which("lisp");
    if(strcmp(bin,"")==0)
      s(bin),bin=which("cmucl");
    bin=truename(bin);
  }else
    bin=cat(impl_path,SLASH,"bin",SLASH,"lisp",EXE_EXTENTION,NULL);

  ret=conss(bin,ret);
  /* runtime options from here */

  ret=conss(q("-quiet"),ret);
  if(image) {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)) {
      ret=conss(q("-core"),ret);
      ret=conss(q(path),ret);
    }else
      cond_printf(1,"core not found:%s\n",path);
    s(path);
  }
  if(help)
    ret=conss(q("-help"),ret);

  ret=conss(q("-noinit"),ret);
  ret=conss(q("-nositeinit"),ret);

  if(dynamic_space_size) {
    ret=conss(q("-dynamic-space-size"),ret);
    ret=conss(q(dynamic_space_size),ret);
  }
  if(control_stack_size) {
    ret=conss(q("-control-stack-size"),ret);
    ret=conss(q(control_stack_size),ret);
  }
  if(cmu_version) {
    ret=conss(q("-eval"),ret);
    ret=conss(q("(progn (format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))(extensions:quit))"),ret);
  }
  ret=conss(q("-eval"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);

  if(quicklisp) {
    ret=conss(q("-eval"),ret);
    ret=conss(q("(ros:quicklisp)"),ret);
  }
  if(program || script) {
    ret=conss(q("-eval"),ret);
    ret=conss(cat("(ros:run '(",program?program:"",
                  script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"",
                  "))",NULL),ret);
  }

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  s(impl_path);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
