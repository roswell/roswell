/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_sbcl(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for sbcl] --noinform --core param --eval init.lisp
    --no-sysinit --no-userinit [terminating NULL] that total 9 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* dynamic_space_size=get_opt("dynamic-space-size",0);
  char* control_stack_size=get_opt("control-stack-size",0);
  char* enable_debugger=get_opt("enable-debugger",0);

  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  LVal ret=0;

  int issystem=(strcmp("system",version)==0);
  char *bin=issystem?
    strcmp(impl,"sbcl32")==0?truename(which("sbcl32")):truename(which("sbcl")):
    cat(impl_path,SLASH,"bin",SLASH,"sbcl",EXE_EXTENTION,NULL);
  setenv("SBCL_HOME",sbcl_home,1);

  s(arch),s(os),s(sbcl_home);
  ret=conss(bin,ret);

  /* runtime options from here */
  if(image||!issystem)
    ret=conss(q("--core"),ret);
  if(!image) {
    if(!issystem)
      ret=conss(cat(impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL),ret);
  }else {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path))
      ret=conss(path,ret);
    else {
      cond_printf(1,"core not found:%s\n",path);
      ret=conss(cat(impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL),ret);
      s(path);
    }
  }
  s(impl_path);
  if(help)
    ret=conss(q("--help"),ret);
  ret=conss(q("--noinform"),ret);

  if(dynamic_space_size) {
    ret=conss(q("--dynamic-space-size"),ret);
    ret=conss(q(dynamic_space_size),ret);
  }
  if(control_stack_size) {
    ret=conss(q("--control-stack-size"),ret);
    ret=conss(q(control_stack_size),ret);
  }
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);

  for(i=1;i<argc;++i) {
    if(strcmp(argv[i],"--eval")==0||
       strcmp(argv[i],"--load")==0)
      break;
    ret=conss(q(argv[i]),ret);
  }
  /* runtime options end here */
  ret=conss(q("--no-sysinit"),ret);
  ret=conss(q("--no-userinit"),ret);
  if(script && !enable_debugger)
    ret=conss(q("--disable-debugger"),ret);

  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);

  if(quicklisp) {
    ret=conss(q("--eval"),ret);
    ret=conss(q("(ros:quicklisp)"),ret);
  }

  if(program || script) {
    ret=conss(q("--eval"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }

  for(;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
