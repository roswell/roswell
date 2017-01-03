/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_ecl(int argc,char** argv,struct sub_command* cmd) {
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for ecl] -norc -eval init.lisp
    [terminating NULL] that total 5 are default. */
  int i;
  char* impl_path= cat(configdir(),"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  LVal ret=0;

  if(strcmp(os,"darwin")==0 && strcmp("system",version)!=0) {
    char* path=getenv("DYLD_FALLBACK_LIBRARY_PATH");
    path=path?q(path):s_cat(q(getenv("HOME")),q("/lib:/usr/local/lib:/lib:/usr/lib"),NULL);
    path=s_cat(q(impl_path),q("/lib:"),path,NULL);
    setenv("DYLD_FALLBACK_LIBRARY_PATH",path,1);
    s(path);
  }
  ret=conss((strcmp("system",version)==0)?truename(which("ecl")):cat(impl_path,SLASH,"bin",SLASH,"ecl",EXE_EXTENTION,NULL),ret);
  s(arch),s(os),s(impl_path);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);

  /* runtime options from here */
  ret=conss(q("-norc"),ret);
  ret=conss(q("-eval"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),
                  s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);
  if(program || script) {
    ret=conss(q("-eval"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }
  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
