#include "opt.h"

char** cmd_run_abcl(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=8; /*[binpath for abcl] --noinform --noinit --nosystem --eval init.lisp
                   [terminating NULL] that total 7 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* abcl_version=get_opt("version",0);
  int paramc=0;
  char *bin;
  int issystem=(strcmp("system",version)==0);
  if(issystem) {
    bin=truename(which("abcl"));
  }else {
    bin=cat(impl_path,SLASH,"bin",SLASH,"abcl",EXE_EXTENTION,NULL);
  }
  s(arch),s(os);
  if(abcl_version)
    offset+=1;
  if(quicklisp)
    offset+=2;
  if(program||script)
    offset+=2;
  arg=alloc(sizeof(char*)*(offset+argc));
  arg[paramc++]=q("wrapper-dummy");
  arg[paramc++]=bin;
  /* runtime options from here */
  arg[paramc++]=q("--noinform");
  arg[paramc++]=q("--noinit");
  arg[paramc++]=q("--nosystem");
  arg[paramc++]=q("--eval");
  arg[paramc++]=s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL);
  if(quicklisp) {
    arg[paramc++]=q("--eval");
    arg[paramc++]=q("(ros:quicklisp)");
  }
  if(program || script) {
    char *tmp;
    arg[paramc++]=q("--eval");
    tmp=cat("(ros:run '(",program?program:"",script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"","))",NULL);
    arg[paramc++]=tmp;
  }
  s(impl_path);
  arg[paramc]=NULL;
  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil"
              ,script?script:"nil");
  return arg;
}
