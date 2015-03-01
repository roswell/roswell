#include <stdio.h>
#include "util.h"
#include "opt.h"

char* ql_path(void);

char** cmd_run_sbcl(int argc,char** argv,struct sub_command* cmd)
{
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=10; /*[binpath for sbcl] --noinform --core param --eval init.lisp
                  --no-sysinit --no-userinit [terminating NULL] that total 9 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* dynamic_space_size=get_opt("dynamic-space-size",0);
  char* dynamic_stack_size=get_opt("dynamic-stack-size",0);
  char* sbcl_version=get_opt("version",0);
  int paramc=0;
  char *bin;
  int issystem=(strcmp("system",version)==0);
  if(issystem){
    bin=truename(which("sbcl"));
  }else {
    bin=cat(impl_path,SLASH,"bin",SLASH,"sbcl",EXE_EXTENTION,NULL);
  }

  s(arch),s(os);
  if(help) {
    offset++;
  }
  if(dynamic_space_size)
    offset+=2;
  if(dynamic_stack_size)
    offset+=2;
  if(sbcl_version)
    offset+=1;
  if(script)
    offset+=1;
  if(quicklisp) {
    char* setup_file=s_cat(ql_path(),q("setup.lisp"),NULL);
    if(file_exist_p(setup_file)) {
      offset+=2;
    }else {
      quicklisp=0;
    }
    s(setup_file);
  }
  if(program||script)
    offset+=2;

  arg=alloc(sizeof(char*)*(offset+argc));
  arg[paramc++]=q("wrapper-dummy");
  arg[paramc++]=bin;
  /* runtime options from here */
  if(image||!issystem)
    arg[paramc++]=q("--core");
  if(!image) {
    if(!issystem)
      arg[paramc++]=cat(impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL);
  }else {
    arg[paramc++]=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
  }
  if(help) {
    arg[paramc++]=q("--help");
  }
  arg[paramc++]=q("--noinform");

  if(dynamic_space_size) {
    arg[paramc++]=q("--dynamic-space-size");
    arg[paramc++]=q(dynamic_space_size);
  }
  if(dynamic_stack_size) {
    arg[paramc++]=q("--dynamic-stack-size");
    arg[paramc++]=q(dynamic_stack_size);
  }
  if(sbcl_version) {
    arg[paramc++]=q("--version");
  }
  /* runtime options end here */
  for(i=1;i<argc;++i) {
    arg[paramc++]=argv[i];
  }
  arg[paramc++]=q("--no-sysinit");
  arg[paramc++]=q("--no-userinit");
  if(script)
    arg[paramc++]=q("--disable-debugger");

  arg[paramc++]=q("--eval");
  arg[paramc++]=s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL);

  if(quicklisp) {
    char *tmp,*tmp2;
    arg[paramc++]=q("--eval");
    tmp=s_cat(q("(progn #-quicklisp(cl:load \""),ql_path(),q("setup.lisp\"))"),NULL);
    arg[paramc++]=tmp;
  }

  if(program || script) {
    char *tmp,*tmp2;
    arg[paramc++]=q("--eval");
    tmp=cat("(ros:run '(",program?program:"",script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"","))",NULL);
    arg[paramc++]=tmp;
  }

  s(impl_path);

  arg[paramc]=NULL;
  if(verbose>0) {
    fprintf(stderr,"\nhelp=%s ",help?"t":"nil");
    fprintf(stderr,"script=%s\n",script?script:"nil");
  }
  s(help),s(dynamic_space_size),s(dynamic_stack_size);
  return arg;
}
