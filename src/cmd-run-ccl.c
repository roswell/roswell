#include <stdio.h>
#include "util.h"
#include "opt.h"
char* ql_path(void);

char* ccl_binname(void) {
  char* ret=q("");
  char* _uname_m=uname_m();
  char* _uname=uname();
  if(strcmp(_uname,"linux")==0) {
    ret=s_cat(ret,q("l"),NULL);
  }else if(strcmp(_uname,"windows")==0) {
    ret=s_cat(ret,q("w"),NULL);
  }else if(strcmp(_uname,"darwin")==0) {
    ret=s_cat(ret,q("d"),NULL);
  }
  if(strcmp(_uname_m,"x86-64")==0 ||
     strcmp(_uname_m,"x86")==0) {
    ret=s_cat(ret,q("x86"),NULL);
  }
  ret=s_cat(ret,q("cl"),NULL);
  if(strcmp(_uname_m,"x86-64")==0)
    ret=s_cat(ret,q("64"),NULL);
  return ret;
}

char** cmd_run_ccl(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=8;  /*[binpath for ccl] --no-init --quiet --batch --image-name param --eval init.lisp
                   [terminating NULL] that total 9 are default. */
  int i;
  int paramc=0;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* bin;
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  int issystem=(strcmp("system",version)==0);
  if(issystem){
    bin=truename(which("ccl"));
  }else {
    bin=s_cat(q(impl_path),q(SLASH),ccl_binname(),q(EXE_EXTENTION),NULL);
  }

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
  arg[paramc++]=q("--no-init");
  arg[paramc++]=q("--quiet");
  if(image||!issystem)
    arg[paramc++]=q("--image-name");
  if(!image) {
    if(!issystem) {
      char* binname=ccl_binname();
      arg[paramc++]=cat(impl_path,SLASH,binname,".image",NULL);
      s(binname);
    }
  }else {
    arg[paramc++]=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
  }
  arg[paramc++]=q("--eval");
  arg[paramc++]=s_cat(q("(progn #-ros.init(cl:load \""),lispdir(),q("init.lisp"),q("\"))"),NULL);

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

  for(i=1;i<argc;++i) {
    arg[paramc++]=argv[i];
  }
  arg[paramc]=NULL;
  return arg;
}
