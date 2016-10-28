/* -*- tab-width : 2 -*- */
#include "opt.h"

char* ccl_binname(char* bit) {
  char* ret=q("");
  char* _uname_m=uname_m();
  char* _uname=uname();
  if(strcmp(_uname,"linux")==0) {
    if(strcmp(_uname_m,"armhf")!=0)
      ret=s_cat(ret,q("l"),NULL);
  }else if(strcmp(_uname,"windows")==0) {
    ret=s_cat(ret,q("w"),NULL);
  }else if(strcmp(_uname,"darwin")==0) {
    ret=s_cat(ret,q("d"),NULL);
  }
  if(strcmp(_uname_m,"x86-64")==0 ||
     strcmp(_uname_m,"x86")==0) {
    ret=s_cat(ret,q("x86"),NULL);
  }else if(strcmp(_uname_m,"armhf")==0)
    ret=s_cat(ret,q("arm"),NULL);
  ret=s_cat(ret,q("cl"),NULL);
  if((strcmp(_uname_m,"x86-64")==0 &&
      strcmp(bit,"32")!=0) ||
     (strcmp(_uname_m,"x86")==0 &&
      strcmp(bit,"64")==0))
     ret=s_cat(ret,q("64"),NULL);
  return ret;
}

char** cmd_run_ccl(int argc,char** argv,struct sub_command* cmd) {
  char* binname=get_opt("ccl.bit",0);
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for ccl] --no-init --quiet --batch --image-name param --eval init.lisp
    [terminating NULL] that total 9 are default. */
  int i;
  char* ccl_version=get_opt("version",0);
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  int issystem=(strcmp("system",version)==0);
  LVal ret=0;
  binname = ccl_binname(binname?binname:"");

  unsetenv("CCL_DEFAULT_DIRECTORY");

  ret=conss(issystem?truename(which(strcmp(impl,"ccl32")==0?"ccl32":"ccl")):
            cat(impl_path,SLASH,binname,EXE_EXTENTION,NULL),ret);
  if(ccl_version)
    ret=conss(q("--version"),ret);
  ret=conss(q("--no-init"),ret);
  ret=conss(q("--quiet"),ret);
  if(image||!issystem)
    ret=conss(q("--image-name"),ret);
  if(!image) {
    if(!issystem)
      ret=conss(cat(impl_path,SLASH,binname,".image",NULL),ret);
  }else 
    ret=conss(cat(impl_path,SLASH,"dump",SLASH,image,".",binname,NULL),ret);

  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),lispdir(),q("init.lisp"),q("\"))"),NULL),ret);

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

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  s(impl_path),s(binname);

  return stringlist_array(nreverse(ret));
}
