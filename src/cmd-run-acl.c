#include "opt.h"

char** cmd_run_acl(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=8; /*[binpath for alisp] --qq -I param -e init.lisp
                  [terminating NULL] that total 7 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* acl_version=get_opt("version",0);
  int paramc=0;
  char *bin;
  int issystem=(strcmp("system",version)==0);
  if(issystem){
    bin=truename(which("alisp"));
  }else {
    bin=cat(impl_path,SLASH,"bin",SLASH,"alisp",EXE_EXTENTION,NULL);
  }
  s(arch),s(os);
  if(acl_version)
    offset+=3;
  if(script)
    offset+=1;
  if(quicklisp)
    offset+=2;
  if(program||script)
    offset+=2;

  arg=alloc(sizeof(char*)*(offset+argc));
  arg[paramc++]=q("wrapper-dummy");
  arg[paramc++]=bin;
  /* runtime options from here */
  arg[paramc++]=q("-qq");
  if(acl_version) {
    arg[paramc++]=q("-e");
    arg[paramc++]=q("(format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))");
    arg[paramc++]=q("-kill");
  }

  if(image) {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)){
      arg[paramc++]=q("-I");
      arg[paramc++]=path;
    }else {
      cond_printf(1,"core not found:%s\n",path);
      arg[paramc++]=cat(impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL);
      s(path);
    }
  }
  arg[paramc++]=q("-e");
  arg[paramc++]=s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL);

  if(quicklisp) {
    arg[paramc++]=q("-e");
    arg[paramc++]=q("(ros:quicklisp)");
  }
  if(program || script) {
    char *tmp;
    arg[paramc++]=q("-e");
    tmp=cat("(ros:run '(",program?program:"",script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"","))",NULL);
    arg[paramc++]=tmp;
  }

  for(i=1;i<argc;++i) {
    arg[paramc++]=argv[i];
  }

  s(impl_path);

  arg[paramc]=NULL;
  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil"
              ,script?script:"nil");
  return arg;
}
