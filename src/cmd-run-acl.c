/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_acl(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* lisp=strcmp(impl,"mlisp")==0?"mlisp":"alisp";
  char* version=(char*)cmd->short_name;

  /*[binpath for alisp] --qq -I param -e init.lisp
    [terminating NULL] that total 7 are default. */
  int i;
  char* impl_path=impldir(arch,os,"allegro",version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* acl_version=get_opt("version",0);
  LVal ret=0;
  s(arch),s(os);

  ret=conss((strcmp("system",version)==0)?
            truename(which(lisp)):
            cat(home,impl_path,SLASH,lisp,EXE_EXTENTION,NULL),ret);
  /* runtime options from here */
  ret=conss(q("-qq"),ret);
  if(acl_version) {
    ret=conss(q("-e"),ret);
    ret=conss(q("(format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))"),ret);
    ret=conss(q("-kill"),ret);
  }

  if(image) {
    char *path=cat(basedir(),impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)) {
      ret=conss(q("-I"),ret);
      ret=conss(path,ret);
    }else
      cond_printf(1,"core not found:%s\n",path);
    s(path);
  }
  ret=conss(q("-e"),ret);
  ret=conss(s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),
                  s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);

  if(program || script) {
    ret=conss(q("-e"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  s(impl_path);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
