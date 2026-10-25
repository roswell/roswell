#include "opt.h"

char** cmd_run_clasp(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for clasp] -r -n --eval init.lisp */ /* don't know very much about image yet.*/
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* program=get_opt("program",0);
  char* image=get_opt("image",0);
  char* core=NULL;

  LVal ret=0;

  char *bin=cat(home,impl_path,DIRSEP,"build/boehmprecise/iclasp",EXE_EXTENTION,NULL);
  s(arch),s(os);
  ret=conss(bin,ret);
  if(image) {
    core=cat(basedir(),impl_path,DIRSEP,"dump",DIRSEP,image,".core",NULL);
    if(!file_exist_p(core)) {
      cond_printf(1,"core not found:%s\n",core);
      s(core);
      core=NULL;
    }
  }
  if(core) {
    ret=conss(q("--snapshot"),ret);
    ret=conss(core,ret);
  }else {
    ret=conss(q("--image"),ret);
    ret=conss(cat(home,impl_path,DIRSEP,"build/boehmprecise/lib/images/base.nfasl",NULL),ret);
  }
  s(impl_path);
  if(help)
    ret=conss(q("--help"),ret);
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);
  ret=conss(q("--norc"),ret);
  ret=conss(q("--noinform"),ret);
  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL),ret);

  ret=conss(q("--eval"),ret);
  ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                  script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                  q("))"),NULL),ret);

  for(i=1;i<argc;++i)
    ret=conss(q(argv[i]),ret);
  return stringlist_array(nreverse(ret));
}
