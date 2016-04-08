/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_clisp(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for clisp] -q -q -M param -repl init.lisp
    [terminating NULL] that total 8 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  int simple=0;
  char *asdf3= get_opt("asdf.version",0);
  LVal ret=0;

  if(!asdf3) {
    char* cmd=cat(which(argv_orig[0])," asdf install",NULL);
    int ret;
    cond_printf(1,"cmd:%s\n",cmd);
    ret=System(cmd);
    cond_printf(1,"ret:%d\n",ret);
  }
  s(arch),s(os);

  ret=conss((strcmp("system",version)==0)?
            truename(which((strcmp(impl,"clisp32")==0)?"clisp32":"clisp")):
            cat(impl_path,SLASH,"bin",SLASH,"clisp",EXE_EXTENTION,NULL),ret);
  if(get_opt("version",0)) {
    ret=conss(q("--version"),ret);
    simple=1;
  }

  for(i=1;i<argc;++i) {
    ret=conss(q(argv[i]),ret);
    if(strcmp(argv[i],"--version")==0)
      simple=1;
  }
  if(help)
    ret=conss(q("--help"),ret);

  if(image) {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)) {
      ret=conss(q("-M"),ret);
      ret=conss(path,ret);
    } else {
      cond_printf(1,"core not found:%s\n",path);
      s(path);
    }
  }

  /* runtime options from here */
  ret=conss(q("-q"),ret);
  ret=conss(q("-q"),ret);

  if(script) {
    ret=conss(q("-on-error"),ret);
    ret=conss(q("exit"),ret);
  }
  ret=conss(q("-repl"),ret);
  if(!simple) {
    ret=conss(s_cat(s_escape_string(lispdir()),q("init.lisp"),NULL),ret);
    if(quicklisp)
      ret=conss(q("(ros:quicklisp)"),ret);

    if(program || script)
      ret=conss(cat("(ros:run '(",program?program:"",
                    script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"",
                    "))",NULL),ret);
  }
  s(impl_path);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
