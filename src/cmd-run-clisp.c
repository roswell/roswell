#include "opt.h"

char** cmd_run_clisp(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for clisp] -q -q -M param -N locale-path [-L lang] -repl init.lisp
    [terminating NULL] that total 8 are default. */
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* lang=get_opt("lang",0);
  int simple=0;
  LVal ret=0;

  s(arch),s(os);

  ret=conss((strcmp("system",version)==0)?
            truename(which((strcmp(impl,"clisp32")==0)?"clisp32":"clisp")):
            cat(home,impl_path,SLASH,"bin",SLASH,"clisp",EXE_EXTENTION,NULL),ret);
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

  {
    char* path=cat(basedir(),impl_path,SLASH,"lib",SLASH,NULL);
    LVal d=directory(path);
    if(d) {
      path=s_cat(path,q(firsts(d)),NULL);
      cond_printf(1,"lisplibdir=%s\n",path);
      ret=conss(q("-B"),ret);
      ret=conss(path,ret);
    }
  }
  {
    char* path=cat(basedir(),impl_path,SLASH,"share",SLASH,"locale",SLASH,NULL);
    cond_printf(1,"localedir=%s\n",path);
    ret=conss(q("-N"),ret);
    ret=conss(path,ret);
  }
  if(lang) {
    cond_printf(1,"lang=%s\n",lang);
    ret=conss(q("-L"),ret);
    ret=conss(q(lang),ret);
  }

  if(image) {
    char *path=cat(basedir(),impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)) {
      ret=conss(q("-M"),ret);
      ret=conss(path,ret);
    }else {
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
  ret=conss(q("-p"),ret);
  ret=conss(q("CL-USER"),ret);
  if(!simple) {
    ret=conss(s_cat(s_escape_string(lispdir()),q("init.lisp"),NULL),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }
  s(impl_path);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
