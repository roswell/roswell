#include "opt.h"

char** cmd_run_clisp(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=9; /*[binpath for clisp] -q -q -M param -repl init.lisp
                  [terminating NULL] that total 8 are default. */
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* clisp_version=get_opt("version",0);
  int paramc=0;
  char *bin;
  int issystem=(strcmp("system",version)==0);
  char *asdf3= cat(home,"lisp",SLASH,"asdf3.lisp",NULL);

  if(!file_exist_p(asdf3)) {
    char* cmd=cat(which(argv_orig[0])," install asdf3",NULL);
    char* ret;
    cond_printf(1,"cmd:%s\n",cmd);
    ret=system_(cmd);
    cond_printf(1,"ret:%s\n",ret);
    s(ret);
  }
  s(asdf3);

  if(issystem){
    bin=truename(which("clisp"));
  }else {
    bin=cat(impl_path,SLASH,"bin",SLASH,"clisp",EXE_EXTENTION,NULL);
  }
  s(arch),s(os);
  if(help) {
    offset++;
  }
  if(clisp_version)
    offset+=1;
  if(quicklisp)
    offset+=2;
  if(program||script)
    offset+=2;

  arg=alloc(sizeof(char*)*(offset+argc));
  arg[paramc++]=q("wrapper-dummy");
  arg[paramc++]=bin;
  /* runtime options from here */
  arg[paramc++]=q("-q");
  arg[paramc++]=q("-q");

  if(image) {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(file_exist_p(path)) {
      arg[paramc++]=q("-M");
      arg[paramc++]=path;
    } else {
      cond_printf(1,"core not found:%s\n",path);
      s(path);
    }
  }
  if(help)
    arg[paramc++]=q("--help");
  if(script) {
    arg[paramc++]=q("-on-error");
    arg[paramc++]=q("exit");
  }
  if (!(program || script))
    arg[paramc++]=q("-repl");
  arg[paramc++]=s_cat(s_escape_string(lispdir()),q("init.lisp"),NULL);
  if(quicklisp) {
    arg[paramc++]=q("(ros:quicklisp)");
  }
  if(program || script) {
    char *tmp;
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
