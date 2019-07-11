#include "cmd-run.h"

char** cmd_run_sbcl(int argc,char** argv,struct sub_command* cmd) {
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname_s();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  /*[binpath for sbcl] --noinform --core param --eval init.lisp
    --no-sysinit --no-userinit [terminating NULL] that total 9 are default. */
  int i;
  char* impl_path=impldir(arch,os,impl,version);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* dynamic_space_size=get_opt("dynamic-space-size",0);
  char* control_stack_size=get_opt("control-stack-size",0);
  char* withoutroswell=get_opt("without-roswell",0);
  char* enable_debugger=get_opt("enable-debugger",0);

  LVal ret=0;

  int issystem=(strcmp("system",version)==0);
  char *bin=issystem?
    strcmp(impl,"sbcl32")==0?truename(which("sbcl32")):truename(which("sbcl")):
    cat(home,impl_path,SLASH,"bin",SLASH,"sbcl",EXE_EXTENTION,NULL);

  s(arch),s(os);

  if (!issystem) {
    char* sbcl_home=cat(home,impl_path,SLASH,"lib",SLASH,"sbcl",NULL);
    setenv("SBCL_HOME",sbcl_home,1);
    s(sbcl_home);
  }

  ret=conss(bin,ret);

  /* runtime options from here */
  if(image) {
    char* core=NULL;
    char* ld=lispdir();
    char* base=basedir();
    char* bindir=cat(base,"bin"SLASH,NULL);
    char* bindir2=cat(home,"bin"SLASH,NULL);
    char* script2=q(script?script+1:"");
    int pos= position_char("\"",script2);
    core=cat(base,impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    if(pos!=-1)
      script2[pos]='\0';
    if(script &&
       (strncmp(ld,script2,strlen(ld)) ==0 ||
        strncmp(bindir,script2,strlen(bindir)) ==0 ||
        strncmp(bindir2,script2,strlen(bindir2)) ==0) &&
       (!file_exist_p(core) ||
        (file_newer_p(script2,core) && !file_newer_p(core,script2))) &&
       strcmp(impl,DEFAULT_IMPL)==0) {
      char* env = get_opt(PACKAGE_NAME"env",1);
      if(!env) env = "-";
      cond_printf(1,"\nbuildcore:%s\ncause newer script:%s\nenv:%s\n",core,script2,env);
      setup(image,env,impl);
    }
    s(ld),s(script2),s(bindir),s(bindir2);
    if(file_exist_p(core)) {
      ret=conss(core,conss(q("--core"),ret));
    }else
      cond_printf(1,"core not found:%s\n",core);
  }else if(!issystem)
    ret=conss(cat(home,impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL),
              conss(q("--core"),ret));
  if(help)
    ret=conss(q("--help"),ret);
  if(!withoutroswell)
    ret=conss(q("--noinform"),ret);

  if(dynamic_space_size)
    ret=conss(q(dynamic_space_size),conss(q("--dynamic-space-size"),ret));
  if(control_stack_size)
    ret=conss(q(control_stack_size),conss(q("--control-stack-size"),ret));
  if(get_opt("version",0))
    ret=conss(q("--version"),ret);

  for(i=1;i<argc;++i) {
    if(strcmp(argv[i],"--eval")==0||
       strcmp(argv[i],"--load")==0)
      break;
    ret=conss(q(argv[i]),ret);
  }
  /* runtime options end here */
  if(!withoutroswell) {
    ret=conss(q("--no-sysinit"),ret);
    ret=conss(q("--no-userinit"),ret);
    if(script && !enable_debugger)
      ret=conss(q("--disable-debugger"),ret);
    char* initlisp=cat(home,impl_path,SLASH,"fasl",SLASH,"init.lisp",NULL);
    char* asdf=get_opt("asdf.version",0);
    if(asdf)
      initlisp=s_cat(initlisp,q("_"),substitute_char('_','.',q(asdf)),NULL);
    if(!file_exist_p(initlisp)) {
      s(initlisp);
      initlisp=s_cat2(s_escape_string(lispdir()),q("init.lisp"));
    }
    cond_printf(1,"init.lisp=%s\n",initlisp);
    ret=conss(q("--eval"),ret);
    ret=conss(s_cat(q("(progn #-ros.init(cl:load \""),initlisp,q("\"))"),NULL),ret);
    s(impl_path);
    ret=conss(q("--eval"),ret);
    ret=conss(s_cat(q("(ros:run '("),q(program?program:""),
                    script?cat("(:script ",script,")(:quit ())",NULL):q(""),
                    q("))"),NULL),ret);
  }
  for(;i<argc;++i)
    ret=conss(q(argv[i]),ret);

  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil",script?script:"nil");
  return stringlist_array(nreverse(ret));
}
