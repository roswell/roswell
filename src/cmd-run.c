#include "opt.h"
#include "cmd-run.h"
#include "cmd-install.h"
DEF_SUBCMD(cmd_script_frontend);

struct run_impl_t impls_to_run[]={
  {"sbcl",&cmd_run_sbcl},
  {"sbcl32",&cmd_run_sbcl},
  {"sbcl-bin",&cmd_run_sbcl},
  {"sbcl-head",&cmd_run_sbcl},
  {"ccl-bin",&cmd_run_ccl},
  {"ccl32",&cmd_run_ccl},
  {"clasp",&cmd_run_clasp},
  {"clasp-bin",&cmd_run_clasp},
  {"clisp-head",&cmd_run_clisp},
  {"clisp",&cmd_run_clisp},
  {"clisp32",&cmd_run_clisp},
  {"ecl",&cmd_run_ecl},
  {"abcl",&cmd_run_abcl},
  {"abcl-bin",&cmd_run_abcl},
  {"cmu-bin",&cmd_run_cmu},
  {"cmucl",&cmd_run_cmu},
  {"acl",&cmd_run_acl},
  {"alisp",&cmd_run_acl},
  {"allegro",&cmd_run_acl},
  {"lispworks",&cmd_run_lispworks},
  {"mkcl",&cmd_run_mkcl},
  {"npt",&cmd_run_npt},
};

struct proc_opt run;

DEF_SUBCMD(cmd_run) {
  int argc=length(arg_);
  char* current=get_opt("program",0);
  cond_printf(1,"cmd_%s:argc=%d argv[0]=%s\n",cmd->name,argc,firsts(arg_));
  if(argc==1 && !current)
    return dispatch(stringlist((char*)cmd->name,"--",NULL),&top);
  else {
    for(arg_=nnthcdr(1,arg_);arg_;arg_=dispatch(arg_,&run));
    dispatch(stringlist("--",NULL),&run);
    cond_printf(1,"cmd_%s ends here\n",cmd->name);
    return 0;
  }
}

int setup(char* target,char* env,char* impl) {
  if(lock_apply("setup",2))
    return 0; /* lock file exists */
  char* v=verbose==1?"-v ":(verbose==2?"-v -v ":"");
  lock_apply("setup",0);
  cond_printf(1,"verbose-option:'%s'\n",v);
  char* version=get_opt(DEFAULT_IMPL".version",0);
  char* path=s_cat(configdir(),q("config"),NULL);
  if(!version) {
    SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install "DEFAULT_IMPL,NULL),"Installing "DEFAULT_IMPL"...\n");
  }else if(strcmp(version,"system")==0) {
    set_defaultlisp(DEFAULT_IMPL,"system");
  }
  global_opt=load_opts(path),s(path);
  version=get_opt(DEFAULT_IMPL".version",0);
  if(strcmp(env,"-")!=0) {
    char *cmd =cat(argv_orig[0]," init env ",env,NULL);
    System(cmd);
    s(cmd);
  }
  SETUP_SYSTEM(cat(argv_orig[0]," -N ",env," -L "DEFAULT_IMPL"/",version," ",v,"setup ",target,NULL),"Making core for Roswell...\n");
  lock_apply("setup",1);
  return 1;
}

char* determin_impl(char* impl) {
  char* version=NULL;
  int pos;
  cond_printf(1,"determin_impl:%s\n",impl);
  if(impl && (pos=position_char("/",impl))!=-1) {
    version=subseq(impl,pos+1,0);
    impl=subseq(impl,0,pos);
  }else {
    if(!impl)
      impl=get_opt("default.lisp",1);
    if(impl) {
      char* opt=s_cat(q(impl),q("."),q("version"),NULL);
      version=get_opt(opt,1);
      if(version)
        version=q(version);
      s(opt);
    }
    if(!impl)
      impl=DEFAULT_IMPL;
    impl=q(impl);
  }
  if(!version&&strcmp(impl,DEFAULT_IMPL)!=0) {
    cond_printf(1,"once!%s,%s\n",impl,version);
    s(version);
    version=q("system");
  }
  if(!(impl && version)) {
    s(impl);
    impl=q(DEFAULT_IMPL);
    setup(PACKAGE,"-",impl);
    version=get_opt(DEFAULT_IMPL".version",0);
    if(version)
      version=q(version);
  }
  return s_cat(impl,q("/"),version,NULL);
}

void set_env_opt(char* path) {
  FILE* fp;
  char buf[1024];
  cond_printf(1,"set_env_opt:%s\n",path);
  if((fp=fopen(path,"r"))==NULL)
    return;
  while(fgets(buf,1024,fp) !=NULL) {
    int i,mode,last;
    char *name,*value;
    for(i=0,mode=0,last=0;i<1024&&buf[i]!='\0';++i) {
      if(buf[i]=='\t'||buf[i]=='\n') {
        switch (mode++) {
        case 0:
          name=subseq(buf,last,i);
          break;
        case 2:
          value=subseq(buf,last,i);
          if(strcmp("quicklisp",name)==0) {
            set_opt(&local_opt,"quicklisp",s_escape_string(cat(configdir(),"env"DIRSEP,value,DIRSEP,"lisp",DIRSEP,"quicklisp",DIRSEP,NULL)));
          }else if(strcmp("default.lisp",name)==0) {
            set_opt(&local_opt,q("lisp"),q(value));
          }else if(strcmp("dists",name)==0) {
          }else
            set_opt(&local_opt,name,q(value));
          s(name),s(value);
          break;
        }
        last=i+1;
      }
    }
  }
  fclose (fp);
}

void star_set_opt(void) {
  char* config=configdir();
  char*lisp=NULL;
  char*image=get_opt("image",0);
  if(!get_opt(PACKAGE_NAME"env",0))
    set_env_opt("."PACKAGE_NAME"env");
  /*If 'roswellenv' not set the below would endup fail to open cause it will be taken as a directory.*/
  set_env_opt(s_escape_string(cat(configdir(),"env",DIRSEP,get_opt(PACKAGE_NAME"env",1),DIRSEP,"config",NULL)));
  lisp= get_opt("lisp",1); /*-L in fourth line of ros script or default.lisp in env file.*/
  if(!lisp) lisp= get_opt("*lisp",0); /*-L via command line.*/
  if(!lisp) lisp= get_opt("default.lisp",0);/*default.lisp via default config file.*/
  set_opt(&local_opt,"impl",determin_impl(lisp));
  if(!get_opt("quicklisp",0))
    set_opt(&local_opt,"quicklisp",s_escape_string(cat(config,"lisp",DIRSEP,"quicklisp",DIRSEP,NULL)));
  set_opt(&local_opt,"argv0",argv_orig[0]);
  set_opt(&local_opt,"wargv0",which(argv_orig[0]));
  set_opt(&local_opt,"homedir",q(config));
  set_opt(&local_opt,"verbose",qsprintf(10,"%d",verbose));
  set_opt(&local_opt,"lispdir",q(lispdir()));
  set_opt(&local_opt,"patchdir",append_trail_slash(q(patchdir())));
  if(get_opt("asdf.version",0))
    set_opt(&local_opt,"asdf",get_opt("asdf.version",0));
  set_opt(&local_opt,"uname",uname_s());
  set_opt(&local_opt,"uname-m",uname_m());
  s(config);
}

void star_rc(void) {
  char* prg=get_opt("program",0);
  char* init=s_cat(configdir(),q("init.lisp"),NULL);
  char* etc=ROSRC;
  prg=s_cat(q((asdf==2||(asdf==1&&get_opt("asdf.version",0)))?"(:eval\"(ros:asdf)\")":""),
            q(quicklisp?"(:eval\"(ros:quicklisp)\")":""),
            rc&&file_exist_p(etc)?cat("(:load\"",etc,"\")",NULL):q(""),
            rc&&file_exist_p(init)?cat("(:load\"",init,"\")",NULL):q(""),
            prg?prg:q(""),NULL);
  s(init);
  set_opt(&local_opt,"program",prg);
}

char** star_wrap(char** arg) {
  /*tbd*/
  char* wrap=get_opt("wrap",1);
  return arg;
}

char** determin_args(int argc,char **argv) {
  struct sub_command cmd;
  char** arg=NULL;
  char *_= get_opt("impl",0);
  int i=position_char("/",_);
  cmd.name=subseq(_,0,i);
  cmd.short_name=subseq(_,i+1,0);
  for(i=0;i<sizeof(impls_to_run)/sizeof(struct run_impl_t);++i)
    if(strcmp(impls_to_run[i].name,cmd.name)==0) {
      arg=impls_to_run[i].impl(argc,argv,&cmd);
      break;
    }
  s((char*)cmd.name),s((char*)cmd.short_name);
  return arg;
}

DEF_SUBCMD(cmd_run_star) {
  int argc=length(arg_);
  char** argv=stringlist_array(arg_);
  cond_printf(1,"cmd_run_star:%d:argv[0],%s\n",argc,argv[0]);
  star_set_opt();
  star_rc();

  char** arg=determin_args(argc,argv);
  char* opts=s_cat(q("("),sexp_opts(local_opt),sexp_opts(global_opt),q(")"),NULL);
  int exist=0;
  if(arg && (exist=file_exist_p(arg[0]))) {
    int i;
    arg=star_wrap(arg);
    setenv("ROS_OPTS",opts,1);
    if(verbose&1 ||testing) {
      cond_printf(0,"args ");
      for(i=0;arg[i]!=NULL;++i)
        fprintf(stderr,"%s ",arg[i]);
      cond_printf(0,"\nROS_OPTS %s\n",getenv("ROS_OPTS"));
    }
    testing?exit(EXIT_SUCCESS):exec_arg(arg);
  }else if(!arg) {
    LVal ret=0;
    int i;
    setenv("ROS_OPTS",opts,1);
    ret=conss(q(argv_orig[0]),ret);
    ret=conss(q("-L"),ret);
    ret=conss(q(DEFAULT_IMPL),ret);
    ret=conss(s_cat2(q(lispdir()),q("run.ros")),ret);
    ret=conss(q(get_opt("impl",0)),ret);
    ret=conss(q(get_opt("script",0)?get_opt("script",0):""),ret);
    ret=conss(q(get_opt("verbose",0)),ret);
    for(i=0;i<argc;++i)
      ret=conss(q(argv[i]),ret);
    exec_arg(stringlist_array(nreverse(ret)));
  }
  s(opts);
  if(arg)
    exist?
      cond_printf(0,"%s is not executable. Missing 32bit glibc?\n",arg[0]):
      cond_printf(0,"%s does not exist.stop.\n",get_opt("impl",0));
  return 1;
}

struct proc_opt* register_cmd_run(struct proc_opt* top) {
  /*options*/
  dispatch_init(&run,"run");
  register_runtime_options(&run);
  run.option=add_command(run.option,"",NULL,cmd_run_star,OPT_SHOW_NONE,1);
  run.option=nreverse(run.option);

  /*commands*/
  top->option =add_command(top->option,""            ,NULL,cmd_script_frontend,OPT_SHOW_NONE,1);
  top->command=add_command(top->command,ROS_RUN_REPL ,NULL,cmd_run,OPT_SHOW_HELP,1);
  top->command=add_command(top->command,"*"          ,NULL,cmd_script_frontend,OPT_SHOW_NONE,1);
  return top;
}
