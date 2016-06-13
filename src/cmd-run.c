/* -*- tab-width : 2 -*- */
#include "opt.h"
#include "cmd-run.h"
DEF_SUBCMD(cmd_run_star);

struct run_impl_t impls_to_run[]={
  {"sbcl",&cmd_run_sbcl},
  {"sbcl32",&cmd_run_sbcl},
  {"sbcl-bin",&cmd_run_sbcl},
  {"ccl-bin",&cmd_run_ccl},
  {"ccl32",&cmd_run_ccl},
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
};

struct proc_opt run;

DEF_SUBCMD(cmd_run) {
  char** argv=firstp(arg_);
  int argc=(int)rest(arg_);
  dealloc((void*)arg_);

  char* current=get_opt("program",0);
  cond_printf(1,"cmd_%s:argc=%d argv[0]=%s\n",cmd->name,argc,argv[0]);
  if(argc==1 && !current) {
    char* tmp[]={(char*)cmd->name,"--"};
    return proc_opt(2,tmp,&top);
  }else {
    int i;
    for(i=1;i<argc;i+=proc_opt(argc-i,&argv[i],&run));
    if(strcmp((char*)cmd->name,ROS_RUN_REPL)!=0) {
      char* tmp[]={"--"};
      proc_opt(1,tmp,&run);
    }else {
      char* tmp[]={"--",ROS_RUN_REPL};
      proc_opt(1,tmp,&run);
    }
    cond_printf(1,"cmd_%s ends here %d\n",cmd->name,i);
    return i;
  }
}

DEF_SUBCMD(cmd_script) {
  char** argv=firstp(arg_);
  int argc=(int)rest(arg_);
  dealloc((void*)arg_);

  char* current=get_opt("program",0);
  cond_printf(1,"script_%s:argc=%d argv[0]=%s\n",cmd->name,argc,argv[0]);
  cond_printf(1,"current=%s\n",current);
  if(argc==1 && !current &&
     strcmp(argv[0],"--")==0) {
    char* tmp[]={"help","--"};
    return proc_opt(2,tmp,&top);
  }else {
    char* result=q("");
    char* tmp[]={"script"};
    int i=strcmp(argv[0],"--")==0?1:0;
    for (;i<argc;++i) {
      char* val=escape_string(argv[i]);
      result=cat(result,"\"",val,"\"",NULL);
      s(val);
    }
    set_opt(&local_opt,"script",result);
    s(result);
    cmd_run_star(cons(tmp,1),cmd);
  }
  return 0;
}

static int script_frontend_sentinel=0;

DEF_SUBCMD(cmd_script_frontend) {
  char** argv=firstp(arg_);
  int argc=(int)rest(arg_);
  dealloc((void*)arg_);

  FILE* in;
  char buf[800];
  int i=0,j,c;
  int argc_;
  char** argv_;
  char** argv_gen;
  struct opts* opt;
  if(script_frontend_sentinel)
    return cmd_script(cons(argv,argc),cmd);
  script_frontend_sentinel=1;
  if(strcmp(argv[0],"--")==0)
    ++argv,--argc;
  cond_printf(1,"frontend:script_%s:argc=%d argv[0]=%s\n",cmd->name,argc,argv[0]);

  for(opt=local_opt;opt;opt=opt->next)
    if(strcmp(opt->name,"lisp")==0)
      opt->name=s_cat(q("*"),opt->name,NULL);
  if((in=fopen(argv[0],"rb"))!=NULL) {
    if(fgetc(in)!='#'||fgetc(in)!='!') {
      fclose(in);
      cmd_script(cons(argv,argc),cmd);
    }
    for(i=0;i<3;++i)
      while((c=fgetc(in))!=EOF && c!='\n');
    for(i=0;(c=fgetc(in))!=EOF;buf[i++]=c)
      if(c=='\r'||c=='\n'||i==799)
        break;
    fclose(in);
  }
  buf[i]='\0';
  cond_printf(1,"ros_script_cmd=%s\n",buf);
  argv_=parse_cmdline(buf,&argc_);
  argv_gen=alloc(sizeof(char**)*(argc+argc_));
  for(i=0;i<argc_-2&&strcmp(argv_[i+2],"$0")!=0;++i)
    argv_gen[i]=argv_[i+2];
  for(j=i;i<j+argc;++i)
    argv_gen[i]=argv[i-j];
  j=i;
  for(i=0;i<j;i+=proc_opt(j-i,&argv_gen[i],&top));
  return 0;
}

int setup(void) {
  if(lock_apply("setup",2))
    return 0; /* lock file exists */
  char* v=verbose==1?"-v ":(verbose==2?"-v -v ":"");
  lock_apply("setup",0);
  char* version=get_opt(DEFAULT_IMPL".version",0);
  if(!version) {
    SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install "DEFAULT_IMPL,NULL),"Installing "DEFAULT_IMPL"...\n");
  }else
    fprintf(stderr,"Already have "DEFAULT_IMPL".\n");
  SETUP_SYSTEM(cat(argv_orig[0]," ",v,lispdir(),"setup.ros main setup",NULL),"Making core for Roswell...\n");
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
      s(opt);
    }
    if(!impl)
      impl=DEFAULT_IMPL;
    impl=q(impl);
    if(version)
      version=q(version);
  }
  if(!version&&strcmp(impl,DEFAULT_IMPL)!=0) {
    cond_printf(1,"once!%s,%s\n",impl,version);
    s(version);
    version=q("system");
  }
  if(!(impl && version)) {
    s(impl);
    impl=q(DEFAULT_IMPL);
    setup();
    char* path=s_cat(configdir(),q("config"),NULL);
    global_opt=load_opts(path),s(path);
    version=get_opt(DEFAULT_IMPL".version",0);
  }
  return s_cat(impl,q("/"),version,NULL);
}

void star_set_opt(void) {
  char* config=configdir();
  char*lisp=get_opt("lisp",1);
  lisp=lisp?lisp:get_opt("*lisp",0);
  set_opt(&local_opt,"impl",determin_impl(lisp));
  set_opt(&local_opt,"quicklisp",s_escape_string(cat(config,"lisp",SLASH,"quicklisp",SLASH,NULL)));
  set_opt(&local_opt,"argv0",argv_orig[0]);
  set_opt(&local_opt,"wargv0",which(argv_orig[0]));
  set_opt(&local_opt,"homedir",q(config));
  set_opt(&local_opt,"verbose",qsprintf(10,"%d",verbose));
  if(get_opt("asdf.version",0))
    set_opt(&local_opt,"asdf",get_opt("asdf.version",0));
  s(config);
}

void star_rc(void) {
  char* init=s_cat(configdir(),q("init.lisp"),NULL);
  char* etc=ROSRC;
  char* current=get_opt("program",0);
  char *path,*would;
  if(file_exist_p(init)) {
    path=cat("(:load \"",init,"\")",NULL);
    would=cat(path,current?current:"",NULL);
    s(current);
    set_opt(&local_opt,"program",would);
    s(path);
  }
  s(init);
  current=get_opt("program",0);
  if(file_exist_p(etc)) {
    path=cat("(:load \"",etc,"\")",NULL);
    would=cat(path,current?current:"",NULL);
    set_opt(&local_opt,"program",would);
  }
}

char** star_wrap(char** arg) {
  //tbd
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
  char** argv=firstp(arg_);
  int argc=(int)rest(arg_);
  dealloc((void*)arg_);

  star_set_opt();
  if(rc)
    star_rc();
  char** arg=determin_args(argc,argv);
  char* opts=s_cat(q("("),sexp_opts(local_opt),sexp_opts(global_opt),q(")"),NULL);
  if(arg && file_exist_p(arg[0])) {
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
    ret=conss(q(argv_orig[0]),ret); 
    ret=conss(q("-L"),ret); 
    ret=conss(q(DEFAULT_IMPL),ret); 
    ret=conss(s_cat2(q(lispdir()),q("run.ros")),ret); 
    ret=conss(q(get_opt("impl",0)),ret); 
    ret=conss(q(get_opt("program",0)?get_opt("program",0):""),ret); 
    ret=conss(q(get_opt("restart",0)?get_opt("restart",0):""),ret);
    ret=conss(q(get_opt("script",0)?get_opt("script",0):""),ret);
    ret=conss(q(get_opt("verbose",0)),ret); 
    exec_arg(stringlist_array(nreverse(ret)));
  }
  s(opts);
  if(arg)cond_printf(0,"%s is not exist.stop.\n",arg[0]);
  return 1;
}

void register_cmd_run(void) {
  /*options*/
  run.option=register_runtime_options((LVal)NULL);
  run.option=add_command(run.option,"",NULL,cmd_run_star,OPT_SHOW_NONE,1);

  run.option=nreverse(run.option);

  /*commands*/
  top.option =add_command(top.option,""            ,NULL,cmd_script_frontend,OPT_SHOW_NONE,1);
  top.command=add_command(top.command,ROS_RUN_REPL ,NULL,cmd_run,OPT_SHOW_HELP,1);
  top.command=add_command(top.command,"*"          ,NULL,cmd_script_frontend,OPT_SHOW_NONE,1);
}
