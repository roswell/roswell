/* -*- tab-width : 2 -*- */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "opt.h"

char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

extern int cmd_internal(int argc,char **argv,struct sub_command* cmd);
extern int opt_version(int argc,char **argv,struct sub_command* cmd);

extern void register_cmd_run(void);
extern void register_cmd_install(void);
extern void register_cmd_internal(void);

int verbose=0;
int testing=0;
int rc=1;
int quicklisp=1;

LVal top_commands =(LVal)NULL;
LVal top_options =(LVal)NULL;

int proccmd(int argc,char** argv,LVal option,LVal command);

int proccmd_with_subcmd(char* path,char* subcmd,int argc,char** argv,LVal option,LVal command) {
  char** argv2=(char**)alloc(sizeof(char*)*(argc+2));
  int i,ret;
  for(i=0;i<argc;++i)
    argv2[i+2]=argv[i];
  argv2[0]=path;
  argv2[1]=subcmd;
  ret=proccmd(argc+2,argv2,option,command);
  dealloc(argv2);
  return ret;
}

int proccmd(int argc,char** argv,LVal option,LVal command) {
  int pos;
  char* alias=NULL;
  if(!strcmp(argv[0],"-V"))
    alias="version";
  else if(!strcmp(argv[0],"-h") || !strcmp(argv[0],"-?"))
    alias="help";
  else alias=argv[0];

  cond_printf(1,"proccmd:%s\n",argv[0]);
  if(alias[0]=='-' || alias[0]=='+') {
    if(alias[0]=='-' &&
       alias[1]=='-') { /*long option*/
      LVal p;
      for(p=option;p;p=Next(p)) {
        struct sub_command* fp=firstp(p);
        if(strcmp(&alias[2],fp->name)==0) {
          int result= fp->call(argc,argv,fp);
          if(fp->terminating) {
            cond_printf(1,"terminating:%s\n",alias);
            exit(result);
          }else
            return result;
        }
      }
      cond_printf(1,"proccmd:invalid? %s\n",alias);
    }else { /*short option*/
      if(alias[1]!='\0') {
        LVal p;
        for(p=option;p;p=Next(p)) {
          struct sub_command* fp=firstp(p);
          if(fp->short_name&&strcmp(alias,fp->short_name)==0) {
            int result= fp->call(argc,argv,fp);
            if(fp->terminating) {
              cond_printf(1,"terminating:%s\n",alias);
              exit(result);
            }else
              return result;
          }
        }
      }
      /* invalid */
    }
  }else if((pos=position_char("=",argv[0]))!=-1) {
    char *l,*r;
    l=subseq(argv[0],0,pos);
    r=subseq(argv[0],pos+1,0);
    if(r)
      set_opt(&local_opt,l,r);
    else{
      struct opts* opt=global_opt;
      struct opts** opts=&opt;
      unset_opt(opts, l);
      global_opt=*opts;
    }
    s(l),s(r);
    return 1+proccmd(argc-1,&(argv[1]),option,command);
  }else {
    char* tmp[]={"help"};
    LVal p,p2=0;
    /* search internal commands.*/
    for(p=command;p;p=Next(p)) {
      struct sub_command* fp=firstp(p);
      if(fp->name) {
        if(strcmp(fp->name,alias)==0)
          exit(fp->call(argc,argv,fp));
        if(strcmp(fp->name,"*")==0)
          p2=p;
      }
    }
    if(command==top_commands && position_char(".",alias)==-1) {
      /* local commands*/
      char* cmddir=configdir();
      char* cmdpath=cat(cmddir,alias,".ros",NULL);
      if(directory_exist_p(cmddir) && file_exist_p(cmdpath))
        proccmd_with_subcmd(cmdpath,"main",argc,argv,top_options,top_commands);
      s(cmddir),s(cmdpath);
      /* systemwide commands*/
      cmddir=subcmddir();
      cmdpath=cat(cmddir,alias,".ros",NULL);
      if(directory_exist_p(cmddir)) {
        if(file_exist_p(cmdpath))
          proccmd_with_subcmd(cmdpath,"main",argc,argv,top_options,top_commands);
        s(cmdpath);cmdpath=cat(cmddir,"+",alias,".ros",NULL);
        if(file_exist_p(cmdpath))
          proccmd_with_subcmd(cmdpath,"main",argc,argv,top_options,top_commands);
      }
      s(cmddir),s(cmdpath);
    }
    if(p2) {
      struct sub_command* fp=firstp(p2);
      exit(fp->call(argc,argv,fp));
    }
    fprintf(stderr,"invalid command\n");
    proccmd(1,tmp,top_options,top_commands);
  }
  return 1;
}

#define OPT_SETVAL(sym,rexp)                                    \
  int opt_##sym(int argc,char** argv,struct sub_command* cmd) { \
    sym=rexp;                                                   \
    cond_printf(1,"opt:%s:%d\n",cmd->name,sym);                 \
    return 1;}

OPT_SETVAL(verbose,(strcmp(cmd->name,"verbose")==0)?1|verbose<<1:verbose>>1)
OPT_SETVAL(testing,1+testing)
OPT_SETVAL(rc,(strcmp(cmd->name,"rc")==0)?1:0)
OPT_SETVAL(quicklisp,(strcmp(cmd->name,"quicklisp")==0)?1:0)

int opt_program0(int argc,char** argv,struct sub_command* cmd) {
  if(cmd->name) {
    char* current=get_opt("program",0);
    current=cat(current?current:"","(:",cmd->name,")",NULL);
    set_opt(&local_opt,"program",current);
  }
  return 1;
}

int opt_take1(int argc,char** argv,struct sub_command* cmd) {
  const char* arg=cmd->name;
  if(arg && argc>1)
    set_opt(&local_opt,arg,argv[1]);
  return 2;
}

#define OPT_APPEND(sym)                                            \
  int opt_##sym(int argc,char** argv,struct sub_command* cmd) {    \
    if(cmd->name && argc>1) {                                      \
      char* current=get_opt(#sym,0);                               \
      current=s_cat(current?q(current):q(""),                      \
                    q("(:"),q(cmd->name),q(" \""),                 \
                    escape_string(argv[1]),q("\")"),NULL);         \
      set_opt(&local_opt,#sym,current);                            \
    }                                                              \
    return 2;                                                      \
  }

OPT_APPEND(program)
OPT_APPEND(restart)
OPT_APPEND(final)

LVal register_runtime_options(LVal opt) {
  /*opt=add_command(opt,"file","-f",opt_program,1,0,"include lisp FILE while building","FILE");*/
  opt=add_command(opt,"load","-l",opt_program,1,0);
  opt=add_command(opt,"source-registry","-S",opt_program,1,0);
  opt=add_command(opt,"system","-s",opt_program,1,0);
  opt=add_command(opt,"load-system",NULL,opt_program,1,0);
  opt=add_command(opt,"package","-p",opt_program,1,0);
  opt=add_command(opt,"system-package","-sp",opt_program,1,0);
  opt=add_command(opt,"eval","-e",opt_program,1,0);
  opt=add_command(opt,"require",NULL,opt_program,1,0);
  opt=add_command(opt,"quit","-q",opt_program0,1,0);

  opt=add_command(opt,"restart","-r",opt_restart,1,0);
  opt=add_command(opt,"entry","-E",opt_restart,1,0);
  opt=add_command(opt,"init","-i",opt_restart,1,0);
  opt=add_command(opt,"print","-ip",opt_restart,1,0);
  opt=add_command(opt,"write","-iw",opt_restart,1,0);

  opt=add_command(opt,"final","-F",opt_final,1,0);

  opt=add_command(opt,"rc","-R",opt_rc,1,0);
  opt=add_command(opt,"no-rc","+R",opt_rc,1,0);
  opt=add_command(opt,"quicklisp","-Q",opt_quicklisp,1,0);
  opt=add_command(opt,"no-quicklisp","+Q",opt_quicklisp,1,0);
  opt=add_command(opt,"verbose","-v",opt_verbose,1,0);
  opt=add_command(opt,"quiet",NULL,opt_verbose,1,0);
  opt=add_command(opt,"test",NULL,opt_testing,1,0);
  opt=add_command(opt,"stdin",NULL,opt_program,0,0);
  return opt;
}

int main (int argc,char **argv) {
  argv_orig=argv;
  argc_orig=argc;
  /* toplevel */
  top_options=add_command(top_options,"version" ,NULL,opt_version,1,1);
  top_options=add_command(top_options,"wrap","-w",opt_take1,1,0);
  top_options=add_command(top_options,"image","-m",opt_take1,1,0);
  top_options=add_command(top_options,"lisp","-L",opt_take1,1,0);
  top_options=register_runtime_options(top_options);

  top_options=nreverse(top_options);
  /*commands*/
  register_cmd_install();
  top_commands=add_command(top_commands,"roswell-internal-use",NULL,cmd_internal,0,1);
  register_cmd_internal();
  register_cmd_run();
  top_commands=nreverse(top_commands);

  char* path=s_cat(configdir(),q("config"),NULL);
  global_opt=load_opts(path);
  struct opts** opts=&global_opt;
  unset_opt(opts,"program");
  s(path);
  if(argc==1) {
    char* tmp[]={"help"};
    proccmd(1,tmp,top_options,top_commands);
  }else {
    int i;
    for(i=1;i<argc;i+=proccmd(argc-i,&argv[i],top_options,top_commands));
  }
  if(get_opt("program",0)) {
    char* tmp[]={"run","-q","--"};
    proccmd(3,tmp,top_options,top_commands);
  }
  free_opts(global_opt);
}
