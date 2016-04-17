/* -*- tab-width : 2 -*- */
#include "opt.h"

char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

extern LVal register_runtime_options();
extern LVal register_runtime_commands();

int verbose=0;
int testing=0;
int rc=1;
int quicklisp=1;

LVal top_commands =(LVal)NULL;
LVal top_options;

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

int main (int argc,char **argv) {
  int i;
  char* path=s_cat(configdir(),q("config"),NULL);
  argv_orig=argv;
  argc_orig=argc;

  top_options=register_runtime_options();
  top_commands=register_runtime_commands();

  global_opt=load_opts(path);
  struct opts** opts=&global_opt;
  unset_opt(opts,"program");
  s(path);
  if(argc==1)
    {char* tmp[]={"help"};proccmd(1,tmp,top_options,top_commands);}
  else
    for(i=1;i<argc;i+=proccmd(argc-i,&argv[i],top_options,top_commands));
  if(get_opt("program",0))
    {char* tmp[]={"run","-q","--"};proccmd(3,tmp,top_options,top_commands);}
  free_opts(global_opt);
}
