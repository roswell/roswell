#include "opt.h"

int dispatch_with_subcmd(char* path,int argc,char** argv,struct proc_opt *popt) {
  char** argv2=(char**)alloc(sizeof(char*)*(argc+1));
  int i,ret;
  for(i=0;i<argc;++i)
    argv2[i+1]=argv[i];
  argv2[0]=path;
  ret=dispatch(argc+1,argv2,popt);
  dealloc(argv2);
  return ret;
}

char** proc_alias(int argc,char** argv,struct proc_opt *popt) {
  char* builtin[][2]= {
    {"-V","version"},
    {"-h","help"},
    {"-?","help"},
    //{"build","dump executable"},
  };
  int i;
  for (i=0;i<sizeof(builtin)/sizeof(char*[2]);++i) 
    if(!strcmp(builtin[i][0],argv[0])) {
      argv[0]=builtin[i][1];
      break;
    }
  return argv;
}

int proc_options(int argc,char** argv,struct proc_opt *popt) {
  if(argv[0][0]=='-' &&
     argv[0][1]=='-') { /*long option*/
    LVal p;
    for(p=popt->option;p;p=Next(p)) {
      struct sub_command* fp=firstp(p);
      if(strcmp(&argv[0][2],fp->name)==0) {
        int result= fp->call(cons(argv,argc),fp);
        if(fp->terminating) {
          cond_printf(1,"terminating:%s\n",argv[0]);
          exit(result);
        }else
          return result;
      }
    }
    cond_printf(1,"dispatch:invalid? %s\n",argv[0]);
  }else { /*short option*/
    if(argv[0][1]!='\0') {
      LVal p;
      for(p=popt->option;p;p=Next(p)) {
        struct sub_command* fp=firstp(p);
        if(fp->short_name&&strcmp(argv[0],fp->short_name)==0) {
          int result= fp->call(cons(argv,argc),fp);
          if(fp->terminating) {
            cond_printf(1,"terminating:%s\n",argv[0]);
            exit(result);
          }else
            return result;
        }
      }
    }
  }
  return 1;
}

int proc_set(int argc,char** argv,struct proc_opt *popt,int pos) {
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
  return 1+dispatch(argc-1,&(argv[1]),popt);
}

void proc_cmd(int argc,char** argv,struct proc_opt *popt) {
  char* tmp[]={"help"};
  LVal p,p2=0;
  /* search internal commands.*/
  for(p=popt->command;p;p=Next(p)) {
    struct sub_command* fp=firstp(p);
    if(fp->name) {
      if(strcmp(fp->name,argv[0])==0)
        exit(fp->call(cons(argv,argc),fp));
      if(strcmp(fp->name,"*")==0)
        p2=p;
    }
  }
  if(popt->top && position_char(".",argv[0])==-1) {
    /* local commands*/
    char* cmddir=configdir();
    char* cmdpath=cat(cmddir,argv[0],".ros",NULL);
    if(directory_exist_p(cmddir) && file_exist_p(cmdpath))
      dispatch_with_subcmd(cmdpath,argc,argv,popt);
    s(cmddir),s(cmdpath);
    /* systemwide commands*/
    cmddir=subcmddir();
    cmdpath=cat(cmddir,argv[0],".ros",NULL);
    if(directory_exist_p(cmddir)) {
      if(file_exist_p(cmdpath))
        dispatch_with_subcmd(cmdpath,argc,argv,popt);
      s(cmdpath);cmdpath=cat(cmddir,"+",argv[0],".ros",NULL);
      if(file_exist_p(cmdpath))
        dispatch_with_subcmd(cmdpath,argc,argv,popt);
    }
    s(cmddir),s(cmdpath);
  }
  if(p2) {
    struct sub_command* fp=firstp(p2);
    exit(fp->call(cons(argv,argc),fp));
  }
  fprintf(stderr,"invalid command\n");
  dispatch(1,tmp,&top);
}

int dispatch(int argc,char** argv,struct proc_opt *popt) {
  int pos;
  cond_printf(1,"dispatch:%s\n",argv[0]);
  argv=proc_alias(argc,argv,popt);
  if(argv[0][0]=='-' || argv[0][0]=='+')
    return proc_options(argc,argv,popt);
  else if((pos=position_char("=",argv[0]))!=-1)
    return proc_set(argc,argv,popt,pos);
  else
    proc_cmd(argc,argv,popt);
  return 1;
}

void dispatch_init(struct proc_opt *popt) {
  popt->option=(LVal)0;
  popt->command=(LVal)0;
  popt->top=0;
}
