#include <stdio.h>
#include <stdlib.h>
#include "opt.h"
#include "util.h"
extern char** argv_orig;
int cmd_help(int argc, const char **argv)
{
  LVal help=(LVal)NULL;
  int i;int cmdmax,optmax;
  char *fmt;
  quicklisp=1;
  if(argc==1) {
    help=top_helps;
  }else {
    LVal i=top_helps;
    while(i) {
      struct command_help* p=firstp(i);
      if(p->name&&strcmp(p->name,argv[argc==1?0:1])==0)
        help=i;
      i=Next(i);
    }
  }
  if(help) {
    struct command_help* h=firstp(help);
    if(h->call) {
      return h->call(argc-1,(char**)&(argv[1]),NULL);
    }
    if(h->usage)
      fprintf(stderr,h->usage,argv_orig[0]);

    LVal p;
    LVal pinit;
    int i;
    char* subcmds=s_cat(lispdir(),q("subcmd"),q(SLASH),NULL);
    LVal dir=directory(subcmds);

    for(i=0;i<2;++i) {
      cmdmax=0;
      optmax=0;
      char *cmd_opt;
      if(i==0){
        cmd_opt="Commands:\n";
        pinit=h->commands;
      }else{
        cmd_opt="Options:\n";
        pinit=h->opts;
      }
      for(p=pinit;p;p=Next(p)) {
        struct sub_command* fp=firstp(p);
        int len=fp && fp->show_opt?strlen(fp->name):0;
        len+=fp && fp->show_opt && fp->arg_example?strlen(fp->arg_example):0;
        if(cmdmax<len)
          cmdmax=len;
        len=fp && fp->show_opt && fp->short_name ?strlen(fp->short_name):0;
        len+=fp && fp->show_opt && fp->arg_example?strlen(fp->arg_example):0;
        if(optmax<len)
          optmax=len;
      }
      if(i==0 && help==top_helps) {
        LVal x=dir;
        LVal v;
        for(v=x;v;v=Next(v)) {
          char* s=firsts(v);
          int len=strlen(s)-4; /* ".ros" */
          if (len>0 && strcmp(s+len,".ros")==0 && cmdmax<len)
            cmdmax=len;
        }
      }
      if(cmdmax) {
        fprintf(stderr,"%s",cmd_opt);
        if(i==0)
          fmt=s_cat(q("    %-"),qsprintf(5,"%d",cmdmax+2),q("s%s\n"),NULL);
        else
          fmt=s_cat(q("    %-"),qsprintf(5,"%d",optmax+2),q("s --%-"),qsprintf(5,"%d",cmdmax+2),q("s%s"),NULL);
        for(p=pinit;p;p=Next(p)) {
          struct sub_command* fp=firstp(p);
          if(fp && fp->name && fp->show_opt) {
            if(i==0)
              fprintf(stderr,fmt,fp->name,fp->description);
            else {
              char* tmp=cat((char*)(fp->name?fp->name:"")," ",fp->arg_example&&fp->name?fp->arg_example:"",NULL);
              char* tmp2=cat((char*)(fp->short_name?fp->short_name:"")," ",fp->arg_example&&fp->short_name?fp->arg_example:"",NULL);
              fprintf(stderr,fmt,tmp2,tmp,fp->description?fp->description:"");
              fprintf(stderr,"\n");
              s(tmp),s(tmp2);
             }
          }
        }
        if(i==0 && help==top_helps) {
          LVal v;
          for(v=dir;v;v=Next(v)) {
            char* f=firsts(v);
            int len=strlen(f)-4; /* ".ros" */
            if (len>0 && strcmp(f+len,".ros")==0) {
              FILE* in;
              char buf[800];
              char* fname=cat(subcmds,SLASH,f);
              if((in=fopen(fname,"r"))!=NULL) {
                fgets(buf,800,in);
                fgets(buf,800,in);
                fclose(in);
              }else{
                strcpy(buf,"");
              }
              s(fname);
              f[len]='\0';
              buf[strlen(buf)-2]='\0';
              fprintf(stderr,fmt,f,buf+1);
            }
          }
        }
        s(fmt);
        fprintf(stderr,"\n");
      }
    }
    sL(dir);
    s(subcmds);
  }
  return 0;
}
