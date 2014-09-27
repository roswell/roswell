#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "opt.h"
#include "util.h"
char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

extern int cmd_pull(int argc,char **argv);
extern int cmd_list(int argc,char **argv);
extern int cmd_download(int argc,char **argv);
extern int cmd_tar(int argc,char **argv);
extern int cmd_version(int argc,char **argv);
extern int cmd_opt(int argc,char **argv);
extern int cmd_run(int argc,char **argv);

//dummy
int cmd_notyet(int argc,char **argv)
{
  printf("notyet\n");
  return 1;
}

static struct sub_command commands[] = {
  /* longopt,shortopt,cmd_function,?, */
  { " usage",NULL,cmd_notyet,0,1},
  { "help","h?",cmd_notyet,0,1},
  { "list",NULL,cmd_list,1,1},
  { "version",NULL,cmd_version,0,1},
  { "pull",NULL,cmd_pull,1,1},
  { "run",NULL,cmd_run,1,1},
  { "config",NULL,cmd_opt,1,1},
  { "set",NULL,cmd_notyet,0,1},
  { "tar",NULL,cmd_tar,0,1},
  { "download",NULL,cmd_download,0,1},
};

static struct sub_command options[] = {
  /* longopt,shortopt,cmd_function,?, */
  { " dummy",NULL,cmd_notyet,0,1},
};

int proccmd(int argc,char** argv) {
  int i;
  int pos;
  if(argv[0][0]=='-') {
    if(argv[0][1]=='-') { //long option
      for(i=0;i<sizeof(options)/sizeof(struct sub_command);++i) {
        if(strcmp(&argv[0][2],options[i].name)==0) {
          int result= options[i].call(argc,argv);
          if(options[i].terminating){
            exit(result);
          }else {
            return result;
          }
        }
      }
    }else { //short option
      if(argv[0][2]=='\0') {
        for(i=0;i<sizeof(options)/sizeof(struct sub_command);++i) {
          if(position_char(&argv[0][1],(char*)options[i].short_name)!=-1) {
            int result= options[i].call(argc,argv);
            if(options[i].terminating){
              exit(result);
            }else {
              return result;
            }
          }
        }
      }
    }
  }else if((pos=position_char("=",argv[0]))!=-1) {
    char *l,*r;
    l=subseq(argv[0],0,pos);
    r=subseq(argv[0],pos+1,0);
    if(r)
      set_opt(&local_opt,l,r,0);
    else{
      struct opts* opt=global_opt;
      struct opts** opts=&opt;
      unset_opt(opts, l);
      global_opt=*opts;
    }
    s(l),s(r);
  }else {
    for(i=0;i<sizeof(commands)/sizeof(struct sub_command);++i) {
      if(strcmp(&argv[0][0],commands[i].name)==0) {
        exit(commands[i].call(argc,argv));
      }
    }
    printf("invalid command %s\n",argv[0]);
  }
  return 1;
}

int main (int argc,char **argv) {
  int i;
  argv_orig=argv;
  argc_orig=argc;
  char* path=s_cat(homedir(),q("config"),NULL);
  global_opt=load_opts(path);
  s(path);
  if(argc==1) {
    char* tmp[]={" usage"};
    proccmd(0,tmp);
  }else{
    for(i=1;i<argc;i+=proccmd(argc-i,&argv[i]));
  }
  free_opts(global_opt);
}
