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

extern int cmd_list(int argc,char **argv,struct sub_command* cmd);
extern int cmd_download(int argc,char **argv,struct sub_command* cmd);
extern int cmd_tar(int argc,char **argv,struct sub_command* cmd);
extern int cmd_version(int argc,char **argv,struct sub_command* cmd);
extern int cmd_config(int argc,char **argv,struct sub_command* cmd);
extern int cmd_help(int argc,char **argv,struct sub_command* cmd);

extern void register_cmd_run(void);
extern void register_cmd_pull(void);
//dummy
int cmd_notyet(int argc,char **argv,struct sub_command* cmd)
{
  printf("notyet\n");
  return 1;
}

LVal top_commands =NULL;
LVal top_options =NULL;

LVal top_helps =NULL;

int proccmd(int argc,char** argv,LVal option,LVal command) {
  int i;
  int pos;
  if(argv[0][0]=='-' || argv[0][0]=='+') {
    if(argv[0][0]=='-' &&
       argv[0][1]=='-' &&
       argv[0][2]!='\0') { /*long option*/
      LVal p;
      for(p=option;p;p=Next(p)) {
        struct sub_command* fp=firstp(p);
        if(strcmp(&argv[0][2],fp->name)==0) {
          int result= fp->call(argc,argv,fp);
          if(fp->terminating) {
            exit(result);
          }else {
            return result;
          }
        }
      }
      /* invalid option? */
    }else { /*short option*/
      if(argv[0][1]!='\0') {
        LVal p;
        for(p=option;p;p=Next(p)) {
          struct sub_command* fp=firstp(p);
          if(fp->short_name&&strcmp(argv[0],fp->short_name)==0) {
            int result= fp->call(argc,argv,fp);
            if(fp->terminating) {
              exit(result);
            }else {
              return result;
            }
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
      set_opt(&local_opt,l,r,0);
    else{
      struct opts* opt=global_opt;
      struct opts** opts=&opt;
      unset_opt(opts, l);
      global_opt=*opts;
    }
    s(l),s(r);
    return proccmd(argc-1,&(argv[1]),option,command);
  }else {
    char* tmp[]={"help"};
    LVal p;
    for(p=command;p;p=Next(p)) {
      struct sub_command* fp=firstp(p);
      if(fp->name&&(strcmp(fp->name,argv[0])==0||strcmp(fp->name,"*")==0)) {
        exit(fp->call(argc,argv,fp));
      }
    }
    printf("invalid command\n");
    proccmd(1,tmp,top_options,top_commands);
  }
  return 1;
}

int opt_top(int argc,char** argv,struct sub_command* cmd) {
  char* arg=argv[0];
  if(strcmp("lisp",arg)==0) {
    set_opt(&local_opt,"lisp",argv[1],0);
  }else if(strcmp("wrap",arg)==0) {
    set_opt(&local_opt,"wrap",argv[1],0);
  }else if(strcmp("image",arg)==0) {
    set_opt(&local_opt,"image",argv[1],0);
  }
  return 2;
}

int main (int argc,char **argv) {
  int i;
  argv_orig=argv;
  argc_orig=argc;
  /*options*/
  top_options=add_command(top_options,"wrap","-w",opt_top,1,0,"load lisp FILE while building","CODE");
  top_options=add_command(top_options,"lisp","-l",opt_top,1,0,"try use these LISP implementation","LIST");
  top_options=add_command(top_options,"image","-m",opt_top,1,0,"build from Lisp image IMAGE","IMAGE");
  top_options=add_command(top_options,"file","-f",opt_top,1,0,"include lisp FILE while building","FILE");
  top_options=add_command(top_options,"load","-L",cmd_notyet,1,0,"load lisp FILE while building","FILE");
  top_options=add_command(top_options,"source-registry","-S",cmd_notyet,1,0,"override source registry of asdf systems","X");
  top_options=add_command(top_options,"system","-s",cmd_notyet,1,0,"load asdf SYSTEM while building","SYSTEM");
  top_options=add_command(top_options,"load-system",NULL,cmd_notyet,1,0,"same as above (buildapp compatibility)","SYSTEM");
  top_options=add_command(top_options,"package","-p",cmd_notyet,1,0,"change current package to PACKAGE","PACKAGE");
  top_options=add_command(top_options,"system-package","-sp",cmd_notyet,1,0,"combination of -s SP and -p SP","SP");
  top_options=add_command(top_options,"eval","-e",cmd_notyet,1,0,"evaluate FORM while building","FORM");
  top_options=add_command(top_options,"require",NULL,cmd_notyet,1,0,"require MODULE while building","MODULE");
  top_options=add_command(top_options,"restart","-r",cmd_notyet,1,0,"restart from build by calling (FUNC)","FUNC");
  top_options=add_command(top_options,"entry","-E",cmd_notyet,1,0,"restart from build by calling (FUNC argv)","FUNC");
  top_options=add_command(top_options,"init","-i",cmd_notyet,1,0,"evaluate FORM after restart","FORM");
  top_options=add_command(top_options,"print","-ip",cmd_notyet,1,0,"evaluate and princ FORM after restart","FORM");
  top_options=add_command(top_options,"write","-iw",cmd_notyet,1,0,"evaluate and write FORM after restart","FORM");
  top_options=add_command(top_options,"final","-F",cmd_notyet,1,0,"evaluate FORM before dumping IMAGE","FORM");
  top_options=add_command(top_options,"include","-I",cmd_notyet,1,0,"runtime PATH to cl-launch installation","PATH");
  top_options=add_command(top_options,"no-include","+I",cmd_notyet,1,0,"disable cl-launch installation feature",NULL);
  top_options=add_command(top_options,"rc","-R",cmd_notyet,1,0,"try read /etc/cl-launchrc, ~/.cl-launchrc",NULL);
  top_options=add_command(top_options,"no-rc","+R",cmd_notyet,1,0,"skip /etc/cl-launchrc, ~/.cl-launchrc",NULL);
  top_options=add_command(top_options,"quicklisp","-Q",cmd_notyet,1,0,"use quicklisp",NULL);
  top_options=add_command(top_options,"no-quicklisp","+Q",cmd_notyet,1,0,"do not use quicklisp",NULL);
  top_options=add_command(top_options,"verbose","-v",cmd_notyet,1,0,"be quite noisy while building",NULL);
  top_options=add_command(top_options,"quiet","-q",cmd_notyet,1,0,"be quite quiet while building (default)",NULL);
  top_options=add_command(top_options,"version",NULL,cmd_version,0,1,NULL,NULL);

  top_options=nreverse(top_options);
  /*commands*/
  register_cmd_run();
  register_cmd_pull();
  top_commands=add_command(top_commands,"execute" ,"!",cmd_notyet,1,1,"Run the specified software without generating a script",NULL);
  top_commands=add_command(top_commands,"output" ,NULL,cmd_notyet,1,1,"Generate an executable script or binary from the software specification",NULL);
  top_commands=add_command(top_commands,"config"  ,NULL,cmd_config,1,1,"Get and set options",NULL);
  /*         {"list",NULL,cmd_list,1,1}, */
  /*         {"set",NULL,cmd_notyet,0,1}, */
  top_commands=add_command(top_commands,"version" ,NULL,cmd_version,1,1,"Show the "PACKAGE" version information",NULL);
  top_commands=add_command(top_commands,"tar"     ,NULL,cmd_tar,0,1,NULL,NULL);
  top_commands=add_command(top_commands,"download",NULL,cmd_download,0,1,NULL,NULL);
  top_commands=add_command(top_commands,"help","h,?",cmd_help,0,1,NULL,NULL);

  top_commands=nreverse(top_commands);
  top_helps=add_help(top_helps,NULL,"Usage: %s [OPTIONS] COMMAND [args...]\n\n",top_commands,top_options,NULL,NULL);
  char* path=s_cat(homedir(),q("config"),NULL);
  global_opt=load_opts(path);
  s(path);
  if(argc==1) {
    char* tmp[]={"help"};
    proccmd(1,tmp,top_options,top_commands);
  }else
    for(i=1;i<argc;i+=proccmd(argc-i,&argv[i],top_options,top_commands));
  free_opts(global_opt);
}
