#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#endif
#include "util.h"
#include "opt.h"

#ifdef _WIN32
BOOL WINAPI ConsoleCtrlHandler(DWORD ctrlChar){
  CHAR szPrintBuffer[512];
  DWORD nCharsWritten;
  if(CTRL_C_EVENT == ctrlChar){
    return TRUE;
  }
  return FALSE;
}
#endif

LVal run_commands=NULL;
LVal run_options =NULL;

char** cmd_run_sbcl(char* impl,char* version,int argc,char** argv)
{
  char** arg=NULL;
  char* home=homedir();
  char* arch=uname_m();
  char* os=uname();
  int offset=2;
  int i;
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  int core_p=1;
  char *bin= cat(impl_path,SLASH,"bin",SLASH,"sbcl",
#ifdef _WIN32
           ".exe",
#endif
           NULL);
  s(arch),s(os);
  for(i=1;i<argc;++i) {
    if(strcmp(argv[i],"--core")==0) {
      core_p=0;
      offset=0;
      break;
    }
  }
  arg=alloc(sizeof(char*)*(offset+2+argc));
  arg[0]=bin;
  if(core_p) {
    arg[1]="--core";
    arg[2]=cat(
#ifdef _WIN32
               //"\"",
#endif
               impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",
#ifdef _WIN32
               //"\"",
#endif
               NULL);
  }
  s(impl_path);
  
  for(i=1;i<argc;++i) {
    arg[i+offset]=argv[i];
  }
  arg[i+offset]=NULL;
  return arg;
}

int cmd_run(int argc,char **argv,struct sub_command* cmd)
{
  LVal options=run_options,commands=run_commands;
  if(argc==1) {
    char* tmp[]={"help","run"};
    return proccmd(2,tmp,top_options,top_commands);
  }else{
    return proccmd(argc-1,&argv[1],options,commands);
  }
}

int cmd_run_star(int argc,char **argv,struct sub_command* cmd)
{
  int ret=1;
  char* impl;
  char* version;

  int pos;
  impl=get_opt("lisp");
  if(impl && (pos=position_char("/",impl))!=-1) {
    version=subseq(impl,pos+1,0);
    impl=subseq(impl,0,pos);
  }else {
    if(!impl)
      impl=get_opt("default.lisp");
    version=get_opt("version");
    if(!version){
      char* opt=s_cat(q(impl),q("."),q("version"),NULL);
      version=get_opt(opt);
      s(opt);
    }
    if(impl) 
      impl=q(impl);
    if(version) 
      version=q(version);
  }

  if(impl) {
    char** arg=NULL;

    int i;
    if(strcmp(impl,"sbcl")==0 ||
       strcmp(impl,"sbcl-bin")==0) {
      arg=cmd_run_sbcl(impl,version,argc,argv);
    }
    if(file_exist_p(arg[0])) {
      char* cmd;
#ifdef _WIN32
      cmd=q(arg[0]);
      for(i=1;arg[i]!=NULL;++i) {
        cmd=s_cat(cmd,q(" "),q(arg[i]),NULL);
      }
      SetConsoleCtrlHandler(ConsoleCtrlHandler, TRUE);
      system(cmd);
      s(cmd);
#else
      execvp(arg[0],arg);
#endif
    }else{
      fprintf(stderr,"%s/%s is not installed.stop.\n",impl,version);
    }
  }else {
    fprintf(stderr,"impl doesn't specified stop\n");
  }
  s(impl),s(version);
  return ret;
}

int cmd_run_execute(int argc,char **argv,struct sub_command* cmd)
{
}

int cmd_run_output(int argc,char **argv,struct sub_command* cmd)
{
}

int cmd_run_help(int argc,char **argv,struct sub_command* cmd)
{
}

void register_cmd_run(void)
{
  char* _help;
  /*options*/
  /*commands*/
  run_commands=add_command(run_commands,"*" ,NULL,cmd_run_star,0,1,NULL,NULL);
  run_commands=add_command(run_commands,"help" ,NULL,cmd_run_help,0,1,NULL,NULL);

  top_commands=add_command(top_commands,"run"     ,NULL,cmd_run,1,1,"Run lisp environment",NULL);
  _help=cat("Usage: ",argv_orig[0]," run [OPTIONS] '(S-Expression)' [args...]\n"
            "Usage: ",argv_orig[0]," run [OPTIONS] script-file [args...]\n\n",NULL);
  top_helps=add_help(top_helps,"run",_help,run_commands,run_options,NULL,NULL);
  s(_help);
}
