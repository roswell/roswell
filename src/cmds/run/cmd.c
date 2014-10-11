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

extern char** cmd_run_sbcl(char* impl,char* version,int argc,char** argv);

int cmd_run(int argc,char **argv,struct sub_command* cmd)
{
  char* current=get_opt("program");
  if(argc==1 && !current) {
    char* tmp[]={"help",(char*)cmd->name};
    return proccmd(2,tmp,top_options,top_commands);
  }else {
    int i;
    if(verbose>0)
      fprintf(stderr,"cmd_run:argc=%d,cmd->name:%s argv[0]=%s\n",argc,cmd->name,argv[0]);
    for(i=1;i<argc;i+=proccmd(argc-i,&argv[i],run_options,run_commands));
    current=get_opt("program");
    if(current&& strcmp((char*)cmd->name,"run")!=0) {
      char* tmp[]={"--"};
      proccmd(1,tmp,run_options,run_commands);
    }else {
      char* tmp[]={"help","run"};
      return proccmd(2,tmp,top_options,top_commands);
    }
    if(verbose>0) {
      fprintf(stderr,"cmd_run ends here %d\n",i);
    }
    return i;
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
    if(impl) {
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

void register_cmd_run(void)
{
  char* _help;
  /*options*/
  run_options=add_command(top_options,"",NULL,cmd_run_star,0,1,NULL,NULL);
  /*commands*/
  top_commands=add_command(top_commands,"script" ,NULL,cmd_run,1,1,"Run lisp environment then quit (default)",NULL);
  top_commands=add_command(top_commands,"run"    ,NULL,cmd_run,1,1,"Run lisp environment",NULL);
  top_commands=add_command(top_commands,"output" ,NULL,cmd_run,1,1,"Generate an executable script or binary from the software specification",NULL);
  _help=cat("Usage: ",argv_orig[0]," [OPTIONS] run [OPTIONS] -- [implementation-native-options...]\n\n",NULL);
  top_helps=add_help(top_helps,"run",_help,run_commands,run_options,NULL,NULL);
  s(_help);
  _help=cat("Usage: ",argv_orig[0]," OPTIONS [script] [OPTIONS] [-- implementation-native-options...]\n\n",NULL);
  top_helps=add_help(top_helps,"script",_help,run_commands,run_options,NULL,NULL);
  s(_help);
}
