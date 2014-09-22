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

int cmd_run(int argc,char **argv)
{
  int ret=1;
  char* impl;
  char* version;
  char* home=homedir();
  int pos;
  impl=get_opt("impl");
  if(impl && (pos=position_char("/",impl))!=-1) {
    version=subseq(impl,pos+1,0);
    impl=subseq(impl,0,pos);
  }else {
    if(!impl)
      impl=get_opt("default.impl");
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
    char* bin=NULL;
    int offset=0;
    int i;
    if(strcmp(impl,"sbcl-bin")==0) {
      s(impl);
      impl=q("sbcl");
      if(version) {
	version=s_cat(q(version),q("-"),uname_m(),q("-"),uname(),NULL);
      }
    }
    if(strcmp(impl,"sbcl")==0) {
      char* impl_path= cat(home,"impls",SLASH,impl,"-",version,NULL);
      int core_p=1;
      offset=2;
      bin= cat(impl_path,SLASH,"bin",SLASH,"sbcl",
#ifdef _WIN32
               ".exe",
#endif
               NULL);
      for(i=0;i<argc;++i) {
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
	arg[2]=cat("\"",impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core","\"",NULL);
      }
      s(impl_path);
    }else if (strcmp(impl,"native")==0) {
      bin= which(version);
      if(strcmp(bin,"")!=0) {
	arg=alloc(sizeof(char*)*(offset+2+argc));
	arg[0]=bin;
      }else {
	printf("can't find %s.\n",version);
	exit(EXIT_FAILURE);
      }
    }

    for(i=0;i<argc;++i) {
      arg[i+1+offset]=argv[i];
    }
    if(file_exist_p(bin)) {
      arg[i+1+offset]=NULL;
#ifdef _WIN32
      s(home);home=q(arg[0]);
      for(i=1;arg[i]!=NULL;++i) {
        home=s_cat(home,q(" "),q(arg[i]),NULL);
      }
      SetConsoleCtrlHandler(ConsoleCtrlHandler, TRUE);
      system(home);
      //_execvp(bin,arg);
#else
      execvp(bin,arg);
#endif
    }else{
      printf("%s/%s is not installed.stop.\n",impl,version);
    }
  }else {
    printf("impl doesn't specified stop");
  }
  s(home),s(impl),s(version);
  return ret;
}
