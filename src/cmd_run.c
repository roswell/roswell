#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "opt.h"

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
      impl=get_opt("default-impl");
    version=get_opt("version");
    when(!version){
      char* opt=cat(q(impl),q("-"),q("version"),NULL);
      version=get_opt(opt);
      s(opt);
    }
    if(impl) 
      impl=q(impl);
    if(version) 
      version=s(version);
  }
  if(impl) {
    char** arg=NULL;
    char* bin=NULL;
    int offset=0;
    int i;
    if(strcmp(impl,"sbcl-bin")==0) {
      impl="sbcl";
      if(version) {
	version=cat(q(version),q("-"),uname_m(),q("-"),uname(),NULL);
      }
    }
    if(strcmp(impl,"sbcl")==0) {
      char* impl_path= cat(home,"impls/",impl,"-",version,NULL);
      int core_p=1;
      
      bin= cat(impl_path,"/bin/sbcl",NULL);
      for(i=0;i<argc;++i) {
	if(strcmp(argv[i],"--core")==0) {
	  core_p=0;
	  break;
	}
      }
      arg=alloc(sizeof(char*)*(offset+2+argc));
      arg[0]=bin;
      if(core_p) {
	offset=2;
	arg[1]="--core";
	arg[2]=cat(impl_path,"/lib/sbcl/sbcl.core",NULL);
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
    arg[i+1+offset]=NULL;
    execvp(bin,arg);
  }else {
    printf("impl doesn't specified stop");
  }
  s(home),s(impl),s(version);
  return ret;
}
