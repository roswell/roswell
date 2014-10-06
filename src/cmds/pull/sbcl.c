#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "pull.h"
#include "opt.h"

int sbcl_start(struct install_options* param)
{
  if(strcmp(param->impl,"sbcl")==0 && !get_opt("sbcl.compiler")) {
    printf("compiler variable 'sbcl.compiler'.assume it as 'sbcl-bin'\n");
    set_opt(&local_opt,"sbcl.compiler",(char*)"sbcl-bin",0);
  }
  return 1;
}

char* log_path;
LVal sbcl_make_redirected(LVal v) {
  FILE *in,*out;
  char c;
  int counter=0;
  if((out=fopen(log_path,"a"))!=NULL) {
    in=(FILE*)v;
    printf("%7d ",0);
    while((c = fgetc(in)) != EOF) {
      if(c=='\n') {
        ++counter;
        if(counter%20==0)
          printf(".");
        if(counter%1200==0)
          printf("\n%7d ",counter);
        fflush(stdout);
      }
      if (fputc(c, out) == EOF) {
        break;
      }
    }
    fclose(out);
  }
  printf("\n make done with %dlines output.\n",counter);
}

int sbcl_make(struct install_options* param) {
  char* impl=param->impl;
  char* version=param->version;
  char* home=homedir();
  char* src=cat(home,"src",SLASH,impl,"-",version,NULL);
  char* arg0=truename(argv_orig[0]);
  char* compiler=cat(arg0," lisp=",get_opt("sbcl.compiler")," --no-rc run --",NULL);
  char* cmd=cat("sh make.sh \"--xc-host=",compiler,"\" ","--prefix=",home,"impls",SLASH,param->arch,SLASH,param->os,SLASH,impl,SLASH,version,NULL);
  log_path=cat(home,"impls/log/",impl,"-",version,"/make.log",NULL);

  printf("Building %s-%s with %s\n",impl,version,compiler);
  change_directory(src);
  ensure_directories_exist(log_path);
  printf("cmd:%s\n",cmd);
  if(system_redirect_function(cmd,sbcl_make_redirected)==-1) {
    return 0;
  }
  s(home),s(src),s(cmd),s(compiler),s(arg0),s(log_path);
  return 1;
}

int sbcl_install(struct install_options* param) {
#ifndef _WIN32
  int ret=1;
  char* home= homedir();
  char* impl=param->impl;
  char* version=param->version;
  char* impl_path= cat(home,"impls",SLASH,param->arch,SLASH,param->os,SLASH,impl,SLASH,version,NULL);
  char* src=param->expand_path;
  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  char* install_root=q(impl_path);
  char* log_path=cat(home,"impls/log/",impl,"-",version,"/install.log",NULL);
  printf("installing %s/%s ",impl,version);
  ensure_directories_exist(impl_path);
  ensure_directories_exist(log_path);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  
  if(system_redirect("sh install.sh",log_path)==-1) {
    ret=0;
  }
  s(home),s(impl_path),s(sbcl_home),s(install_root),s(log_path);
  printf("done.\n");
  return ret;
#else

#endif
}

int sbcl_version(struct install_options* param)
{
  if(param->version && strcmp(param->version,"latest")!=0) {
    param->version= q(param->version);
    return 1;
  }
  /* TBD */
  printf("detecting versions\n");
  param->version= q("1.2.3");
  return 1;
}

char* sbcl_uri(struct install_options* param)
{
  char* version=param->version;
  return cat("http://sourceforge.net/projects/sbcl/files/sbcl/",version,
	     "/sbcl-",version,"-source.tar.bz2",NULL);
}

char* sbcl_extention(struct install_options* param)
{
  return "tar.bz2";
}

install_cmds install_sbcl_full[]={
  sbcl_version,
  sbcl_start,
  start,
  download,
  expand,
  sbcl_make,
  sbcl_install,
  NULL
};

struct install_impls impls_sbcl={ "sbcl", install_sbcl_full,sbcl_uri,sbcl_extention};
