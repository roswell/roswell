#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "pull.h"
#include "opt.h"

int sbcl_start(char* impl,char* version)
{
  if(strcmp(impl,"sbcl")==0 && !get_opt("sbcl.compiler")) {
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


int sbcl_make(char* impl,char* version) {
  char* home=homedir();
  char* src=cat(home,"src/",impl,"-",version,NULL);
  char* compiler=cat(argv_orig[0]," -impl ",get_opt("sbcl.compiler")," run",NULL);
  char* cmd=cat("sh make.sh \"",compiler,"\" ","--prefix=",home,"impls/",impl,"-",version,NULL);
  log_path=cat(home,"impls/log/",impl,"-",version,"/make.log",NULL);

  printf("Building %s-%s\n",impl,version);
  change_directory(src);
  ensure_directories_exist(log_path);
  printf("cmd:%s\n",cmd);
  if(system_redirect_function(cmd,sbcl_make_redirected)==-1) {
    return 0;
  }
  s(home),s(src),s(cmd),s(compiler),s(log_path);
  return 1;
}

int sbcl_install(char* impl,char* version) {
#ifndef _WIN32
  int ret=1;
  char* home= homedir();
  if(strcmp(impl,"sbcl-bin")==0) {
    impl="sbcl";
  }
  char* impl_path= cat(home,"impls/",impl,"-",version,NULL);
  char* src=cat(home,"src/",impl,"-",version,NULL);
  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  char* install_root=q(impl_path);
  char* log_path=cat(home,"impls/log/",impl,"-",version,"/install.log",NULL);
  impl="sbcl";
  printf("installing %s-%s ",impl,version);
  ensure_directories_exist(impl_path);
  ensure_directories_exist(log_path);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  
  if(system_redirect("sh install.sh",log_path)==-1) {
    ret=0;
  }
  s(home),s(impl_path),s(src),s(sbcl_home),s(install_root),s(log_path);
  printf("done.\n");
  return ret;
#else
#error not implemented.
#endif
}

char* sbcl_version(char* impl,char* version)
{
  if(version && strcmp(version,"latest")!=0) {
    return q(version);
  }
  /* TBD */
  printf("detecting versions\n"); 
  return q("1.2.3");
}

char* sbcl_uri(char* impl,char* version)
{
  return cat("http://sourceforge.net/projects/sbcl/files/sbcl/",version,
	     "/sbcl-",version,"-source.tar.bz2",NULL);
}

char* sbcl_extention(char* impl,char* version)
{
  return "tar.bz2";
}

install_cmds install_sbcl_full[]={
  sbcl_start,
  start,
  download,
  expand,
  sbcl_make,
  sbcl_install,
  NULL
};

struct install_impls impls_sbcl={ "sbcl", install_sbcl_full,sbcl_version,sbcl_uri,sbcl_extention};
