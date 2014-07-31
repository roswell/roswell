#include <stdio.h>
#include <stdlib.h>
#include "util.h"

static int in_resume=0;
static char *flags =NULL;
typedef int (*install_cmds)(char* impl,char* version);
install_cmds *cmds=NULL;

int installed_p(char* impl,char* version) 
{
  int ret;
  char* i;
  if(strcmp(impl,"sbcl-bin")==0){
    impl="sbcl";
  }
  i=s_cat(homedir(),q("impls/"),q(impl),q("-"),q(version),q("/"),NULL);
  ret=directory_exist_p(i);
  s(i);
  return ret;
}

int install_running_p(char* impl,char* version)
{
  /* TBD */
  return 0;
}

int start(char* impl,char* version)
{
  char* home= homedir();
  char* p;
  ensure_directories_exist(home);
  if(installed_p(impl,version)) {
    printf("%s-%s are already installed.if you intend to reinstall by (TBD).\n",impl,version);
    return 0;
  }
  if(install_running_p(impl,version)) {
    printf("It seems running installation process for $1-$2.\n");
    return 0;
  }
  if(strcmp(impl,"sbcl")==0 && NULL==get_opt("sbcl.compiler")) {
    printf("compiler variable 'sbcl.compiler' should be specified\n");
    return 0;
  }
  /*trap "exit 1" HUP INT PIPE QUIT TERM*/
  /*trap "rm -f $CIM_HOME/tmp/$1-$2.lock" EXIT*/
  p=cat(home,"tmp/",impl,"-",version,"/",NULL);
  ensure_directories_exist(p);
  s(p);
 
  p=cat(home,"tmp/",impl,"-",version,".lock",NULL);
  touch(p);
  s(p);
  s(home);
  return 1;
}

char* bz2distp(char* impl)
{
  /* TBD rename function */
  if(strcmp(impl,"sbcl")==0||
     strcmp(impl,"sbcl-bin")==0||
     strcmp(impl,"alisp")==0) {
    return "bz2";
  }
  return "gz";
}

char* get_default_version(char* impl)
{
  /* TBD */
  if(strcmp(impl,"sbcl")==0) {
    return q("1.2.1");
  }else if(strcmp(impl,"sbcl-bin")==0) {
    return s_cat(get_default_version("sbcl"),q("-"),uname_m(),q("-"),uname(),NULL);
  }
}

char* get_download_path(char** impl,char** version)
{
  if(strcmp(*impl,"sbcl")==0 && *version) {
    return cat("http://sourceforge.net/projects/sbcl/files/sbcl/",*version,
	       "/sbcl-",*version,"-source.tar.bz2",NULL);
  }else if(strcmp(*impl,"sbcl-bin")==0) {
    return q("http://prdownloads.sourceforge.net/sbcl/sbcl-1.2.1-x86-64-linux-binary.tar.bz2");
  }
  return NULL;
}

int download(char* impl,char* version)
{
  char* home= homedir();
  char* url;
  char* impl_archive;
  printf("Donwloading archive.\n");
  url=get_download_path(&impl,&version);
  if(url) {
    impl_archive=cat(home,"archives/",impl,"-",version,".tar.",bz2distp(impl),NULL);
    ensure_directories_exist(impl_archive);

    /* pipe connect for logging cim_with_output_control */
    if(download_simple(url,impl_archive,0)) {
      printf("Failed to Download.\n");
      return 0;
      /* fail */
    }else{
      printf("download done:%s\n",url);
    }
  }
  s(impl_archive);
  s(url);
  s(home);
  return 1;
}

int expand(char* impl,char* version)
{
  char* home= homedir();
  char* argv[5]={"-xf",NULL,"-C",NULL,NULL};
  int argc=4;
  char* archive;
  char* dist_path;
  
  archive=cat(impl,"-",version,".tar.",bz2distp(impl),NULL);
  if(strcmp(impl,"sbcl-bin")==0) {
    impl="sbcl";
  }else {
  }
  version=q(version);
  dist_path=cat(home,"src/",impl,"-",version,"/",NULL);

  printf("Extracting archive. %s to %s\n",archive,dist_path);
  
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);

  /* pipe connect for logging cim_with_output_control */
  argv[1]=cat(home,"archives/",archive,NULL);
  argv[3]=cat(home,"src/",NULL);
  cmd_tar(argc,argv);

  s(argv[1]);
  s(argv[3]);
  s(dist_path);
  s(archive);
  s(home);
  s(version);
  return 1;
}

int configure(char* impl,char* version)
{
  char* home= homedir();
  char* confgcache= cat(home,"/src/",impl,"-",version,"/src/confg.cache",NULL);
  char* cd;
  char* configure;
  int ret;
  if(in_resume) {
    delete_file(confgcache);
  }
  if(flags==NULL) {
    flags=q("");
  }
  if(strcmp("gcl",impl)==0) {
    flags=s_cat(flags,q(" --enable-ansi"));
  }
  if(strcmp("ecl",impl)==0 ||strcmp("ecl",impl)==0 || strcmp("clisp",impl)==0) {
    flags=s_cat(flags,q(" --mandir="),q(home),q("/share/man"));
  }
  printf("Configuring %s-%s\n",impl,version);
  cd=cat(home,"src/",impl,"-",version,NULL);
  printf ("cd:%s\n",cd);
  change_directory(cd);
  /* pipe connect for logging cim_with_output_control */
  configure=cat("./configure ",flags," --prefix=",home,"/impls/",impl,"-",version,NULL);
  ret=system(configure);
  s(configure);
  s(cd);
  s(confgcache);
  s(home);
}

/*int sbcl_ensure_impl(char* impl,char* version) {
  int ret=1;
  char* bin_version;
  printf((bin_version=q("Finding Lisp impl to build sbcl.\n")));
  char* sbcl_bin_url=get_download_path("sbcl-bin",bin_version);
  if(sbcl_bin_url){
    if(!directory_exist_p()) {
      
      }
    s(sbcl_bin_url);
  }else {
    printf("can't download sbcl\n");
    ret=0;
  }
  s(bin_version);
  return ret;
}
*/

int sbcl_make(char* impl,char* version) {
  char* home=homedir();
  char* src=cat(home,"src/",impl,"-",version,NULL);
  char* compiler=cat(argv_orig[0]," -impl ",get_opt("sbcl.compiler")," run",NULL);
  char* cmd=cat("sh make.sh \"",compiler,"\" ","--prefix=\"",home,"impls/",impl,"-",version,"\"",NULL);
  printf("Building %s-%s\n",impl,version);
  change_directory(src);
  printf("cmd:%s\n",cmd);
  system(cmd);
  /* pipe connect for logging cim_with_output_control */
  s(home),s(src),s(cmd),s(compiler);
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
  impl="sbcl";
  printf("installing %s-%s\n",impl,version);
  ensure_directories_exist(impl_path);
  printf("cd:%s\n",src);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  if(system_redirect("sh install.sh","/tmp/hoge")==-1) {
    ret=0;
  }
  s(home);
  s(impl_path);
  s(src);
  s(sbcl_home);
  s(install_root);
  return ret;
#else
#error not implemented.
#endif
}

install_cmds install_full[]={
  start,
  download,
  expand,
  configure,
  NULL
};
install_cmds install_sbcl_full[]={
  start,
  download,
  expand,
  sbcl_make,
  sbcl_install,
  NULL
};

install_cmds install_sbcl_bin_full[]={
  start,
  download,
  expand,
  sbcl_install,
  NULL
};

int cmd_install(int argc,char **argv)
{
  int ret=1;
  if(argc!=0) {
    char* impl=q(argv[0]);
    char* version=NULL;
    if(strcmp(impl,"sbcl-bin")==0) {
      cmds=install_sbcl_bin_full;
    }else if(strcmp(impl,"sbcl")==0) {
      cmds=install_sbcl_full;
    }else {
      printf("%s is not implemented for install.\n",impl);
      exit(EXIT_FAILURE);
    }
    if(argc==1) {
      version=get_default_version(impl);
    }else {
      version=q(argv[1]);
    }
    for(;*cmds&&ret;++cmds) {
      ret=(*cmds)(impl,version);
    }
    s(version);
  }else {
    printf("what would you like to install?\n");
    exit(EXIT_FAILURE);
  }
  return ret;
}
