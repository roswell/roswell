#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "ros_install.h"
#include "opt.h"

int quicklisp_start(struct install_options* param)
{
  char* home=configdir();
  param->version=NULL;
  param->arch_in_archive_name=0;
  //param->expand_path=cat(home,"src",SLASH,param->impl,SLASH,NULL);
  s(home);
  return 1;
}

char* quicklisp_uri(struct install_options* param)
{
  return q("http://beta.quicklisp.org/quicklisp.lisp");
}

char* quicklisp_extention(struct install_options* param)
{
  return ".lisp";
}

int quicklisp_setup(struct install_options* param)
{
  char* home=configdir();
  char* lisp_path=s_cat(lispdir(),q("install"),q(SLASH),q("quicklisp.lisp"),NULL);
  char* archive=s_cat(q(home),q("archives"),q(SLASH),q("quicklisp.lisp"),NULL);
  char* exe_path=truename(argv_orig[0]);
  char* install_path=cat(q(home),"impls",SLASH,"ALL",SLASH,"ALL",SLASH,param->impl,SLASH,NULL);
  {char* p[]={"--no-rc"};proccmd(sizeof(p)/sizeof(p[0]),p,top_options,top_commands);}
  {char* p[]={"-l",archive};proccmd(sizeof(p)/sizeof(p[0]),p,top_options,top_commands);}
  {char* p[]={"--",lisp_path,exe_path,install_path};proccmd(sizeof(p)/sizeof(p[0]),p,top_options,top_commands);}
  s(archive),s(lisp_path),s(exe_path),s(home),s(install_path);
  return 1;
}

install_cmds install_quicklisp_full[]= {
  quicklisp_start,
  start,
  download,
  quicklisp_setup,
  NULL
};

struct install_impls utils_quicklisp={ "quicklisp", install_quicklisp_full,quicklisp_uri,quicklisp_extention,1};
