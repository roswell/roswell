#include "opt.h"

char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

struct proc_opt top;

#ifndef ROSWELL_HTML_TEST
int main(int argc,char **argv) {
  int i;
  char* path=s_cat(configdir(),q("config"),NULL);
  argv_orig=argv;
  argc_orig=argc;

  lispdir();
  register_top(&top);

  global_opt=load_opts(path);
  struct opts** opts=&global_opt;
  unset_opt(opts,"program");
  s(path);
  LVal arg;
  if(argc==1)
    dispatch(stringlist("help",NULL),&top);
  else
    for(arg=array_stringlist(argc-1,argv+1);arg;arg=dispatch(arg,&top));
  if(get_opt("program",0))
    dispatch(stringlist("run","--hook","--quit","--",NULL),&top);
  free_opts(global_opt);
}
#endif
