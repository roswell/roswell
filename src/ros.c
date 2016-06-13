/* -*- tab-width : 2 -*- */
#include "opt.h"

char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

int verbose=0;
int testing=0;
int rc=1;
int quicklisp=1;

struct proc_opt top;

int main(int argc,char **argv) {
  int i;
  char* path=s_cat(configdir(),q("config"),NULL);
  argv_orig=argv;
  argc_orig=argc;

  register_top(&top);

  global_opt=load_opts(path);
  struct opts** opts=&global_opt;
  unset_opt(opts,"program");
  s(path);
  if(argc==1)
    {char* tmp[]={"help"};proc_opt(1,tmp,&top);}
  else
    for(i=1;i<argc;i+=proc_opt(argc-i,&argv[i],&top));
  if(get_opt("program",0))
    {char* tmp[]={"run","-q","--"};proc_opt(3,tmp,&top);}
  free_opts(global_opt);
}
