#include "opt.h"

extern char** argv_orig;
int cmd_setup(int argc, const char **argv) {
  char* v="";
  int ret=1;
  if(verbose==1)
    v="-v ";
  if(verbose==2)
    v="-v -v ";
  char* sbcl_bin_version=get_opt("sbcl-bin.version",0);
  char* sys=cat(argv_orig[0]," ",v,"install sbcl-bin",NULL);
  fprintf(stderr,"setting up sbcl-bin\n");
  if(!sbcl_bin_version)
    ret=system(sys);
  else fprintf(stderr,"already have sbcl-bin\n"),ret=0;
  s(sys);
  if(ret)
    return ret;
  sys=cat(argv_orig[0]," ",v,"install quicklisp",NULL);
  fprintf(stderr,"setting up quicklisp\n");
  ret=system(sys);
  s(sys);
  if(ret)
    return ret;
#ifdef _WIN32
  sys=cat(argv_orig[0]," ",v,"install 7zip",NULL);
  fprintf(stderr,"setting up 7zip\n");
  ret=system(sys);
  s(sys);
#endif
  return ret;
}
