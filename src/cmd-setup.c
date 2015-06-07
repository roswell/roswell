#include "opt.h"

extern char** argv_orig;

#define CMD_SETUP_SYSTEM(sys,msg) { \
    fprintf(stderr,"%s",msg);       \
    ret=system(sys);                \
    s(sys);                         \
    if(ret) return ret;             \
  }

int cmd_setup(int argc, const char **argv) {
  char* v=verbose==1?"-v ":(verbose==2?"-v -v ":"");
  int ret=1;
  char* sbcl_bin_version=get_opt("sbcl-bin.version",0);
  if(!sbcl_bin_version) {
    CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install sbcl-bin",NULL),"setting up sbcl-bin\n");
  }else
    fprintf(stderr,"already have sbcl-bin\n");
  CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install quicklisp",NULL),"setting up quicklisp\n");
  CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"roswell-internal-core-build",NULL),"making core for roswell\n");
#ifdef HAVE_WINDOWS_H
  CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install 7zip",NULL),"setting up 7zip\n");
#endif
  return ret;
}
