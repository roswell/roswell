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
    CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install sbcl-bin",NULL),"Installing sbcl-bin...\n");
  }else
    fprintf(stderr,"Already have sbcl-bin.\n");
  CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install quicklisp",NULL),"Installing Quicklisp...\n");
#ifdef HAVE_WINDOWS_H
  CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"install 7zip",NULL),"Installing 7zip...\n");
#else /* until fix it for windows*/
  if(argc==1)
    CMD_SETUP_SYSTEM(cat(argv_orig[0]," ",v,"roswell-internal-core-build",NULL),"Making core for Roswell...\n");
#endif
  return ret;
}
