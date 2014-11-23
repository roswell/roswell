#include <stdio.h>
#include <stdlib.h>
#include "opt.h"
#include "util.h"

extern char** argv_orig;
int cmd_setup(int argc, const char **argv)
{
  char* sys=cat(argv_orig[0]," install sbcl-bin",NULL);
  fprintf(stderr,"setting up sbcl-bin\n");
  system(sys);
  s(sys);
  sys=cat(argv_orig[0]," install quicklisp",NULL);
  fprintf(stderr,"setting up quicklisp\n");
  system(sys);
  s(sys);
  return 0;
}
