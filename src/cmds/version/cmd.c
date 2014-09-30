#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>

int cmd_version(int argc,char **argv)
{
  fprintf(stderr,"%s\n",PACKAGE_STRING);
  return EXIT_SUCCESS;
}
