#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>

int cmd_version(int argc,char **argv)
{
  printf("%s\n",PACKAGE_STRING);
  return 0;
}
