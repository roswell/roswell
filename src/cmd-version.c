#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#ifdef HAVE_CURL_CURL_H
#  include <curl/curl.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "opt.h"

int cmd_version(int argc,char **argv,struct sub_command* cmd)
{
  fprintf(stderr,"%s",PACKAGE_STRING);
  if(strcmp(argv[0],"--version")!=0) {
#ifdef HAVE_CURL_CURL_H
    fprintf(stderr,"\nlibcurl %s",LIBCURL_VERSION);
#endif
    fprintf(stderr,"\n");
    return 0;
  }
  fprintf(stderr,"\n");
  return 0;
}
