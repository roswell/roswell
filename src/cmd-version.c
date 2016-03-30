/* -*- tab-width : 2 -*- */
#include "gend.h"
#include "opt.h"

int cmd_version(int argc,char **argv,struct sub_command* cmd) {
  char *asdf= get_opt("asdf.version",0);
  fprintf(stderr,"%s",PACKAGE_STRING);
  if(strlen(ROS_REVISION)>0)
    fprintf(stderr,"(%s)",ROS_REVISION);
  if(strcmp(argv[0],"--version")!=0) {
    char *c= configdir(),*l=lispdir();
    fprintf(stderr,"\nbuild with %s",ROS_COMPILE_ENVIRONMENT);
#ifdef HAVE_CURL_CURL_H
    fprintf(stderr,"\nlibcurl %s",LIBCURL_VERSION);
#endif
    if(asdf)
      fprintf(stderr,"\nASDF %s",asdf);
    fprintf(stderr,"\nlispdir='%s'",l),s(l);
    if(c)
      fprintf(stderr,"\nconfigdir='%s'",c),s(c);
  }
  fprintf(stderr,"\n");
  return 0;
}

