#include "gend.h"
#include "opt.h"
#include "util.h"

int cmd_version(int argc,char **argv,struct sub_command* cmd)
{
  fprintf(stderr,"%s",PACKAGE_STRING);
  if(strlen(ROS_REVISION)>0)
    fprintf(stderr,"(%s)",ROS_REVISION);
  if(strcmp(argv[0],"--version")!=0) {
    fprintf(stderr,"\nbuild with %s",ROS_COMPILE_ENVIRONMENT);
#ifdef HAVE_CURL_CURL_H
    fprintf(stderr,"\nlibcurl %s",LIBCURL_VERSION);
#endif
    fprintf(stderr,"\n");
    return 0;
  }
  fprintf(stderr,"\n");
  return 0;
}
