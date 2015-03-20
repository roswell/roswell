#include "gend.h"
#include "opt.h"

int cmd_version(int argc,char **argv,struct sub_command* cmd) {
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

char* lispdir(void) {
  char *ros_bin=pathname_directory(truename(which(argv_orig[0])));
  char* ros_bin_lisp=cat(ros_bin,"lisp",SLASH,NULL);
  char* lisp_path;
  s(ros_bin);
  if(directory_exist_p(ros_bin_lisp)) {
    lisp_path=ros_bin_lisp;
  }else {
    s(ros_bin_lisp);
#if defined(WIN_LISP_PATH)
    lisp_path=q(WIN_LISP_PATH);
#else
# ifdef LISP_PATH
    lisp_path=q(LISP_PATH);
# else
    lisp_path=cat(".",SLASH,NULL);
# endif
#endif
    lisp_path=append_trail_slash(lisp_path);
  }
  return lisp_path;
}
