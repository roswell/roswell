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

char* lispdir(void) {
  char *w=which(argv_orig[0]);
  char *ros_bin=pathname_directory(truename(w));
  char* ros_bin_lisp=cat(ros_bin,"lisp",SLASH,NULL);
  char* lisp_path=NULL;
  s(ros_bin),s(w);
  if(directory_exist_p(ros_bin_lisp)) {
    lisp_path=ros_bin_lisp;
  }else {
    s(ros_bin_lisp);
#if defined(WIN_LISP_PATH)
    if(lisp_path==NULL)
      lisp_path=q(WIN_LISP_PATH);
#endif
    if(lisp_path==NULL)
      lisp_path=q(LISP_PATH);
    lisp_path=append_trail_slash(lisp_path);
  }
  return lisp_path;
}
