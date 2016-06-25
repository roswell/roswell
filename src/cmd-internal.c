/* -*- tab-width : 2 -*- */
#include "opt.h"
#include "cmd-run.h"
#include "gend.h"

struct proc_opt internal;

DEF_SUBCMD(cmd_download) {
  int argc=length(arg_);
  int opt=argc>3?atoi(firsts(nthcdr(3,arg_))):0;
  return argc>=3?
    (1&opt?
     0:
     fprintf(opt?stdout:stderr,"Downloading %s\n",firsts(nthcdr(1,arg_)))),
    download_simple(firsts(nthcdr(1,arg_)),firsts(nthcdr(2,arg_)),opt>2?0:opt):0;
}

DEF_SUBCMD(cmd_uname) {
  int argc=length(arg_);

  if(argc==1) {
    printf("%s\n",uname());
  }else if(argc==2) {
    if(strncmp(firsts(nthcdr(1,arg_)),"-m",2)==0)
      printf("%s\n",uname_m());
  }
  return 0;
}

DEF_SUBCMD(cmd_which) {
  int argc=length(arg_);

  if(argc==2) {
    char* str=which(firsts(nthcdr(1,arg_)));
    printf("%s\n",str);
    s(str);
  }
  return 0;
}

DEF_SUBCMD(cmd_impl) {
  int argc=length(arg_);
  
  if(argc==1 && get_opt("default.lisp",0)) {
    char* impl=determin_impl(get_opt("default.lisp",0));
    printf("%s\n",impl);
    s(impl);
  }
  if(argc==2) {
    char* impl=determin_impl(firsts(nthcdr(1,arg_)));
    printf("%s\n",impl);
    s(impl);
  }
  return 0;
}

DEF_SUBCMD(cmd_internal_version) {
  int argc=length(arg_);
  char* arg1=firsts(nthcdr(1,arg_));

  if(argc==1) {
    printf("%s\n",PACKAGE_VERSION);
  }else if (argc==2) {
    char* ev=NULL;
    if(strcmp(arg1,"date")==0) {
      ev= "date";
    }else if (strcmp(arg1,"lisp")==0) {
      ev= "version";
    }else if (strcmp(arg1,"dump")==0) {
      ev= "roswell";
    }
    if(ev) {
      char *cmd=cat("(progn(format t\"~A~%\"(or(ignore-errors(getf(symbol-value(read-from-string \"ros.util::*version*\")) :",ev,
                    "))(ros:quit 1))) (ros:quit 0))",NULL);
      {char* p[]={"--no-rc"};dispatch(sizeof(p)/sizeof(p[0]),p,&top);}
      {char* p[]={"-L",DEFAULT_IMPL};dispatch(sizeof(p)/sizeof(p[0]),p,&top);}
      {char* p[]={"-m","roswell"};dispatch(sizeof(p)/sizeof(p[0]),p,&top);}
      {char* p[]={"--eval",cmd};dispatch(sizeof(p)/sizeof(p[0]),p,&top);}
      {char* p[]={"run"};dispatch(sizeof(p)/sizeof(p[0]),p,&top);}
      s(cmd);
    }else if(strncmp(arg1,"cc",2)==0) {
      printf("%s\n",ROS_COMPILE_ENVIRONMENT);
    }else if(strncmp(arg1,"curl",4)==0) {
#ifdef HAVE_CURL_CURL_H
      printf("%s\n",LIBCURL_VERSION);
#endif
    }else if(strncmp(arg1,"asdf",4)==0) {
      char *asdf= get_opt("asdf.version",0);
      if(asdf)
        printf("%s\n",asdf);
    }else if(strncmp(arg1,"lispdir",7)==0) {
      printf("%s\n",lispdir());
    }else if(strncmp(arg1,"confdir",7)==0) {
      printf("%s\n",configdir());
    }else if(strcmp(arg1,"package")==0) {
      printf("%s\n",PACKAGE_STRING);
    }else if(strcmp(arg1,"revision")==0) {
      printf("%s\n",ROS_REVISION);
    }else if(strcmp(arg1,"sbcl-bin-version-uri")==0) {
      printf("%s\n",PLATFORM_HTML_URI);
    }else if(strcmp(arg1,"sbcl-bin-uri")==0) {
      printf("%s\n",SBCL_BIN_URI);
    }else
      return 1;
  }
  return 0;
}

char* core_extention(char *impl) {
  if(strncmp("ccl",impl,3)==0) {
    char* binname=get_opt("ccl.bit",0);
    binname = ccl_binname(binname?binname:"");
    return binname;
  }
  return q("core");
}

DEF_SUBCMD(cmd_internal_core_extention) {
  int argc=length(arg_);
  char* arg1=firsts(nthcdr(1,arg_));

  if (argc==2)
    printf("%s\n",core_extention(arg1));
  return 0;
}

void register_cmd_internal(void) {
  dispatch_init(&internal);
  LVal cmds=0;
  cmds=add_command(cmds,"tar"     ,NULL,cmd_tar,0,1);
  cmds=add_command(cmds,"download",NULL,cmd_download,0,1);
  cmds=add_command(cmds,"uname",NULL,cmd_uname,0,1);
  cmds=add_command(cmds,"which",NULL,cmd_which,0,1);
  cmds=add_command(cmds,"impl",NULL,cmd_impl,0,1);
  cmds=add_command(cmds,"version",NULL,cmd_internal_version,0,1);
  cmds=add_command(cmds,"core-extention",NULL,cmd_internal_core_extention,0,1);
  internal.command=cmds;
}

DEF_SUBCMD(cmd_internal) {
  char** argv=firstp(arg_);
  int argc=(int)rest(arg_);
  dealloc((void*)arg_);

  setup_uid(0);
  return dispatch22(array_stringlist(argc-1,&(argv[1])),&internal);
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

DEF_SUBCMD(opt_version) {
  fprintf(stderr,"%s",PACKAGE_STRING);
  if(strlen(ROS_REVISION)>0)
    fprintf(stderr,"(%s)",ROS_REVISION);
  fprintf(stderr,"\n");
  return 0;
}
