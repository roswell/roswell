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

  if(argc==1) {
    printf("%s\n",PACKAGE_VERSION);
  }else if (argc==2) {
    char* ev=NULL;
    char* arg1=firsts(nthcdr(1,arg_));
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
      LVal arg=stringlist("--no-rc","-L",DEFAULT_IMPL,"-m","roswell","--eval",cmd,"run",NULL);
      for(;arg;arg=dispatch(arg,&top));
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

struct proc_opt* register_cmd_internal(struct proc_opt* top_) {
  LVal cmds=0;
  top_->command=add_command(top_->command,"roswell-internal-use",NULL,cmd_internal,0,1);
  dispatch_init(&internal,"internal");
  cmds=add_command(cmds,"tar"     ,NULL,cmd_tar,0,1);
  cmds=add_command(cmds,"download",NULL,cmd_download,0,1);
  cmds=add_command(cmds,"uname",NULL,cmd_uname,0,1);
  cmds=add_command(cmds,"which",NULL,cmd_which,0,1);
  cmds=add_command(cmds,"impl",NULL,cmd_impl,0,1);
  cmds=add_command(cmds,"version",NULL,cmd_internal_version,0,1);
  cmds=add_command(cmds,"core-extention",NULL,cmd_internal_core_extention,0,1);
  internal.command=cmds;
  return top_;
}

DEF_SUBCMD(cmd_internal) {
  setup_uid(0);
  return dispatch(nnthcdr(1,arg_),&internal);
}

#define LISPDIR_CANDIDATE(candidate) {    \
    ros_bin_lisp=candidate;               \
    if(directory_exist_p(ros_bin_lisp)) { \
      result=q(ros_bin_lisp);             \
      s(ros_bin);                         \
      return ros_bin_lisp;                \
    }                                     \
    s(ros_bin_lisp);                      \
  }

char* lispdir(void) {
  static char *result=NULL;
  char *w,*ros_bin,*ros_bin_lisp;
  if(result)
    return q(result);

  w=which(argv_orig[0]);
  ros_bin=pathname_directory(truename(w));
  s(w);

  /* $(bindir)/lisp/ */
  LISPDIR_CANDIDATE(cat(ros_bin,"lisp",SLASH,NULL));

  ros_bin[strlen(ros_bin)-1]='\0';
  ros_bin=pathname_directory(ros_bin);

  /* $(bindir)/../lisp/ */
  LISPDIR_CANDIDATE(cat(ros_bin,"lisp",SLASH,NULL));
  /* $(bindir)/../etc/roswell/ */
  LISPDIR_CANDIDATE(cat(ros_bin,"etc"SLASH PACKAGE_STRING SLASH,NULL));
  s(ros_bin);
  result=append_trail_slash(q(LISP_PATH));
  return q(result);
}

DEF_SUBCMD(opt_version) {
  fprintf(stderr,"%s",PACKAGE_STRING);
  if(strlen(ROS_REVISION)>0)
    fprintf(stderr,"(%s)",ROS_REVISION);
  fprintf(stderr,"\n");
  return 0;
}
