/* -*- tab-width : 2 -*- */
#include "opt.h"

LVal internal_commands=(LVal)NULL;

int cmd_download (int argc,char **argv,struct sub_command* cmd) {
  int opt=argc>3?atoi(argv[3]):0;
  return argc>=3?(1&opt?0:fprintf(opt?stdout:stderr,"Downloading %s\n",argv[1])),download_simple(argv[1],argv[2],opt>2?0:opt):0;
}

int cmd_uname (int argc,char **argv,struct sub_command* cmd) {
  if(argc==1) {
    printf("%s\n",uname());
  }else if(argc==2) {
    if(strcmp(argv[1],"-m")==0)
      printf("%s\n",uname_m());
  }
  return 0;
}

int cmd_which (int argc,char **argv,struct sub_command* cmd) {
  if(argc==2) {
    char* str=which(argv[1]);
    printf("%s\n",str);
    s(str);
  }
  return 0;
}

int cmd_impl (int argc,char **argv,struct sub_command* cmd) {
  if(argc==1 && get_opt("default.lisp",0)) {
    char* impl=determin_impl(get_opt("default.lisp",0));
    printf("%s\n",impl);
    s(impl);
  }
  if(argc==2) {
    char* impl=determin_impl(argv[1]);
    printf("%s\n",impl);
    s(impl);
  }
  return 0;
}

int cmd_internal_version (int argc,char **argv,struct sub_command* cmd) {
  printf("%s\n",PACKAGE_VERSION);
  return 0;
}

void register_cmd_internal(void) {
  LVal cmds=internal_commands;
  cmds=add_command(cmds,"tar"     ,NULL,cmd_tar,0,1,NULL,NULL);
  cmds=add_command(cmds,"download",NULL,cmd_download,0,1,NULL,NULL);
  cmds=add_command(cmds,"uname",NULL,cmd_uname,0,1,NULL,NULL);
  cmds=add_command(cmds,"which",NULL,cmd_which,0,1,NULL,NULL);
  cmds=add_command(cmds,"impl",NULL,cmd_impl,0,1,NULL,NULL);
  cmds=add_command(cmds,"version",NULL,cmd_internal_version,0,1,NULL,NULL);
  internal_commands=cmds;
}

int cmd_internal(int argc,char **argv,struct sub_command* cmd) {
  setup_uid(0);
  return proccmd(argc-1,&(argv[1]),(LVal)NULL,internal_commands);
}
