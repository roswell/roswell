#include "opt.h"
#include "util.h"

LVal internal_commands=(LVal)NULL;
extern int cmd_tar(int argc,char **argv,struct sub_command* cmd);

int cmd_download (int argc,char **argv,struct sub_command* cmd) {
  if(argc>=2) {
    fprintf(stderr,"download %s %s",argv[1],argv[2]);
    return download_simple(argv[1],argv[2],1);
  }
  return 0;
}

int cmd_uname (int argc,char **argv,struct sub_command* cmd) {
  if(argc==1) {
    printf("%s\n",uname());
  }else if(argc==2) {
    if(strcmp(argv[1],"-m")==0) {
      printf("%s\n",uname_m());
    }
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

void register_cmd_internal(void) {
  LVal cmds=internal_commands;
  cmds=add_command(cmds,"tar"     ,NULL,cmd_tar,0,1,NULL,NULL);
  cmds=add_command(cmds,"download",NULL,cmd_download,0,1,NULL,NULL);
  cmds=add_command(cmds,"uname",NULL,cmd_uname,0,1,NULL,NULL);
  cmds=add_command(cmds,"which",NULL,cmd_which,0,1,NULL,NULL);
  internal_commands=cmds;
}

int cmd_internal(int argc,char **argv,struct sub_command* cmd) {
  return proccmd(argc-1,&(argv[1]),(LVal)NULL,internal_commands);
}
