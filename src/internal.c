#include "opt.h"
#include "util.h"

LVal internal_commands=(LVal)NULL;
extern int cmd_download(int argc,char **argv,struct sub_command* cmd);
extern int cmd_tar(int argc,char **argv,struct sub_command* cmd);

void register_cmd_internal(void) {
  LVal cmds=internal_commands;
  cmds=add_command(cmds,"tar"     ,NULL,cmd_tar,0,1,NULL,NULL);
  cmds=add_command(cmds,"download",NULL,cmd_download,0,1,NULL,NULL);
  internal_commands=cmds;
}

int cmd_internal(int argc,char **argv,struct sub_command* cmd)
{
  return proccmd(argc-1,&(argv[1]),(LVal)NULL,internal_commands);
}
