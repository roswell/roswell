/* -*- tab-width : 2 -*- */
#include "opt.h"

int cmd_config(int argc,char **argv,struct sub_command* cmd) {
  char* home=configdir();
  char* path=cat(home,"config",NULL);
  if(argc==1) {
    fprintf(stderr,"local:\n");
    print_opts(global_opt);
    fprintf(stderr,"\nPossible subcommands:\n");
    fflush(stderr);
    printf("set\n");
    printf("show\n");
  }else {
    struct opts* opt=global_opt;
    struct opts** opts=&opt;
    // TBD parse options
    if(argc==2) {
      unset_opt(opts, argv[1]);
      save_opts(path,opt);
    }else {
      if(strcmp(argv[1],"set")==0) {
        set_opt(opts, argv[2],(char*)argv[3],0);
        save_opts(path,opt);
      }else if (strcmp(argv[1],"show")==0) {
        fprintf(stdout,"%s\n",_get_opt(opt,argv[2]));
      }else {
        set_opt(opts, argv[1],(char*)argv[2],0);
        save_opts(path,opt);
      }
    }
  }
  s(home),s(path);
  return 0;
}
int config_help(int argc,char **argv,struct sub_command* cmd) {
  if(argc==1) {
    cond_printf(0,
                "Usage: %s config              show all variables and it's value.\n"
                "Usage: %s config set var val  set variable.\n"
                "Usage: %s config show var     show a variable value.\n\n"
                ,argv_orig[0],argv_orig[0],argv_orig[0]);
  }
  return 0;
}

void register_cmd_config(void) {
  top_commands=add_command(top_commands,"config"  ,NULL,cmd_config,1,1,NULL,NULL);
  top_helps=add_help(top_helps,"config","",(LVal)NULL,(LVal)NULL,NULL,NULL,config_help);
}
