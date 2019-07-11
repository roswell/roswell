#include "opt.h"
#include "cmd-run.h"
DEF_SUBCMD(cmd_run_star);

DEF_SUBCMD(cmd_script) {
  cond_printf(1,"cmd_script\n");
  int argc=length(arg_);
  char* arg0=firsts(arg_);

  char* current=get_opt("program",0);
  cond_printf(1,"script_%s:argc=%d argv[0]=%s\n",cmd->name,argc,arg0);
  cond_printf(1,"current=%s\n",current);
  if(argc==1 && !current &&
     strcmp(arg0,"--")==0)
    return dispatch(stringlist("help","--",NULL),&top);
  else {
    char* result=q("");
    LVal i=strcmp(arg0,"--")==0?rest(arg_):arg_;
    for (;i;i=rest(i)) {
      char* val=escape_string(firsts(i));
      result=cat(result,"\"",val,"\"",NULL);
      s(val);
    }
    set_opt(&local_opt,"script",result);
    s(result);
    cmd_run_star(stringlist("script",NULL),cmd);
  }
  return 0;
}

static int script_frontend_sentinel=0;

DEF_SUBCMD(cmd_script_frontend) {
  int argc=length(arg_);

  FILE* in;
  char buf[800];
  int i=0,j,c;
  int argc_;
  char** argv_;
  char** argv_gen;
  LVal arg;
  struct opts* opt;
  cond_printf(1,"cmd_script_frontend:%d\n",script_frontend_sentinel);
  if(script_frontend_sentinel)
    return cmd_script(arg_,cmd);
  script_frontend_sentinel=1;
  if(strcmp(firsts(arg_),"--")==0)
    arg_=nnthcdr(1,arg_),argc--;
  if (argc == 0) return -1;
  cond_printf(1,"frontend:script_%s:argc=%d argv[0]=%s\n",cmd->name,argc,firsts(arg_));

  for(opt=local_opt;opt;opt=opt->next)
    if(strcmp(opt->name,"lisp")==0)
      opt->name=s_cat(q("*"),opt->name,NULL);
  if((in=fopen(firsts(arg_),"rb"))!=NULL) {
    if(fgetc(in)!='#'||fgetc(in)!='!') {
      fclose(in);
      cmd_script(arg_,cmd);
    }
    for(i=0;i<3;++i)
      while((c=fgetc(in))!=EOF && c!='\n');
    for(i=0;(c=fgetc(in))!=EOF;buf[i++]=c)
      if(c=='\r'||c=='\n'||i==799)
        break;
    fclose(in);
  }
  buf[i]='\0';
  cond_printf(1,"ros_script_cmd=%s\n",buf);
  argv_=parse_cmdline(buf,&argc_);
  argv_gen=alloc(sizeof(char**)*(argc+argc_));
  for(i=0;i<argc_-2&&strcmp(argv_[i+2],"$0")!=0;++i)
    argv_gen[i]=argv_[i+2];
  for(j=i;i<j+argc;++i)
    argv_gen[i]=firsts(nthcdr(i-j,arg_));
  j=i;
  for(arg=array_stringlist(j,argv_gen);arg;arg=dispatch(arg,&top));
  return 0;
}
