#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "opt.h"

LVal impl_filter=0;

void initialize_impl_filter(void) {
  if(impl_filter)
    sL(impl_filter);
}

LVal filter_impl_fromdir(LVal v) {
  char* str=firsts(v);
  LVal spl;
  if(strcmp(str,"../")==0)
    return 0;
  if(strcmp(str,"./")==0)
    return 0;
  if(strncmp(str,"sbcl",4)==0) {
    spl=split_string(str,"-");
    if(Number(length(spl))==2) {
      if(!find((LVal)"sbcl",impl_filter,string_equal))
	impl_filter=conss(q("sbcl"),impl_filter);
    }else {
      if(!find((LVal)"sbcl-bin",impl_filter,string_equal))
	impl_filter=conss(q("sbcl-bin"),impl_filter);
    }
    sL(spl);
    return 1;
  }
  return 0;
}

int list_impls(int argc,char **argv,struct sub_command* cmd)
{
  char* impls=s_cat2(homedir(),q("impls/"));
  LVal dirs=nreverse(directory(impls)); //sort?
  initialize_impl_filter();
  sL(remove_if_not1(filter_impl_fromdir,dirs));
  sL(dirs);
  print_list(impl_filter);
  s(impls);
}

LVal impl_versions=0;
char* filter_impl=NULL;

LVal filter_versions_fromdir(LVal v) {
  char* str=firsts(v);
  int len=strlen((char*)str);
  int c;
  int i;
  char* sub=subseq(str,0,4);
  if(strcmp(sub,filter_impl)==0) {
    s(sub);
  }
  for(i=0,c=0;str[i]!='\0';++i)
    if(str[i]=='-')
      ++c;
  return (c>1);
}

int list_versions(int argc,char **argv,struct sub_command* cmd)
{
  char* sub=subseq(argv[1],-4,0);
  if(strcmp(sub,"-bin")==0) {
    char* impls=s_cat2(homedir(),q("impls/"));
    LVal dirs=nreverse(directory(impls)); //sort?
    LVal out2=remove_if_not1(filter_impl_fromdir,dirs);
    LVal out;
    
    filter_impl=argv[1];
    out=remove_if_not1(filter_versions_fromdir,out2);
    sL(dirs);
    print_list(out);
    sL(out2);
    sL(out);
    s(impls);
  }else{
    
  }
  s(sub);
}

static struct sub_command list_commands[] = {
  { "installed", NULL,list_impls},
  { "sbcl", NULL,list_versions},
  { "sbcl-bin", NULL,list_versions},
  { "ccl", NULL,list_versions},
};

int cmd_list(int argc,char **argv)
{
  struct sub_command* j;
  int ret=1,k,i,found=0;
  if(argc!=1) {
    for(i=1;i<sizeof(list_commands)/sizeof(struct sub_command);++i) {
      j = &list_commands[i];
      if(strcmp(argv[1],j->name)==0) {
	found=1;
	j->call(argc,argv,j);
	break;
      }
    }
  }else {
    char* str = file_namestring(q(argv_orig[0]));
    printf("usage %s %s\n\n",str,argv[0]);
    s(str);
    for(i=0;i<sizeof(list_commands)/sizeof(struct sub_command);++i) {
      j = &list_commands[i];
      printf("%s\n",j->name);
    }
    return(EXIT_SUCCESS);
  }
  return(EXIT_SUCCESS);
}
