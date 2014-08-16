#include <stdio.h>
#include <stdlib.h>
#include "opt.h"
#include "util.h"

void free_opts(struct opts* opt)
{
  void* tmp;
  while(opt) {
    if(opt->value) {
      dealloc((void*)opt->value);
      opt->value=NULL;
    }
    if(opt->name) {
      dealloc((void*)opt->name);
      opt->name=NULL;
    }
    tmp=opt->next;
    opt->next=NULL;
    tmp=opt;
    opt=opt->next;
    dealloc(tmp);
  }
}

void print_opts(struct opts* opt) 
{
  void* tmp;
  char* typestr;
  while(opt) {
    switch(opt->type) {
    case OPT_INT:
      typestr="int";
      break;
    case OPT_STRING:
      typestr="string";
      break;
    case OPT_BOOL:
      typestr="bool";
      break;
    default:
      typestr="unknown";
    }
    printf("%s=%s[%s]\n",opt->name,opt->value,typestr);
    opt=opt->next;
  }
}

struct opts* load_opts(const char* path) {
  FILE* fp;
  char buf[1024];
  struct opts opt;
  struct opts *cur=&opt;

  if((fp=fopen(path,"r"))==NULL) {
    return NULL;
  }

  while(fgets(buf,1024,fp) !=NULL) {
    int i,mode,last;
    char* str;
    cur->next=(struct opts*)alloc(sizeof(struct opts));
    cur=cur->next;
    cur->type=OPT_VOID;
    cur->value=NULL;
    cur->name=NULL;
    cur->next=NULL;
    for(i=0,mode=0,last=0;i<1024&&buf[i]!='\0';++i) {
      if(buf[i]=='\t'||buf[i]=='\n') {
	switch (mode++) {
	case 0:
	  cur->name=subseq(buf,last,i);
	  break;
	case 1:
	  cur->type=buf[i-1]-'0';
	  break;
	case 2:
	  cur->value=subseq(buf,last,i);
	  break;
	}
	last=i+1;
      }
    }
  }
  fclose (fp);
  return opt.next;
}

int save_opts(const char* path,struct opts* opt) {
  FILE* fp;
  void* tmp;

  if((fp=fopen(path,"w"))==NULL) {
    return 0;
  }

  while(opt) {
    fprintf(fp,"%s\t%d\t%s\n",opt->name,opt->type,opt->value);
    opt=opt->next;
  }
  fclose(fp);
  return 1;
}

int set_opt(struct opts** opts,const char* name,char* value,int type)
{
  int found=0;
  struct opts* opt=*opts;

  while(opt) {
    if(strcmp(opt->name,name)==0) {
      found=1;
      s((char*)opt->value);
      opt->value=remove_char("\n\t",value);
      if(type!=0) {
	opt->type=type;
      }
    }
    opt=opt->next;
  }
  if(!found) {
    opt=(struct opts*)alloc(sizeof(struct opts));
    opt->next=*opts;
    opt->type=type;
    opt->name=(const char*)remove_char("\n\t",(char*)name);
    opt->value=remove_char("\n\t",value);
    *opts=opt;
  }
  return 1;
}

int get_opt_type(struct opts* opt,const char* name)
{
  while(opt) {
    if(strcmp(opt->name,name)==0) {
      return opt->type;
    }
    opt=opt->next;
  } 
  return 0;
}
char* _get_opt(struct opts* opt,const char* name)
{
  while(opt) {
    if(strcmp(opt->name,name)==0) {
      return (char*)opt->value;
    }
    opt=opt->next;
  }
  return NULL;
}

char* get_opt(const char* name)
{
  char* ret=NULL;
  ret=_get_opt(local_opt,name);
  if(!ret) {
    ret=_get_opt(global_opt,name);
  }
  return ret;
}

int unset_opt(struct opts** opts,const char* name)
{
  struct opts *opt=*opts;
  struct opts dummy;
  struct opts *before=&dummy;
  before->next=opt;
  while(opt) {
    if(strcmp(opt->name,name)==0) {
      before->next=opt->next;
      opt->next=NULL;
      free_opts(opt);
      opt=before;
    }
    before=opt;
    opt=opt->next;
  }
  *opts=dummy.next;
  return 1;
}

int set_opts_int(struct opts* opts,const char* name,int value) {
}

int cmd_opt(int argc, const char **argv)
{
  char* home=homedir();
  char* path=cat(home,"config",NULL);
  struct opts* opt=global_opt;
  struct opts** opts=&opt;
  if(argc==0) {
    printf("local:\n");
    print_opts(local_opt);
    printf("global:\n");
    print_opts(opt);
  }else if(strcmp(argv[0],"set")==0) {
    if(argc>=3) {
      set_opt(opts, argv[1],(char*)argv[2],0);
      save_opts(path,opt);
    }
  }else if(strcmp(argv[0],"unset")==0) {
    if(argc>=2) {
      unset_opt(opts, argv[1]);
      save_opts(path,opt);
    }
  }
  s(home);
  s(path);
}
