/* -*- tab-width : 2 -*- */
#include "opt.h"

void free_opts(struct opts* opt) {
  while(opt) {
    void* tmp=opt->next;
    if(opt->value) {
      dealloc((void*)opt->value);
      opt->value=NULL;
    }
    if(opt->name) {
      dealloc((void*)opt->name);
      opt->name=NULL;
    }

    opt->next=NULL;
    tmp=opt;
    opt=opt->next;
    dealloc(tmp);
  }
}

char* sexp_opts(struct opts* opt) {
  void* ret=q("");
  for(;opt;opt=opt->next)
    ret=s_cat(ret,q("(\""),q(opt->name),q("\"\""),escape_string((char*)opt->value),q("\")"),NULL);
  return ret;
}

struct opts* load_opts(const char* path) {
  FILE* fp;
  char buf[1024];
  struct opts opt;
  struct opts *cur=&opt;
  opt.next=NULL;

  if((fp=fopen(path,"r"))==NULL)
    return NULL;

  while(fgets(buf,1024,fp) !=NULL) {
    int i,mode,last;
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

  if((fp=fopen(path,"w"))==NULL)
    return 0;

  while(opt) {
    fprintf(fp,"%s\t%d\t%s\n",opt->name,opt->type,opt->value);
    opt=opt->next;
  }
  fclose(fp);
  return 1;
}

int set_opt(struct opts** opts,const char* name,char* value) {
  int found=0;
  struct opts* opt=*opts;

  while(opt) {
    if(strcmp(opt->name,name)==0) {
      found=1;
      opt->value=remove_char("\n\t",value);
    }
    opt=opt->next;
  }
  if(!found) {
    opt=(struct opts*)alloc(sizeof(struct opts));
    opt->next=*opts;
    opt->type=0;
    opt->name=(const char*)remove_char("\n\t",(char*)name);
    opt->value=remove_char("\n\t",value);
    *opts=opt;
  }
  return 1;
}

char* _get_opt(struct opts* opt,const char* name) {
  for(;opt;opt=opt->next)
    if(strcmp(opt->name,name)==0)
      return (char*)opt->value;
  return NULL;
}

char* _getenv(const char* name) {
  char* name_=substitute_char('_','.',q(name));
  char* ret=getenv(name_);
  s(name_);
  return ret;
}

char* get_opt(const char* name,int env) {
  char* ret=NULL;
  if(env)ret=_getenv(name);
  if(!ret) {
    ret=_get_opt(local_opt,name);
    if(!ret)
      ret=_get_opt(global_opt,name);
  }
  return ret;
}

int unset_opt(struct opts** opts,const char* name) {
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

LVal add_command(LVal cmd,const char* name,const char* short_name,sub_command_fnc call,int show_opt,int terminating) {
  struct sub_command* p=alloc(sizeof(struct sub_command));
  p->name=name?q(name):NULL;
  p->short_name=short_name?q(short_name):NULL;
  p->call=call;
  p->show_opt=show_opt;
  p->terminating=terminating;
  cmd=cons(p,cmd);
  return cmd;
}
