#include <stdio.h>
#include <stdlib.h>
#include <archive.h>
#include <archive_entry.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "opt.h"
#include "util.h"
#include "ros_install.h"
static int in_resume=0;
static char *flags =NULL;
struct install_impls *install_impl;

struct install_impls *impls_to_install[]={
  &impls_sbcl_bin,
  &utils_quicklisp
};

extern int extract(const char *filename, int do_extract, int flags,const char* outputpath,Function2 f,void* p);

int installed_p(struct install_options* param)
{
  int ret;
  char* i;
  char *impl;

  impl=q(param->impl);
  //TBD for util.
  i=s_cat(homedir(),q("impls"),q(SLASH),q(param->arch),q(SLASH),q(param->os),q(SLASH),
          q(impl),q(param->version?SLASH:""),q(param->version?param->version:""),q(SLASH),NULL);
  ret=directory_exist_p(i);
  if(verbose>0) {
    fprintf(stderr,"directory_exist_p(%s)=%d\n",i,ret);
  }
  s(i),s(impl);
  return ret;
}

int install_running_p(struct install_options* param)
{
  /* TBD */
  return 0;
}

int start(struct install_options* param)
{
  char* home= homedir();
  char* p;
  ensure_directories_exist(home);
  if(installed_p(param)) {
    printf("%s/%s are already installed.if you intend to reinstall by (TBD).\n",param->impl,param->version?param->version:"");
    return 0;
  }
  if(install_running_p(param)) {
    printf("It seems running installation process for $1/$2.\n");
    return 0;
  }
  p=cat(home,"tmp",SLASH,param->impl,param->version?"-":"",param->version?param->version:"",SLASH,NULL);
  ensure_directories_exist(p);
  s(p);

  p=cat(home,"tmp",SLASH,param->impl,param->version?"-":"",param->version?param->version:"",".lock",NULL);
  setup_signal_handler(p);
  touch(p);

  s(p);
  s(home);
  return 1;
}

char* download_archive_name(struct install_options* param)
{
  char* ret=cat(param->impl,param->version?"-":"",param->version?param->version:"",NULL);
  if(param->arch_in_archive_name==0) {
    ret=s_cat(ret,q("."),q((*(install_impl->extention))(param)),NULL);
  }else {
    ret=s_cat(ret,cat("-",param->arch,"-",param->os,".",q((*(install_impl->extention))(param)),NULL),NULL);
  }
  return ret;
}

int download(struct install_options* param)
{
  char* home= homedir();
  char* url=(*(install_impl->uri))(param);
  char* archive_name=download_archive_name(param);
  char* impl_archive=cat(home,"archives",SLASH,archive_name,NULL);
  if(!file_exist_p(impl_archive)
     || get_opt("download.force")) {
    printf("Downloading archive.:%s\n",url);
    /*TBD proxy support... etc*/
    if(url) {
      ensure_directories_exist(impl_archive);
      if(download_simple(url,impl_archive,0)) {
	printf("Failed to Download.\n");
	return 0;
	/* fail */
      }else{
	printf("\ndone\n");
      }
      s(url);
    }
  } else {
    printf("Skip downloading %s\n",url);
  }
  s(impl_archive);
  s(home);
  return 1;
}

LVal expand_callback(LVal v1,LVal v2) {
  char* path=(char*)v1;
  struct install_options* param=(struct install_options*)v2;
  if(param->arch_in_archive_name) {
    int pos=position_char("/",path);
    if(pos!=-1) {
      return (LVal)s_cat(cat(param->impl,"-",param->version?param->version:"","-",param->arch,"-",param->os,NULL),subseq(path,pos,0),NULL);
    }
  }
  return (LVal)q(path);
}

int expand(struct install_options* param)
{
  char* home= homedir();
  char* argv[5]={"-xf",NULL,"-C",NULL,NULL};
  int argc=4;
  int pos;
  char* archive;
  char* dist_path;
  char *impl,*version;
  archive=download_archive_name(param);
  pos=position_char("-",param->impl);
  if(pos!=-1) {
    impl=subseq(param->impl,0,pos);
  }else
    impl=q(param->impl);
  version=q(param->version);
  if(!param->expand_path)
    param->expand_path=cat(home,"src",SLASH,impl,"-",version,SLASH,NULL);
  dist_path=param->expand_path;
  printf("Extracting archive. %s to %s\n",archive,dist_path);
  
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);

  /* TBD log output */
  argv[1]=cat(home,"archives",SLASH,archive,NULL);
  argv[3]=cat(home,"src",SLASH,NULL);
  extract(argv[1], 1, ARCHIVE_EXTRACT_TIME,argv[3],expand_callback,param);

  s(argv[1]),s(argv[3]);
  s(impl);
  s(archive);
  s(home);
  s(version);
  return 1;
}

int cmd_install(int argc,char **argv,struct sub_command* cmd)
{
  int ret=1,k;
  install_cmds *cmds=NULL;
  struct install_options param;
  param.os=uname();
  param.arch=uname_m();
  param.arch_in_archive_name=0;
  param.expand_path=NULL;
  if(argc!=1) {
    for(k=1;k<argc;++k) {
      char* version_arg=NULL;
      int i,pos;
      param.impl=argv[k];
      pos=position_char("/",param.impl);
      if(pos!=-1) {
	param.version=subseq(param.impl,pos+1,0);
	param.impl=subseq(param.impl,0,pos);
      }else {
        param.version=NULL;
	param.impl=q(param.impl);
      }

      for(install_impl=NULL,i=0;i<sizeof(impls_to_install)/sizeof(struct install_impls*);++i) {
	struct install_impls* j=impls_to_install[i];
	if(strcmp(param.impl,j->name)==0) {
	  install_impl=j;
	}
      }
      if(!install_impl) {
        char* lisp_path=lispdir();
        int i,j,argc_;
        char** tmp;
        char* install_ros=s_cat2(lisp_path,q("install.ros"));
        if(verbose>0) {
          fprintf(stderr,"%s is not implemented for install. %s argc:%d\n",param.impl,install_ros,argc);
          for(i=0;i<argc;++i)
            fprintf(stderr,"%s:",argv[i]);
          fprintf(stderr,"\n");
        }
        tmp=(char**)alloc(sizeof(char*)*(argc+9));
        i=0;
        tmp[i++]=q("--no-rc");
        tmp[i++]=q("lisp=sbcl-bin");
        tmp[i++]=q("--");
        tmp[i++]=install_ros;
        tmp[i++]=q("install");
        tmp[i++]=q(argv[1]);
        tmp[i++]=sexp_opts(local_opt);
        tmp[i++]=sexp_opts(global_opt);
        tmp[i++]=homedir();
        tmp[i++]=truename(argv_orig[0]);
        for(j=2;j<argc;tmp[i++]=q(argv[j++]));
        argc_=i;
        for(i=0;i<argc_;i+=proccmd(argc_-i,&tmp[i],top_options,top_commands));
        for(j=0;j<argc_;s(tmp[j++]));
        dealloc(tmp);
        return 0;
      }
      for(cmds=install_impl->call;*cmds&&ret;++cmds)
	ret=(*cmds)(&param);
      if(ret) { // after install latest installed impl/version should be default for 'run'
        struct opts* opt=global_opt;
        struct opts** opts=&opt;
        int i;
        char* home=homedir();
        char* path=cat(home,"config",NULL);
        char* v=cat(param.impl,".version",NULL);
        char* version=param.version;
        if(!install_impl->util){
          for(i=0;version[i]!='\0';++i)
            if(version[i]=='-')
              version[i]='\0';
          set_opt(opts,"default.lisp",param.impl,0);
          set_opt(opts,v,version,0);
          save_opts(path,opt);
        }
        s(home),s(path),s(v);
      }
      if(param.version)s(param.version);
      s(param.impl),s(param.arch),s(param.os);
      s(param.expand_path);
    }
  }else {
    char* tmp[]={"help","install"};
    proccmd(2,tmp,top_options,top_commands);
    exit(EXIT_FAILURE);
  }
  return ret;
}

int install_help(int argc,char **argv,struct sub_command* cmd)
{
  int i;
  if(argc==1) {
    char* lisp_path=lispdir();
    fprintf(stderr,"Candidates to install are:\nsbcl-bin\n");
    char* install=s_cat2(lisp_path,q("install/"));
    LVal d=directory(install);
    {
      LVal v=d;
      for(;v;v=Next(v)) {
        char* str=firsts(v);
        if(str[strlen(str)-1]!='/') {
          int p=position_char(".",str);
          if(p!=-1) {
            char *sub=subseq(str,0,p);
            printf("%s\n",sub);
            s(sub);
          }
        }
      }
    }
    sL(d);
  }else if(argc==2) {
        char* lisp_path=lispdir();
        int i,j,argc_;
        char** tmp;
        char* install_ros=s_cat2(lisp_path,q("install.ros"));
        tmp=(char**)alloc(sizeof(char*)*(argc+9));
        i=0;
        tmp[i++]=q("--no-rc");
        tmp[i++]=q("lisp=sbcl-bin");
        tmp[i++]=q("--");
        tmp[i++]=install_ros;
        tmp[i++]=q("help");
        tmp[i++]=q(argv[1]);
        tmp[i++]=sexp_opts(local_opt);
        tmp[i++]=sexp_opts(global_opt);
        tmp[i++]=homedir();
        tmp[i++]=truename(argv_orig[0]);
        for(j=2;j<argc;tmp[i++]=q(argv[j++]));
        argc_=i;
        for(i=0;i<argc_;i+=proccmd(argc_-i,&tmp[i],top_options,top_commands));
        for(j=0;j<argc_;s(tmp[j++]));
        dealloc(tmp);
        return 0;
  }
  return 0;
}

void register_cmd_install(void)
{
  top_commands=add_command(top_commands,"install"    ,NULL,cmd_install,1,1,"Install archive and build it for "PACKAGE" environment",NULL);
  top_helps=add_help(top_helps,"install",q(""),(LVal)NULL,(LVal)NULL,NULL,NULL,install_help);
}
