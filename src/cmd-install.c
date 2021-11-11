#include "opt.h"
#include "cmd-install.h"
static int in_resume=0;
static char *flags =NULL;
struct install_impls *install_impl;
struct install_impls *impls_to_install[]={&impls_sbcl_bin};

extern int extract(const char *filename, int do_extract, int flags,const char* outputpath,Function2 f,void* p);

int installed_p(struct install_options* param) {
  int ret;
  char *i,*impl=q(param->impl);
  i=s_cat(configdir(),q("impls"),q(DIRSEP),q(param->arch),q(DIRSEP),q(param->os),q(DIRSEP),
          q(impl),q(param->version?DIRSEP:""),q(param->version?param->version:""),q(DIRSEP),NULL);
  ret=directory_exist_p(i);
  cond_printf(1,"directory_exist_p(%s)=%d\n",i,ret);
  s(i),s(impl);
  return ret;
}

int start(struct install_options* param) {
  char *home=configdir(),*p;
  char *localprojects=cat(home,"local-projects/",NULL);
  setup_uid(1);
  ensure_directories_exist(localprojects);
  s(localprojects);
  if(installed_p(param)) {
    printf("%s/%s is already installed.\n",param->impl,param->version?param->version:"");
    if(param->version)
      set_defaultlisp(param->impl,param->version);
    exit(EXIT_SUCCESS);
  }
  p=cat(home,"tmp",DIRSEP,param->impl,param->version?"-":"",param->version?param->version:"",DIRSEP,NULL);
  ensure_directories_exist(p);
  s(p);

  p=cat(home,"tmp",DIRSEP,param->impl,param->version?"-":"",param->version?param->version:"",".lock",NULL);
  delete_at_exit(p);
  touch(p);

  s(p),s(home);
  return 1;
}

char* download_archive_name(struct install_options* param) {
  return s_cat(q(param->impl),
               param->version?cat("-",param->version,NULL):q(""),
               param->arch_in_archive_name?cat("-",param->arch,"-",param->os,NULL):q(""),
               q((*(install_impl->extention))(param)),NULL);
}

int download(struct install_options* param) {
  char* home=configdir();
  char* url=install_impl->uri;
  char* archive_name=download_archive_name(param);
  char* impl_archive=cat(home,"archives",DIRSEP,archive_name,NULL);
  if(!file_exist_p(impl_archive)
     || get_opt("download.force",1)) {
    printf("Downloading %s\n",url);
    if(url) {
      ensure_directories_exist(impl_archive);
      int status = download_simple(url,impl_archive,0);
      if(status) {
        printf("Download Failed with status %d. See download_simple in src/download.c\n", status);
        return 0; /* fail */
      }
      s(url);
    }
  } else printf("Skip downloading %s\n",url);
  s(impl_archive),s(home);
  return 1;
}

DEF_SUBCMD(cmd_install) {
  int argc=length(arg_);

  cond_printf(1,"cmd_install:\n");
  install_cmds *cmds=NULL;
  struct install_options param;
  int ret=1,k;
  char* variant= get_opt("variant",0);
  param.os=uname_s();
  param.arch=uname_m();
  param.variant=variant?cat("-",variant,NULL):q(SBCL_BIN_VARIANT);
  param.arch_in_archive_name=0;
  param.version_not_specified=1;
  param.expand_path=NULL;

  if(argc==1) {
    dispatch(stringlist("help","install",NULL),&top);
    exit(EXIT_SUCCESS);
  }
  for(k=1;k<argc;++k) {
    int i,pos;
    param.impl=firsts(nthcdr(k,arg_));
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
    if(install_impl) {
      for(cmds=install_impl->call;*cmds&&ret;++cmds)
        ret=(*cmds)(&param);
      if(ret)
        set_defaultlisp(param.impl,param.version);
      else
        exit(EXIT_FAILURE);
      cond_printf(1,"done with install impl \n");
    }
    {
      int i,j;
      LVal tmp=0;
      char* install_ros=s_cat2(lispdir(),q("install.ros"));
      cond_printf(1,"%s \n",install_ros);
      if(verbose&1) {
        cond_printf(1,"%s has not been implemented internally yet. %s argc:%d\n",param.impl,install_ros,argc);
        for(i=0;i<argc;++i)
          cond_printf(1,"%s:",firsts(nthcdr(i,arg_)));
        cond_printf(1,"\n");
      }
      tmp=conss(q("--"),tmp);
      tmp=conss(install_ros,tmp);
      tmp=conss(q(firsts(nthcdr(1,arg_))),tmp);
      for(j=2;j<argc;tmp=conss(q(firsts(nthcdr(j++,arg_))),tmp));
      tmp=nreverse(tmp);
      if(verbose&1) {
        int j,argc_=length(tmp);
        cond_printf(1,"argc_=%d ",argc_);
        for(j=0;j<argc_;++j)
          cond_printf(1,"argv[%d]=%s,",j,firsts(nthcdr(j,tmp)));
      }
      for(;tmp;tmp=dispatch(tmp,&top));
      return 0;
    }
    if(param.version)
      s(param.version);
    s(param.impl),s(param.arch),s(param.os),s(param.variant);
    s(param.expand_path);
  }
  return 0;
}

struct proc_opt* register_cmd_install(struct proc_opt* top) {
  top->command=add_command(top->command,"install"    ,NULL,cmd_install,1,1);
  return top;
}

int set_defaultlisp (char* impl,char* version) {
  struct opts* opt=global_opt;
  struct opts** opts=&opt;
  char* home=configdir();
  char* path=cat(home,"config",NULL);
  char* v=cat(impl,".version",NULL);
  int i;
  cond_printf(1,"impl %s version= %s \n",impl,version);
  set_opt(opts,"default.lisp",impl);
  set_opt(opts,v,version);
  save_opts(path,opt);
  global_opt=opt;
  s(home),s(path),s(v);
  return 1;
}
