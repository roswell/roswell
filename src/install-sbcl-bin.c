#include "cmd-install.h"
#include "opt.h"

int sbcl_bin_expand(struct install_options* param);
int sbcl_bin_install(struct install_options* param);
char* sbcl_bin_extention(struct install_options* param);

char* arch_(struct install_options* param) {
  return cat(param->arch,"-",param->os,NULL);
}

void sbcl_bin_check_file(char* file) {
  FILE* fp=fopen(file,"r");
  int found=0,i,j;
  char line[2000],*str="sbcl";
  if(fp!=NULL)
    while(fgets(line,2000,fp) != NULL)
      for(i=0,j=0;i<2000 && line[i]!='\0';++i)
        if(str[j]==line[i]) {
          ++j;
          if(str[j]=='\0') {
            found=1;break;
          }
        }else j=0;
  else {
    cond_printf(0,"File Open Error\n");
    exit(1);
  }
  if(!found) {
    cond_printf(0,"Invalid html(man in the middle attack?)\n");
    exit(1);
  }
}

int sbcl_version_bin(struct install_options* param) {
  char* home=configdir();
  char* platforms_tsv=cat(home,"tmp",SLASH,"sbcl-bin_uri.tsv",NULL);
  char* uri=get_opt("uri",0);
  cond_printf(1,"sbcl_version_bin\n");
  ensure_directories_exist(platforms_tsv);
  param->version_not_specified=param->version||uri?0:1;
  if(param->version_not_specified) {
    int ret;
    printf("No SBCL version specified. Downloading sbcl-bin_uri.tsv to see the available versions...\n");
    char* uri=get_opt("sbcl-bin-version-uri",0);
    ret=download_simple(uri?uri:PLATFORM_TSV_URI,platforms_tsv,0);
    if(ret!=0) {
      printf("Download failed (Code=%d)\n",ret);
      return 0;
    }
    sbcl_bin_check_file(platforms_tsv);
    param->version=sbcl_bin(platforms_tsv,param->version_not_specified++);
  }else {
    if(param->version)
      param->version=q(param->version);
    else {
      char* name=file_namestring(q(uri));
      int i;
      for(i=0;name[i]!='-' && name[i]!='\0';++i);
      if(name[i]=='-') {
        param->version=q(name+i+1);
        for(i=0;param->version[i]!='-' && param->version[i]!='\0';++i);
        param->version[i]='\0';
      }else {
        param->version="unknown";
      }
      s(name);
    }
  }
  printf("Installing sbcl-bin/%s...\n",param->version);
  param->arch_in_archive_name=1;

  s(platforms_tsv),s(home);
  return 1;
}

int sbcl_bin_download(struct install_options* param) {
  int result;
  char* home=configdir();
  char* arch=arch_(param);
  char* uri=get_opt("uri",0);
  cond_printf(1,"sbcl_bin_download\n");
  int retry=10;
  do {
    param->expand_path=cat(home,"src",SLASH,"sbcl","-",param->version,"-",arch,param->variant,SLASH,NULL);
    impls_sbcl_bin.uri=uri?q(uri):cat(SBCL_BIN_URI ,param->version,"/sbcl-",param->version,
                                   "-",arch,param->variant,"-binary",sbcl_bin_extention(param),NULL);
    result = download(param);
    if(!result && param->version_not_specified) {
      int len = strlen(param->version)-1;
      if('1'<= param->version[len] && param->version[len] <= '9') {
        param->version[len]--;
        s(param->expand_path),s(impls_sbcl_bin.uri);
      }else if('2' <= param->version[len-1] && param->version[len-1] <= '9') {
        param->version[len-1]--;
        param->version[len] = '9';
        s(param->expand_path),s(impls_sbcl_bin.uri);
      }else if('1' == param->version[len-1]) {
        param->version[len-1] = '9';
        param->version[len] = '\0';
        s(param->expand_path),s(impls_sbcl_bin.uri);
      }else{
        s(arch),s(home);
        return 0;
      }
    }
  }while (!result && retry--);
  s(arch),s(home);
  return !!result;
}
#ifndef HAVE_WINDOWS_H

char* sbcl_bin_extention(struct install_options* param) {
  return ".tar.bz2";
}

int sbcl_bin_expand(struct install_options* param) {
  cond_printf(1,"sbcl_bin_expand\n");
  char* argv[6]={"","-xf",NULL,"-C",NULL,NULL};
  char* archive=download_archive_name(param);
  char* dist_path=param->expand_path;
  char* home=configdir();
  printf("Extracting %s to %s\n",archive,dist_path);
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);
  argv[2]=cat(home,"archives",SLASH,archive,NULL);
  argv[4]=cat(home,"src",SLASH,NULL);
  return !cmd_tar(array_stringlist(5,argv),NULL);
}

int sbcl_bin_install(struct install_options* param) {
  int ret;
  char* home=configdir();
  char* impl=param->impl;
  char* version=param->version;
  char* impl_path= cat(home,impldir(param->arch,param->os,impl,version),NULL);
  char* src=param->expand_path;
  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  char* install_root=q(impl_path);
  char* log_path=cat(home,"impls/log/",impl,"-",version,"/install.log",NULL);
  cond_printf(0,"Building %s/%s...",impl,version);
  ensure_directories_exist(impl_path);
  ensure_directories_exist(log_path);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  ret=System("(cat find-gnumake.sh; echo find_gnumake)|sh");
  if(ret!=0) {
    fprintf(stderr,"'make' command not available.\n");
    return 0;
  }
  ret=1;
  if(system_redirect("sh install.sh",log_path)==-1)
    ret=0;
  s(home),s(impl_path),s(sbcl_home),s(install_root),s(log_path);
  printf(" Done.\n");
  return ret;
}
#endif

install_cmds install_sbcl_bin_full[]={
  sbcl_version_bin,
  start,
  sbcl_bin_download,
  sbcl_bin_expand,
  sbcl_bin_install,
  NULL
};

struct install_impls impls_sbcl_bin={ "sbcl-bin", install_sbcl_bin_full,NULL,sbcl_bin_extention};
