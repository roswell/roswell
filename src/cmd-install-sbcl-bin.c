#include "cmd-install.h"
#include "opt.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

char* arch_(struct install_options* param) {
  return cat(param->arch,"-",param->os,NULL);
}

char* sbcl_bin_extention(struct install_options* param) {
  return SBCL_BIN_EXTENTION;
}

int sbcl_version_bin(struct install_options* param) {
  char* home=configdir();
  char* platforms_html=cat(home,"tmp",SLASH,"sbcl-bin.html",NULL);
  ensure_directories_exist(platforms_html);

  if(!param->version) {
    int ret;
    printf("SBCL version is not specified. Downloading platform-table.html to see which version to install...\n");
    ret=download_simple("http://www.sbcl.org/platform-table.html",platforms_html,0);
    if(ret!=0) {
      printf("Something wrong with sbcl.org.Download failed (Code=%d) try backup.\n",ret);
      ret=download_simple("https://gist.githubusercontent.com/snmsts/12f0cf8567b8e26ae15f/raw/0b9134f9820c195e1ca84d382176777ec989183e/platform-table.html",platforms_html,0);
    }
    if(ret!=0) {
      printf("Download failed (Code=%d)\n",ret);
      return 0;
    }

    param->version=sbcl_bin(platforms_html);
    printf("Installing sbcl-bin/%s...\n",param->version);
  }else
    param->version=q(param->version);
  param->arch_in_archive_name=1;

  s(platforms_html),s(home);
  return 1;
}

int sbcl_bin_download(struct install_options* param) {
  int result;
  char* home=configdir();
  char* arch=arch_(param);
  do {
    param->expand_path=cat(home,"src",SLASH,"sbcl","-",param->version,"-",arch,SLASH,NULL);
    impls_sbcl_bin.uri=cat(SBCL_MASTER"/sbcl-",param->version,
                           "-",arch,"-binary",sbcl_bin_extention(param),NULL);
    result = download(param);
    if(!result) {
      int len = strlen(param->version)-1;
      if('1'<= param->version[len] && param->version[len] <= '9') {
        param->version[len]--;
        s(param->expand_path),s(impls_sbcl_bin.uri);
      }else{
        s(arch),s(home);
        return 0;
      }
    }
  }while (!result);
  s(arch),s(home);
  return 1;
}

int sbcl_bin_expand(struct install_options* param) {
#ifdef HAVE_WINDOWS_H
  char* impl=param->impl;
  char* version=q(param->version);
  int ret;
  char* home=configdir();
  char* arch= arch_(param);
  char* archive=cat(impl,"-",version,"-",arch,".msi",NULL);
  char* log_path=cat(home,"impls",SLASH,"log",SLASH,impl,"-",version,"-",arch,SLASH,"install.log",NULL);
  char* dist_path;
  int pos=position_char("-",impl);
  if(pos!=-1) {
    impl=subseq(impl,0,pos);
  }else
    impl=q(impl);
  dist_path=cat(home,"src",SLASH,impl,"-",version,"-",arch,SLASH,NULL);
  printf("Extracting archive by msi. %s to %s\n",archive,dist_path);
  archive=s_cat(q(home),q("archives"),q(SLASH),archive,NULL);
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);
  ensure_directories_exist(log_path);
  if(dist_path[strlen(dist_path)-1]=='\\')
    dist_path[strlen(dist_path)-1]='\0';

  char* cmd=cat("msiexec.exe /a \"",
                archive,
                "\" targetdir=\"",
                dist_path,
                "\" /qn /lv ",
                "\"",
                log_path,
                "\"",
                NULL);
  cmd=cat("cmd /c \"",cmd,"\"",NULL);
  cond_printf(1,"msiexeccmd:%s\n",cmd);
  ret=System(cmd);
  s(impl);
  s(dist_path);
  s(log_path);
  s(archive);
  s(cmd),s(home),s(version),s(arch);
  return !ret;
#else
  char* argv[6]={"","-xf",NULL,"-C",NULL,NULL};
  char* archive=download_archive_name(param);
  char* dist_path=param->expand_path;
  char* home=configdir();
  printf("Extracting %s to %s\n",archive,dist_path);
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);
  argv[2]=cat(home,"archives",SLASH,archive,NULL);
  argv[4]=cat(home,"src",SLASH,NULL);
  return !cmd_tar(5,argv,NULL);
#endif
}

int sbcl_bin_install(struct install_options* param) {
#ifdef HAVE_WINDOWS_H
  char* version=param->version;
  char* arch=param->arch;
  char* home=configdir();
  char *str,*str2,*str3,*str4;
  char* version_num= q(version);
  int ret;
  str2=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\sbcl.exe",NULL);
  str3=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\bin\\sbcl.exe",NULL);
  str=cat("cmd /c \"echo f|xcopy ^\"",str2,"^\" ^\"",str3,"^\" > NUL","\"",NULL);
  s(str2),s(str3);
  ret=System(str);s(str);
  if(ret) return 0;
  str2=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\sbcl.core",NULL);
  str3=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\sbcl.core",NULL);
  str=cat("cmd /c \"echo f|xcopy ^\"",str2,"^\" ^\"",str3,"^\" > NUL","\"",NULL);
  ret=System(str);s(str);
  if(ret) return 0;
  str=cat("echo d|xcopy ^\"",
          home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\contrib^\" ^\"",
          home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\contrib^\" >NUL",NULL);

  str2=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\contrib",NULL);
  str3=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\contrib",NULL);
  str=cat("cmd /c \"echo d|xcopy ^\"",str2,"^\" ^\"",str3,"^\""," > NUL","\"",NULL);
  ret=System(str);
  s(str),s(home);
  if(ret) return 0;
  return 1;
#else
  int ret;
  char* home=configdir();
  char* impl=param->impl;
  char* version=param->version;
  char* impl_path= cat(home,"impls",SLASH,param->arch,SLASH,param->os,SLASH,impl,SLASH,version,NULL);
  char* src=param->expand_path;
  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  char* install_root=q(impl_path);
  char* log_path=cat(home,"impls/log/",impl,"-",version,"/install.log",NULL);
  fprintf(stderr,"Building %s/%s...",impl,version);
  ensure_directories_exist(impl_path);
  ensure_directories_exist(log_path);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  ret=System("(cat find-gnumake.sh; echo find_gnumake)|sh");
  if(ret!=0) {
    fprintf(stderr,"make does not exists.\n");
    return 0;
  }
  ret=1;
  if(system_redirect("sh install.sh",log_path)==-1)
    ret=0;
  s(home),s(impl_path),s(sbcl_home),s(install_root),s(log_path);
  printf(" Done.\n");
  return ret;
#endif
}

install_cmds install_sbcl_bin_full[]={
  sbcl_version_bin,
  start,
  sbcl_bin_download,
  sbcl_bin_expand,
  sbcl_bin_install,
  NULL
};

struct install_impls impls_sbcl_bin={ "sbcl-bin", install_sbcl_bin_full,NULL,sbcl_bin_extention,0};
