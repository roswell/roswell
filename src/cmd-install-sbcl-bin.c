#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "cmd-install.h"
#include "opt.h"
char* download_archive_name(struct install_options* param);
int cmd_tar(int argc, const char **argv);
char* arch_(struct install_options* param) {
  return cat(param->arch,"-",param->os,NULL);
}

char* sbcl_bin(char* file);

int sbcl_version_bin(struct install_options* param) {
  char* home=configdir();
  char* ret;
  char* platforms_html=cat(home,"tmp",SLASH,"sbcl-bin.html",NULL);
  ensure_directories_exist(platforms_html);

  if(!param->version) {
    int ret;
    printf("version not specified\nto specify version,downloading platform-table.html...");
    ret=download_simple("http://www.sbcl.org/platform-table.html",platforms_html,0);
    if(ret==0)
      printf("done\n");
    else {
      printf("download error %d\n",ret);
      return 0;
    }

    param->version=sbcl_bin(platforms_html);
    printf("version to install would be '%s'\n",param->version);
  }else {
    param->version=q(param->version);
  }
  param->arch_in_archive_name=1;
  param->expand_path=cat(home,"src",SLASH,"sbcl","-",param->version,"-",arch_(param),SLASH,NULL);
  s(platforms_html),s(home);
  return 1;
}

char* sbcl_bin_extention(struct install_options* param) {
  return SBCL_BIN_EXTENTION;
}

char* sbcl_uri_bin(struct install_options* param) {
  /*should I care about it's existance? */
  char* arch=arch_(param);
  char* ret=cat("http://prdownloads.sourceforge.net/sbcl/sbcl-",param->version,
                "-",arch,
                "-binary",
                sbcl_bin_extention(param)
                ,NULL);
  s(arch);
  return ret;
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
  char* cmd2=escape_string(cmd);
  s(cmd);cmd=cat("cmd /c \"",cmd2,"\"",NULL);s(cmd2);
  if(verbose)
    fprintf(stderr,"msiexeccmd:%s\n",cmd);
  ret=system(cmd);
  s(impl);
  s(dist_path);
  s(log_path);
  s(archive);
  s(cmd),s(home),s(version),s(arch);
  return !ret;
#else
  char* argv[6]={"","-xf",NULL,"-C",NULL,NULL};
  char* archive=download_archive_name(param);
  char* version=q(param->version);
  char* dist_path=param->expand_path;
  char* home=configdir();
  printf("Extracting archive tar. %s to %s\n",archive,dist_path);
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);
  argv[2]=cat(home,"archives",SLASH,archive,NULL);
  argv[4]=cat(home,"src",SLASH,NULL);
  return !cmd_tar(5,(const char**)argv);
#endif
}

int sbcl_bin_install(struct install_options* param) {
#ifdef HAVE_WINDOWS_H
  char* impl=param->impl;
  char* version=param->version;
  char* arch=param->arch;
  char* home=configdir();
  char *str,*str2,*str3,*str4;
  char* version_num= q(version);
  int ret;
  str4=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\sbcl.exe",NULL);
  str2=escape_string(str4);s(str4);
  str4=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\bin\\sbcl.exe",NULL);
  str3=escape_string(str4);s(str4);
  str=cat("cmd /c \"echo f|xcopy \\\"",str2,"\\\" \\\"",str3,"\\\""," > NUL","\"",NULL);
  s(str2),s(str3);
  ret=system(str);s(str);
  if(ret) return 0;
  str4=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\sbcl.core",NULL);
  str2=escape_string(str4);s(str4);
  str4=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\sbcl.core",NULL);
  str3=escape_string(str4);s(str4);
  str=cat("cmd /c \"echo f|xcopy \\\"",str2,"\\\" \\\"",str3,"\\\""," > NUL","\"",NULL);
  ret=system(str);s(str);
  if(ret) return 0;
  str=cat("echo d|xcopy \"",
          home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\contrib\" \"",
          home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\contrib\" >NUL",NULL);

  str4=cat(home,"src\\sbcl-",version,"-",arch,"-windows\\PFiles\\Steel Bank Common Lisp\\",version_num,"\\contrib",NULL);
  str2=escape_string(str4);s(str4);
  str4=cat(home,"impls\\",arch,"\\windows\\sbcl-bin\\",version,"\\lib\\sbcl\\contrib",NULL);
  str3=escape_string(str4);s(str4);
  str=cat("cmd /c \"echo d|xcopy \\\"",str2,"\\\" \\\"",str3,"\\\""," > NUL","\"",NULL);
  ret=system(str);
  s(str),s(home);
  if(ret) return 0;
  return 1;
#else
  int ret=1;
  char* home=configdir();
  char* impl=param->impl;
  char* version=param->version;
  char* impl_path= cat(home,"impls",SLASH,param->arch,SLASH,param->os,SLASH,impl,SLASH,version,NULL);
  char* src=param->expand_path;
  char* sbcl_home=cat(impl_path,"/lib/sbcl",NULL);
  char* install_root=q(impl_path);
  char* log_path=cat(home,"impls/log/",impl,"-",version,"/install.log",NULL);
  fprintf(stderr,"installing %s/%s ",impl,version);
  ensure_directories_exist(impl_path);
  ensure_directories_exist(log_path);
  change_directory(src);
  setenv("SBCL_HOME",sbcl_home,1);
  setenv("INSTALL_ROOT",install_root,1);
  ret=system("(cat find-gnumake.sh; echo find_gnumake)|sh");
  if(ret!=0) {
    fprintf(stderr,"make does not exists.\n");
    return 0;
  }
  ret=1;
  if(system_redirect("sh install.sh",log_path)==-1) {
    ret=0;
  }
  s(home),s(impl_path),s(sbcl_home),s(install_root),s(log_path);
  printf("done.\n");
  return ret;
#endif
}

install_cmds install_sbcl_bin_full[]={
  sbcl_version_bin,
  start,
  download,
  sbcl_bin_expand,
  sbcl_bin_install,
  NULL
};

struct install_impls impls_sbcl_bin={ "sbcl-bin", install_sbcl_bin_full,sbcl_uri_bin,sbcl_bin_extention,0};
