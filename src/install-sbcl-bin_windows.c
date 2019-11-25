#include "opt.h"
#ifdef HAVE_WINDOWS_H
#include "cmd-install.h"
#include "util.h"

char* sbcl_bin_extention(struct install_options* param) {
  return ".msi";
}

int sbcl_bin_expand(struct install_options* param) {
  char* impl=param->impl;
  char* version=q(param->version);
  int ret;
  char* home=configdir();
  char* arch= arch_(param);
  char* archive=cat(impl,"-",version,"-",arch,".msi",NULL);
  char* log_path=cat(home,"impls",SLASH,"log",SLASH,impl,"-",version,"-",arch,SLASH,"install.log",NULL);
  char* dist_path;
  int pos=position_char("-",impl);
  impl=(pos!=-1)?subseq(impl,0,pos):q(impl);
  dist_path=cat(home,"src",SLASH,impl,"-",version,"-",arch,SLASH,NULL);
  
  char* msiexec_path="msiexec.exe";
  if(!file_exist_p(msiexec_path))
  {
	  msiexec_path="C:\\Windows\\System32\\msiexec.exe";
	  if(!file_exist_p(msiexec_path))
	  {
		  printf("Msiexec.exe not found in the system path\n");
		  return 0;
	  }
  }
  
  printf("Extracting the msi archive. %s to %s\n",archive,dist_path);
  archive=s_cat(q(home),q("archives"),q(SLASH),archive,NULL);
  delete_directory(dist_path,1);
  ensure_directories_exist(dist_path);
  ensure_directories_exist(log_path);
  if(dist_path[strlen(dist_path)-1]=='\\')
    dist_path[strlen(dist_path)-1]='\0';

  char* cmd=cat(msiexec_path,
				" /a \"",
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
}

int sbcl_bin_install(struct install_options* param) {
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
}
#endif
