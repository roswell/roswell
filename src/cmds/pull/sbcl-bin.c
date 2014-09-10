#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "pull.h"
#include "opt.h"

extern char* sbcl_extention(char* impl,char* version);
extern int sbcl_install(char* impl,char* version);

char* arch(void) {
  return s_cat(uname_m(),q("-"),uname(),NULL);
}

char* sbcl_bin(char* file);

char* sbcl_version_bin(char* impl,char* version)
{
  char* home= homedir();
  char* platforms_html=cat(home,"tmp/sbcl.html",NULL);
  ensure_directories_exist(platforms_html);
  /* TBD */
  if(version) {
    s(platforms_html);
    return s_cat(q(version),q("-"),arch(),NULL);
  }else {
    char* version;
    char* ret;
    printf("version not specified\nto specify version,downloading platform-table.html...");
    download_simple("http://www.sbcl.org/platform-table.html",platforms_html,0);
    printf("done\n");
    version=sbcl_bin(platforms_html);
    printf("version to install would be '%s'\n",version);
    ret = s_cat(version,q("-"),arch(),NULL);
    s(platforms_html);
    return ret;
  }
}

char* sbcl_uri_bin(char* impl,char* version)
{
  /*should I care about it's existance? */
  return cat("http://prdownloads.sourceforge.net/sbcl/sbcl-",version,
	     "-binary.tar.bz2",NULL);
}

install_cmds install_sbcl_bin_full[]={
  start,
  download,
  expand,
  sbcl_install,
  NULL
};

struct install_impls impls_sbcl_bin={ "sbcl-bin", install_sbcl_bin_full,sbcl_version_bin,sbcl_uri_bin,sbcl_extention};
