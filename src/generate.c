#include "util.h"
int verbose=0;
char** argv_orig;
int main(int argc,char **argv) {
  char* cmd=cat(argv[1]," --version",NULL);
  char* result=system_(cmd);
  int pos=position_char("\r\n",result);
  result[pos]='\0';
  printf("#define ROS_COMPILE_ENVIRONMENT \"%s on ",result);
  printf("%s",uname());
  printf("(%s)\"\n",uname_m());
  result=which("git"EXE_EXTENTION);
  if (file_exist_p(result) && directory_exist_p("../.git/")) {
    result=system_("git log -n 1 --oneline");
    pos=position_char(" ",result);
    result[pos]='\0';
    printf("#define ROS_REVISION \"commit:%s\"\n",result);
  }else {
    printf("#define ROS_REVISION \"\"\n");
  }
  exit(EXIT_SUCCESS);
}
