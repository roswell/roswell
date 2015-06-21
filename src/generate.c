#include "util.h"
int verbose=0;
char* lispdir(){return q("");}

int main(int argc,char **argv) {
  char buf[1000];
  int pos;
  char *result=fgets(buf,1000,stdin);
  switch(argv[1][0]) {
  case '1':
    if(result) {
      pos=position_char("\r\n",result);
      result[pos]='\0';
      printf("#define ROS_COMPILE_ENVIRONMENT \"%s on ",buf);
      printf("%s",uname());
      printf("(%s)\"\n",uname_m());
    }
    break;
  case '2':
    if(result && strlen(result)!=0) {
      pos=position_char(" ",result);
      result[pos]='\0';
      printf("#define ROS_REVISION \"commit:%s\"\n",result);
    }else
      printf("#define ROS_REVISION \"\"\n");
    break;
  case '3':
    if(result && result[0]!='\0'&&result[0]!='\r'&&result[0]!='\n') {
      pos=position_char("\r\n",result);
      result[pos]='\0';
      substitute_char('\\','/',result);
      result=escape_string(result);
      printf("#define WIN_LISP_PATH \"%s\\\\\"\n",result);
    }
    break;
  }
  exit(EXIT_SUCCESS);
}
