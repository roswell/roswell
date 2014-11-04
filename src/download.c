#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>
#include "util.h"

static int count=0;
static int block=10240;
static int fold=90;

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  int current = count/block;
  int dots;
  int i;
  count+=written;
  for(i=current;i<count/block;++i){
    printf(".");
    fflush(stdout);
    if(i%fold==0)
      printf("\n");
  }
  return written;
}

int download_simple (char* uri,char* path,int verbose)
{
  CURL *curl;
  CURLcode res;
  FILE *bodyfile;
  char* path_partial=cat(path,".partial",NULL);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, uri);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    bodyfile = fopen(path_partial,"wb");
    if (bodyfile == NULL) {
      curl_easy_cleanup(curl);
      s(path_partial);
      return -1;
    }
    curl_easy_setopt(curl,CURLOPT_WRITEDATA,bodyfile);
    res = curl_easy_perform(curl);
    if(res != CURLE_OK && verbose) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }
    curl_easy_cleanup(curl);
  }
  fclose(bodyfile);
  if(res != CURLE_OK) {
    return -2;
  }else {
    int ret=rename_file(path_partial,path);
    s(path_partial);
    if(ret)
      return 0;
    return -3;
  }
}

int cmd_download (int argc,char **argv) {
  if(argc>=2) {
    fprintf(stderr,"download %s %s",argv[1],argv[2]);
    return download_simple(argv[1],argv[2],1);
  }
  return 0;
}
