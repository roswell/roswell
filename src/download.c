#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
}
extern char* homedir(void);
extern int ensure_directories_exist (char* path);

int download_simple (char* uri,char* path,int verbose)
{
  CURL *curl;
  CURLcode res;
  FILE *bodyfile;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, uri);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    bodyfile = fopen(path,"wb");
    if (bodyfile == NULL) {
      curl_easy_cleanup(curl);
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
    return 0;
  }
}

int cmd_download (int argc,char **argv) {
  if(argc>=2) {
    return download_simple(argv[0],argv[1],1);
  }
  return 0;
}
