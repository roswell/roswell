#include "opt.h"

int download_count=0;
int download_width=74;
int content_length=0;
int download_opt=0;
FILE* download_out;

size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  static char* last_showd=NULL;
  char* w=q("\r");
  last_showd=last_showd?last_showd:q("");
  download_count+=written*size;
  if(download_opt&&content_length) {
    int i,len=download_width*download_count/content_length-download_width*(download_count-written*size)/content_length;
    s(w);
    for(i=0;i<len;++i)
      fprintf(download_out,"#"),fflush(download_out);
    return written;
  }
  if(content_length) {
    int i;
    w=s_cat2(w,q("["));
    for(i=0;i<download_width;++i)
      w=s_cat2(w,q(((i>=(download_count*download_width)/content_length)?" ":"#")));
    w=s_cat2(w,qsprintf(8,"]%3d%%",(100*download_count)/content_length));
  }else {
    int current,aux;
    aux=1024>download_count?' ':1024*1024>download_count?(current=download_count/1024,'K'):
      1024*1024*1024>download_count?(current=download_count/(1024*1024),'M'):(current=download_count/(1024*1024*1024),'G');
    w=s_cat2(w,qsprintf(20,"%4d%c downloaded.",current,aux));
  }
  if(strcmp(w,last_showd)) {
    if(!(download_opt&1))
      fprintf(download_out, "%s", w),fflush(download_out);
    s(last_showd),last_showd=q(w);
  }
  s(w);return written;
}

static size_t header_callback(char *buffer, size_t size,size_t nitems, int *opt) {
  int pos=-1,pos2,code=0;
  if(strncmp("HTTP",buffer,nitems<4?nitems:4)==0) {
    pos=position_char(" ",buffer);
    if(pos!=-1 && (pos2=position_char_not("0123456789",&buffer[pos+1]))!=-1) {
      char *num=subseq(&buffer[pos+1],0,pos2);
      code=atoi(num),s(num);
      if(verbose)
        fprintf(download_out, "http response:%d\n",code);
    }
    if(400<=code)
      return 0; /*invoke error for curl*/
  }else if(strncmp("content-length:",downcase(buffer),nitems<15?nitems:15)==0) {
    pos=position_char(" ",buffer);
    if(pos!=-1 && (pos2=position_char_not("0123456789",&buffer[pos+1]))!=-1) {
      char *num=subseq(&buffer[pos+1],0,pos2);
      code=atoi(num),s(num);
      content_length=code;
    }
  }
  return nitems * size;
}
/* return value:
 * 0 success
 * 1 fopen failed
 * 2 curl initialization failed
 * 3 scheme is neither http nor https
 * 4 faild to parse the URL (InternetCrackUrl) (windows)
 * 5 https responce status is not HTTP_STATUS_OK (windows)
 * 6 HttpQueryInfo failed (windows)
 * 7 rename failure
 */
#ifndef HAVE_WINDOWS_H
int download_simple (char* uri,char* path,int opt) {
  FILE *bodyfile;
  char* path_partial=cat(path,".partial",NULL);
  cond_printf(1,"download_simple(\"%s\",\"%s\",%d)\n",uri,path,opt);
  bodyfile = fopen(path_partial,"wb");
  if(bodyfile == NULL) {
    s(path_partial);
    return 1;
  }
  download_out=0==(download_opt=opt)?stderr:stdout;

  CURL *curl;
  CURLcode res=!CURLE_OK;
  curl = curl_easy_init();
  if(curl) {
    char* current=get_opt("ros.proxy",1);
    int lenuri=strlen(uri);
    int https=(lenuri>5 && strncmp("https",uri,5)==0);
    int httponly= get_opt("proxy.http.only",0) && strcmp(get_opt("proxy.http.only",0),"1")==0;
    int ignoressl= get_opt("ssl.ignore_verify",0) && strcmp(get_opt("ssl.ignore_verify", 0), "1") ==0;
    if(current&& ((https && !httponly) || !https)) {
      /*<[protocol://][user:password@]proxyhost[:port]>*/
      char *reserve,*protocol=NULL,*userpwd=NULL,*port=NULL,*uri=NULL;
      int pos=position_char("/",current);
      reserve=current=q_(current);
      if(pos>0 && current[pos-1]==':' && current[pos+1]=='/')
        protocol=current,current[pos-1]='\0',current=current+pos+2;
      pos=position_char("@",current);
      if(pos!=-1)
        userpwd=current,current[pos]='\0',current=current+pos+1;
      pos=position_char(":",current);
      if(pos!=-1)
        current[pos]='\0',port=current+pos+1,uri=current;
      cond_printf(1,"proxy uri=%s",uri);
      curl_easy_setopt(curl, CURLOPT_PROXY, uri);
      if(port) {
        cond_printf(1," port=%s",port);
        curl_easy_setopt(curl, CURLOPT_PROXYPORT,atoi(port));
      }
      if(userpwd)
        curl_easy_setopt(curl, CURLOPT_PROXYUSERPWD, userpwd);
      cond_printf(1,"\n");
      s(reserve);
    }
    download_count=0,content_length=0;
    curl_easy_setopt(curl, CURLOPT_URL, uri);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, PACKAGE_STRING);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, header_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA,bodyfile);
    if((verbose & 2) != 0)
      curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);
    if(ignoressl) {
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0); 
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0);
    }
    res=curl_easy_perform(curl);
    if(res != CURLE_OK && verbose) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }
    curl_easy_cleanup(curl);
    fclose(bodyfile);
  }
  if(res != CURLE_OK)
    return 2;
  fprintf(download_out, "\n");
  int ret=rename_file(path_partial,path);
  s(path_partial);
  return ret?0:7;
}

int download_head (char* uri,int opt) {
  download_out=(0==opt)?stderr:stdout;
  if(verbose)
    fprintf(download_out,"head:%s\n",uri);
  CURL *curl;
  CURLcode res=!CURLE_OK;
  curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, uri);
  curl_easy_setopt(curl, CURLOPT_USERAGENT, PACKAGE_STRING);
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
  curl_easy_setopt(curl, CURLOPT_NOBODY, 1);
  res = curl_easy_perform(curl);
  long http_code = 0;
  curl_easy_getinfo (curl, CURLINFO_RESPONSE_CODE, &http_code);
  curl_easy_cleanup(curl);
  if(res != CURLE_OK)
    return 2;
  if(verbose)
    fprintf(download_out,"head code:%ld\n",http_code);
  if(200 <= http_code && http_code < 300)
    return 0;
  return 1;
}
#endif
