#include "opt.h"

static int count=0;
static int block=10240;
static int fold=90;

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  int current = count/block;
  int dots,i;
  count+=written*size;
  for(i=current;i<count/block;++i) {
    printf(".");
    fflush(stdout);
    if(i%fold==0 && i)
      printf("\n");
  }
  return written;
}

static size_t header_callback(char *buffer, size_t size,size_t nitems, int *verbose) {
  int pos=-1,pos2,code=0;
  if(strncmp("HTTP",buffer,size<4?size:4)==0)
    pos=position_char(" ",buffer);
  if(pos!=-1 && (pos2=position_char_not("0123456789",&buffer[pos+1]))!=-1) {
    char *num=subseq(&buffer[pos+1],0,pos2);
    code=atoi(num),s(num);
    if(*verbose)
      fprintf(stderr, "http response:%d\n",code);
  }
  if(400<=code)
    return 0; /*invoke error for curl*/
  return nitems * size;
}

int download_simple (char* uri,char* path,int verbose) {
#ifndef HAVE_WINDOWS_H
  CURL *curl;
  CURLcode res;
  FILE *bodyfile;
  char* path_partial=cat(path,".partial",NULL);

  curl = curl_easy_init();
  if(curl) {
    char* current=get_opt("ros.proxy",1);
    if(current) {
      /*<[protocol://][user:password@]proxyhost[:port]>*/
      char *reserve=current,*protocol=NULL,*userpwd=NULL,*port=NULL,*uri=NULL;
      int pos=position_char("/",current);
      if(pos>0 && current[pos-1]==':' && current[pos+1]=='/')
        protocol=current,current[pos-1]='\0',current=current+pos+2;
      pos=position_char("@",current);
      if(pos!=-1)
        userpwd=current,current[pos]='\0',current=current+pos+1;
      pos=position_char(":",current);
      if(pos!=-1)
        current[pos]='\0',port=current+pos+1,uri=current;
      curl_easy_setopt(curl, CURLOPT_PROXY, uri);
      if(port)
        curl_easy_setopt(curl, CURLOPT_PROXYPORT,atoi(port));
      if(userpwd)
        curl_easy_setopt(curl, CURLOPT_PROXYUSERPWD, userpwd);
      s(reserve);
    }
    count=0;
    curl_easy_setopt(curl, CURLOPT_URL, uri);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, header_callback);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, &verbose);
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
  if(res != CURLE_OK)
    return -2;
#else
  URL_COMPONENTS u;
  TCHAR szHostName[4096];
  TCHAR szUrlPath[4096];
  FILE *bodyfile;
  char* path_partial=cat(path,".partial",NULL);
  bodyfile = fopen(path_partial,"wb");
  if (bodyfile == NULL) {
    s(path_partial);
    return -1;
  }
  u.dwStructSize = sizeof( u );

  u.dwSchemeLength    = 1;
  u.dwHostNameLength  = 4096;
  u.dwUserNameLength  = 1;
  u.dwPasswordLength  = 1;
  u.dwUrlPathLength   = 4096;
  u.dwExtraInfoLength = 1;

  u.lpszScheme     = NULL;
  u.lpszHostName   = szHostName;
  u.lpszUserName   = NULL;
  u.lpszPassword   = NULL;
  u.lpszUrlPath    = szUrlPath;
  u.lpszExtraInfo  = NULL;
  if(!InternetCrackUrl( uri, (DWORD)strlen(uri), 0, &u ))
    return -2;
  HINTERNET hSession = InternetOpen("WinInet",INTERNET_OPEN_TYPE_PRECONFIG,NULL,NULL,0);
  HINTERNET hConnection = InternetConnect(hSession,szHostName,u.nPort,NULL,NULL,INTERNET_SERVICE_HTTP,0,0);
  DWORD dwFlags = INTERNET_FLAG_RELOAD | INTERNET_FLAG_DONT_CACHE;
  if(INTERNET_SCHEME_HTTP == u.nScheme) {
  }else if( INTERNET_SCHEME_HTTPS == u.nScheme ) {
    dwFlags = dwFlags | INTERNET_FLAG_SECURE| INTERNET_FLAG_IGNORE_CERT_DATE_INVALID| INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  }else return -2;
  HINTERNET hRequest = HttpOpenRequest(hConnection,"GET",szUrlPath,NULL,NULL,NULL,dwFlags,0);

  HttpSendRequest(hRequest,NULL,0,NULL,0);
  DWORD dwStatusCode;
  DWORD dwLength = sizeof(DWORD);
  if( !HttpQueryInfo(hRequest,HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,&dwStatusCode,&dwLength,0 ) ) 
    return -4;
  if(HTTP_STATUS_OK != dwStatusCode)
    return -3;
  DWORD dwContentLen;
  DWORD dwBufLen = sizeof(dwContentLen);
  char pData[10000];
  DWORD dwBytesRead = 1;
  count=0;
  while (dwBytesRead) {
    InternetReadFile(hRequest, pData, 99, &dwBytesRead);
    pData[dwBytesRead] = 0;
    write_data(pData,dwBytesRead,1,bodyfile);
  }
  fclose(bodyfile);
#endif
  int ret=rename_file(path_partial,path);
  s(path_partial);
  return ret?0:-3;
}
