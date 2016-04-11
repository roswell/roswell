/* -*- tab-width : 2 -*- */
#include "opt.h"
extern int count=0;
extern FILE* c_out;
extern int content_length;
extern int download_opt;

#ifdef HAVE_WINDOWS_H
int download_simple (char* uri,char* path,int opt) {
  FILE *bodyfile;
  char* path_partial=cat(path,".partial",NULL);
  bodyfile = fopen(path_partial,"wb");
  if (bodyfile == NULL) {
    s(path_partial);
    return -1;
  }
  c_out=0==(download_opt=opt)?stderr:stdout;
  URL_COMPONENTS u;
  TCHAR szHostName[4096];
  TCHAR szUrlPath[4096];

  u.dwStructSize = sizeof(u);
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
  if(!InternetCrackUrl(uri,(DWORD)strlen(uri),0,&u)) {
    fclose(bodyfile);
    return -4;
  }
  HINTERNET hSession = InternetOpen("WinInet",INTERNET_OPEN_TYPE_PRECONFIG,NULL,NULL,0);
  HINTERNET hConnection = InternetConnect(hSession,szHostName,u.nPort,NULL,NULL,INTERNET_SERVICE_HTTP,0,0);
  DWORD dwFlags = INTERNET_FLAG_RELOAD | INTERNET_FLAG_DONT_CACHE;
  if(INTERNET_SCHEME_HTTP == u.nScheme) {
  }else if( INTERNET_SCHEME_HTTPS == u.nScheme ) {
    dwFlags = dwFlags | INTERNET_FLAG_SECURE| INTERNET_FLAG_IGNORE_CERT_DATE_INVALID| INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  }else {
    fclose(bodyfile);
    return -3;
  }
  HINTERNET hRequest = HttpOpenRequest(hConnection,"GET",szUrlPath,NULL,NULL,NULL,dwFlags,0);

  HttpSendRequest(hRequest,NULL,0,NULL,0);
  DWORD dwStatusCode,dwContentLen;
  DWORD dwLength = sizeof(DWORD);
  if(HttpQueryInfo(hRequest,HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER,&dwContentLen,&dwLength,0))
    content_length=dwContentLen;
  if(!HttpQueryInfo(hRequest,HTTP_QUERY_STATUS_CODE|HTTP_QUERY_FLAG_NUMBER,&dwStatusCode,&dwLength,0)) {
    fclose(bodyfile);
    return -6;
  }
  if(HTTP_STATUS_OK != dwStatusCode) {
    fclose(bodyfile);
    return -5;
  }
  char pData[10000];
  DWORD dwBytesRead = 1;
  count=0;
  while (dwBytesRead) {
    InternetReadFile(hRequest, pData, 99, &dwBytesRead);
    pData[dwBytesRead] = 0;
    write_data(pData,dwBytesRead,1,bodyfile);
  }
  fclose(bodyfile);
  fprintf(c_out, "\n");
  int ret=rename_file(path_partial,path);
  s(path_partial);
  return ret?0:-7;
}
#endif
