#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#if defined (HAVE_ARCHIVE_H) && defined(_WIN32)
#include <archive.h>
#include <archive_entry.h>
#endif
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "util.h"

static void errmsg(const char *);
int extract(const char *filename, int do_extract, int flags,const char* outputpath);
#if defined (HAVE_ARCHIVE_H) && defined(_WIN32)
static int copy_data(struct archive *, struct archive *);
#endif
static void msg(const char *);

int cmd_tar(int argc, const char **argv)
{
  const char *filename = NULL;
  const char *outputpath = NULL;
  int compress, flags=0, mode, opt;

  mode = 'x';
  compress = '\0';
#if defined(HAVE_ARCHIVE_H) && defined(_WIN32)
  flags = ARCHIVE_EXTRACT_TIME;
#endif
  /* Among other sins, getopt(3) pulls in printf(3). */
  while (*++argv != NULL && **argv == '-') {
    const char *p = *argv + 1;
    while ((opt = *p++) != '\0') {
      switch (opt) {
      case 'f':
        if (*p != '\0')
          filename = p;
        else
          filename = *++argv;
        p += strlen(p);
        break;
      case 'C':
        if (*p != '\0')
          outputpath = p;
        else
          outputpath = *++argv;
        p += strlen(p);
        break;
      case 'p':
#if defined(HAVE_ARCHIVE_H) && defined(_WIN32)
        flags |= ARCHIVE_EXTRACT_PERM;
        flags |= ARCHIVE_EXTRACT_ACL;
        flags |= ARCHIVE_EXTRACT_FFLAGS;
#else
        flags = 1;
#endif
        break;
      case 't':
        mode = opt;
        break;
      case 'v':
        verbose++;
        break;
      case 'x':
        mode = opt;
        break;
      }
    }
  }
  switch (mode) {
  case 't':
    extract(filename, 0, flags,outputpath);
    break;
  case 'x':
    extract(filename, 1, flags,outputpath);
    break;
  }
  return (0);
}

int extract(const char *filename, int do_extract, int flags,const char* outputpath)
{
#if !defined(_WIN32)
  char* str;
  int len=strlen(filename);
  char* type="gzip"; /*for gz*/
  if(len>4) {
    int i;
    for(i=len-4;filename[i]!='\0';++i)
      if(filename[i]=='b'||filename[i]=='B')
        type="bzip2"; /*bz*/
  }
  str=cat(type," -dc ",filename," | tar -",extract?"x":"t",
    flags?"p":"","f - -C ",outputpath,NULL);
  if(verbose>0)
    fprintf(stderr,"extractcmd=%s\n",str);
  int ret=system(str);
  s(str);
#else
  struct archive *a;
  struct archive *ext;
  struct archive_entry *entry;
  int r;

  a = archive_read_new();
  ext = archive_write_disk_new();
  archive_write_disk_set_options(ext, flags);
#if ARCHIVE_VERSION_NUMBER < 3000000
  archive_read_support_compression_bzip2(a);
  archive_read_support_compression_gzip(a);
#else
  archive_read_support_filter_bzip2(a);
  archive_read_support_filter_gzip(a);
  archive_read_support_filter_xz(a);
#endif
  archive_read_support_format_tar(a);

#ifndef NO_LOOKUP
  archive_write_disk_set_standard_lookup(ext);
#endif
  if (filename != NULL && strcmp(filename, "-") == 0)
    filename = NULL;
  if ((r = archive_read_open_filename(a, filename, 10240))) {
    errmsg(archive_error_string(a));
    errmsg("\n");
    exit(r);
  }
  for (;;) {
    r = archive_read_next_header(a, &entry);
    if (r == ARCHIVE_EOF)
      break;
    if (r != ARCHIVE_OK) {
      errmsg(archive_error_string(a));
      errmsg("\n");
      exit(EXIT_FAILURE);
    }
    if (verbose && do_extract)
      msg("x ");
    if (verbose || !do_extract)
      msg(archive_entry_pathname(entry));
    if(outputpath) {
      char* p;
      char* result;
      p=s_cat2(q(outputpath),q(archive_entry_pathname(entry)));
      archive_entry_copy_pathname(entry,p);
      s(p);
    }

    if (do_extract) {
      r = archive_write_header(ext, entry);
      if (r != ARCHIVE_OK)
        errmsg(archive_error_string(a));
      else
        copy_data(a, ext);
    }

    if (verbose || !do_extract)
      msg("\n");
  }
  archive_read_close(a);
#if ARCHIVE_VERSION_NUMBER < 3000000
  archive_read_finish(a);
#else
  archive_read_free(a);
#endif
  return 0;
#endif
}

#if defined(HAVE_ARCHIVE_H) && defined(_WIN32)
static int
copy_data(struct archive *ar, struct archive *aw)
{
  int r;
  const void *buff;
  size_t size;
  int64_t offset;

  for (;;) {
    r = archive_read_data_block(ar, &buff, &size, &offset);
    if (r == ARCHIVE_EOF) {
      errmsg(archive_error_string(ar));
      return (ARCHIVE_OK);
    }
    if (r != ARCHIVE_OK)
      return (r);
    r = archive_write_data_block(aw, buff, size, offset);
    if (r != ARCHIVE_OK) {
      errmsg(archive_error_string(ar));
      return (r);
    }
  }
}

static void
msg(const char *m)
{
  int ret=write(1, m, strlen(m));
}

static void
errmsg(const char *m)
{
  int ret;
  if (m != NULL) {
    ret=write(2, m, strlen(m));
  }
}
#endif
