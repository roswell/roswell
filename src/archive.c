#include <sys/types.h>
#include <sys/stat.h>

#include <archive.h>
#include <archive_entry.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "util.h"

static void errmsg(const char *);
static int extract(const char *filename, int do_extract, int flags,const char* outputpath);
static int copy_data(struct archive *, struct archive *);
static void msg(const char *);

static int verbose = 0;

int cmd_tar(int argc, const char **argv)
{
  const char *filename = NULL;
  const char *outputpath = NULL;
  int compress, flags, mode, opt;

  (void)argc;
  mode = 'x';
  verbose = 0;
  compress = '\0';
  flags = ARCHIVE_EXTRACT_TIME;
  /* Among other sins, getopt(3) pulls in printf(3). */
  argv--;
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
	flags |= ARCHIVE_EXTRACT_PERM;
	flags |= ARCHIVE_EXTRACT_ACL;
	flags |= ARCHIVE_EXTRACT_FFLAGS;
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

static int
extract(const char *filename, int do_extract, int flags,const char* outputpath)
{
  struct archive *a;
  struct archive *ext;
  struct archive_entry *entry;
  int r;

  a = archive_read_new();
  ext = archive_write_disk_new();
  archive_write_disk_set_options(ext, flags);
  archive_read_support_filter_bzip2(a);
  archive_read_support_filter_gzip(a);
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
      char* p=s_cat2(q(outputpath),q(archive_entry_pathname(entry)));
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
  archive_read_free(a);
}

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
