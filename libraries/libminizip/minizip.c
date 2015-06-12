// MiniZip based routines to manage embedded zip archives

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef WIN32
#include <sys/types.h>
#include <sys/stat.h>
#endif 

#include "unzip.h"
#include <zlib.h>

//#define DEBUG_LNEMBED 1

#ifdef DEBUG_LNEMBED
#define DMSG(fmt...) (fprintf(stderr,"DEBUG: LNEMBED: " fmt),fprintf(stderr,"\n"))
#else
#define DMSG(fmt...)
#endif

extern char *embed_zip;
extern unsigned int embed_zip_len;

extern char *embed2_zip;
extern unsigned int embed2_zip_len;

void fill_memory_filefunc(zlib_filefunc_def*);

static int _minizip_file_exists(char *fname)
{
  FILE *fd = fopen(fname,"rb");
  int res = (fd?1:0);
  if (fd) fclose(fd);
  DMSG("_minizip_file_exists %s [%i]", fname, res);
  return res;
}

static int portable_mkdir(char *dir)
{
#ifdef WIN32
  return (int)_mkdir(dir);
#else
  return mkdir(dir,0777);
#endif
}

static int _minizip_mkdir(char *dir) {
  DMSG("_minizip_mkdir %s", dir);
  char tmp[2048];
  char *p = NULL;
  size_t len;
  snprintf(tmp, sizeof(tmp),"%s",dir);
  len = strlen(tmp);
  if(tmp[len - 1] == '/') tmp[len - 1] = 0;
  for (p = tmp + 1; *p; p++) {
    if (*p == '/') {
      *p = 0;
      portable_mkdir(tmp);
      *p = '/';
    }
  }
  return portable_mkdir(tmp);
}

int minizip_unpack_to_disk(char *path, void *ptr, int len)
{
  // check the magic
  char *data = (char*)ptr;
  if (data[0]!='P'||data[1]!='K') goto unpack_to_disk_fail;

  // setup the memory image
  zlib_filefunc_def pzlib_filefunc_def;
  fill_memory_filefunc(&pzlib_filefunc_def);
  char memname[128];
  sprintf(memname,"%lx+%lx", (unsigned long)ptr, (unsigned long)len);
  unzFile hFile = unzOpen2(memname,&pzlib_filefunc_def);

  // get info  
  unz_global_info  globalInfo = {0};
  if (!unzGetGlobalInfo(hFile, &globalInfo )==UNZ_OK ) goto unpack_to_disk_fail;
  int entries = globalInfo.number_entry;

  // do the unpacking 
  if (unzGoToFirstFile(hFile) != UNZ_OK) goto unpack_to_disk_fail;
  int i=0;

  while (1) {
    char fname[1024];
    char filename_inzip[256]={0};
    unz_file_info64 file_info;
    if (unzGetCurrentFileInfo64(hFile,&file_info,
          filename_inzip,sizeof(filename_inzip), 
          NULL,0,NULL,0)!=UNZ_OK) goto unpack_to_disk_fail;

    sprintf(fname,"%s%c%s", path,
      #ifdef WIN32 
      '\\'
      #else
      '/'
      #endif
      , filename_inzip);

    int fname_len = strlen(filename_inzip);
    if (filename_inzip[fname_len-1]=='/'||filename_inzip[fname_len-1]=='\\') {
      // it's a directory
      _minizip_mkdir(fname);
    } else {
      // it's a file
      if (unzOpenCurrentFile(hFile) != UNZ_OK) goto unpack_to_disk_fail;
      FILE *fd =0;
      int n_tot=0, n_read=0;
      unsigned char buf[32768];
      while ((n_read = unzReadCurrentFile(hFile, buf, 32768)) > 0) {
        if (!n_tot&&!fd&&!_minizip_file_exists(fname)) {
          DMSG("fopen %s", fname);
          fd = fopen(fname,"wb");
        } 
        if (fd) {
          DMSG("fwrite %i", n_read);
          fwrite(buf,n_read,1,fd);
        }
        n_tot+=n_read;
      }
      if (fd) { DMSG("fclose"); fclose(fd); }
      
      DMSG("%s [%i]",fname,n_tot);

      unzCloseCurrentFile(hFile);
    }

    if (++i==entries) break;
    if (unzGoToNextFile(hFile)!=UNZ_OK) goto unpack_to_disk_fail;
  }
 
  // cleanup
  unzClose(hFile);
  return 0;
  unpack_to_disk_fail:
  return 1;
}

void *minizip_unpack_to_memory(void *ptr, int len, int *unpack_len)
{
  char *data = (char*)ptr;
  if (data[0]!='P'||data[1]!='K') goto unpack_to_mem_fail;
  zlib_filefunc_def pzlib_filefunc_def;
  fill_memory_filefunc(&pzlib_filefunc_def);
  char memname[128];
  sprintf(memname,"%lx+%lx", (unsigned long)ptr, (unsigned long)len);
  unzFile hFile = unzOpen2(memname,&pzlib_filefunc_def);
  if (unzGoToFirstFile(hFile) != UNZ_OK) goto unpack_to_mem_fail;
  if (unzOpenCurrentFile(hFile) != UNZ_OK) goto unpack_to_mem_fail;
  unsigned char *tot = 0;
  unsigned int n_tot=0, n_read=0;
  unsigned char buf[1024];
  while ((n_read = unzReadCurrentFile(hFile, buf, 1024)) > 0) {
    if (tot==0) tot = malloc(n_read); else tot = realloc(tot,n_tot+n_read);
    if (tot==0) goto unpack_to_mem_fail;
    memcpy(tot+n_tot,buf,n_read);
    n_tot+=n_read;
  }
  unzClose(hFile);
  *unpack_len = n_tot;
  return tot;
  unpack_to_mem_fail:
  return 0;
}

// eof
