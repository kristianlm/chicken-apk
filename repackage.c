// taken from https://github.com/godotengine/godot/blob/e20fb10d35fad895f1150232f99448e4812643ae/platform/android/export/export.cpp
//
// most of this is from godot.  however, the way that godot rewrites
// manifest and resources has not been used. see binary.scm for that.
//
// by Kristian Lein-Mathisen 2019
#include "minizip/ioapi.h"
#include "minizip/unzip.h"
#include "minizip/zip.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

// TODO: do something sensible here
static zip_fileinfo get_zip_fileinfo() {
  zip_fileinfo zipfi;
  zipfi.tmz_date.tm_hour = 10;
  zipfi.tmz_date.tm_mday = 2;
  zipfi.tmz_date.tm_min = 22;
  zipfi.tmz_date.tm_mon = 5;
  zipfi.tmz_date.tm_sec = 33;
  zipfi.tmz_date.tm_year = 1985;
  zipfi.dosDate = 0;
  zipfi.external_fa = 0;
  zipfi.internal_fa = 0;

  return zipfi;
}

int string_prefixp(const char *str, const char *prefix)
{
    size_t lenpre = strlen(prefix), lenstr = strlen(str);
    return lenstr < lenpre ? 0 : strncmp(prefix, str, lenpre) == 0;
}

int zipadd(zipFile unaligned_apk, char* fname) {
  zip_fileinfo zipfi = get_zip_fileinfo();
  zipOpenNewFileInZip(unaligned_apk, fname,
                      &zipfi, NULL, 0, NULL, 0, NULL,
                      0, Z_DEFAULT_COMPRESSION);

  FILE *f = fopen(fname, "r");
  if(!f) {
    fprintf(stderr, "couldn't open '%s'\n", fname);
    exit(-1);
  }
  char* buffer[1024];
  int size;
  while((size = fread(buffer, 1, 1024, f)) > 0) {
    zipWriteInFileInZip(unaligned_apk, buffer, size);
  }
  zipCloseFileInZip(unaligned_apk);
}

int rezip(char* apk_src, char* apk_dst, C_word user) {
  fprintf(stderr, "########## rezipping '%s' => '%s'\n", apk_src, apk_dst);

  unzFile pkg = unzOpen(apk_src);
  if (!pkg) {
    fprintf(stderr, "Could not find template APK to export:\n '%s'", apk_src);
    return -1;
  }
  fprintf(stderr, "%p\n", &pkg);

  int ret = unzGoToFirstFile(pkg);
  zipFile unaligned_apk = zipOpen(apk_dst, APPEND_STATUS_CREATE);

  while (ret == UNZ_OK) {
    int skip = 0;
    //get filename
    unz_file_info info;
    char fname[16384];
    ret = unzGetCurrentFileInfo(pkg, &info, fname, 16384, NULL, 0, NULL, 0);
    
    int64_t size = info.uncompressed_size;
    char* data = malloc(size);
    unzOpenCurrentFile(pkg);
    unzReadCurrentFile(pkg, data, size);
    unzCloseCurrentFile(pkg);

    //fprintf(stderr, "editing: %s  %p (%d bytes)\n", fname, data, size);
    user = edit_apk_file(user, fname, &data, &size);
    fprintf(stderr, "edited:  %s  %p (now %d bytes)\n", fname, data, size);
    
    if(data) {
      int method = info.compression_method;// or Z_DEFLATED;
      zip_fileinfo zipfi = get_zip_fileinfo();
      zipOpenNewFileInZip(unaligned_apk,
                          fname, &zipfi,
                          NULL, 0, NULL, 0, NULL,
                          method,
                          Z_DEFAULT_COMPRESSION);
      zipWriteInFileInZip(unaligned_apk, data, size);
      zipCloseFileInZip(unaligned_apk);
      free(data);
    }
    ret = unzGoToNextFile(pkg);
  }
  unzClose(pkg);

  // // ==================== adding custom files ====================

  zipadd(unaligned_apk, "lib/x86/libchicken.so");
  zipadd(unaligned_apk, "lib/x86/libnative-lib.so");

  zipClose(unaligned_apk, NULL);
  fprintf(stderr, "########## produced '%s'\n", apk_dst);
}


// from what I understand, aligning means making the start of every
// uncompressed file in the zip-file and making its offset a multiple
// of 4. this is probably so that android can memory-map directly into
// the zip-file or something like that.
int apk_align(char* funaligned, char* faligned) {
  static const int ZIP_ALIGNMENT = 4;
  unzFile unaligned = unzOpen(funaligned);
  zipFile aligned = zipOpen(faligned, APPEND_STATUS_CREATE);
  fprintf(stderr, "########## aligning %s => %s \n", funaligned, faligned);
  int bias = 0;
  int ret = unzGoToFirstFile(unaligned);
  
  while(ret == UNZ_OK) {
    unz_file_info info;
    memset(&info, 0, sizeof(info));

    char fname[16384];
    char extra[16384];
    ret = unzGetCurrentFileInfo(unaligned, &info, fname, 16384, extra, 16384 - ZIP_ALIGNMENT, NULL, 0);
    int64_t size = info.compressed_size;

    fprintf(stderr, "aligning '%s' (%d bytes)\n", fname, size);
    
    char* data = malloc(size);

    int method, level;
    unzOpenCurrentFile2(unaligned, &method, &level, 1); // raw read
    long file_offset = unzGetCurrentFileZStreamPos64(unaligned);
    unzReadCurrentFile(unaligned, data, size);
    unzCloseCurrentFile(unaligned);

    int padding = 0;
    if (!info.compression_method) { // uncompressed file => align
      long new_offset = file_offset + bias;
      padding = (ZIP_ALIGNMENT - (new_offset % ZIP_ALIGNMENT)) % ZIP_ALIGNMENT;
    }
    memset(extra + info.size_file_extra, 0, padding);

    // write
    zip_fileinfo zipfi = get_zip_fileinfo();
    zipOpenNewFileInZip2(aligned,
                         fname,
                         &zipfi,
                         extra,
                         info.size_file_extra + padding,
                         NULL,
                         0,
                         NULL,
                         method,
                         level,
                         1); // raw write
    zipWriteInFileInZip(aligned, data, size);
    zipCloseFileInZipRaw(aligned, info.uncompressed_size, info.crc);
    
    bias += padding;
    
    free(data);
    ret = unzGoToNextFile(unaligned);
  }
  zipClose(aligned, NULL);
  unzClose(unaligned);
}

/* int main() { */
/*   apk_rezip("sotest.apk", "out-unaligned.apk"); */
/*   //apk_debug_sign(); */
/*   //apk_align(); */
/* } */
