#include <jni.h>
#include <string.h>
#include <stdio.h>
#include <zconf.h>

int app(const char *libDir) {
    printf("# Dummy Template\n");
    printf("\n");
    printf("This `int app(const char*)` is meant to be provided by your own native library. ");
    printf("This one just \x1b{{capitalizes input\x1b}} for testing purposes. ");
    printf("The native library directory of this app are here: '%s'\n", libDir);
    printf("\n");

    jbyte buf[1024];
    while(1) {
        size_t rd = read(0, buf, 1024);
        if(rd == 0) break;
        for (int i = 0; i < rd; ++i) {
            if(buf[i] >= 'a' && buf[i] <= 'z')
                buf[i] = buf[i] - 'a' + 'A';
          //printf("%x ", buf[i]);
        } //printf("\n");
        write(1, buf, rd);
    }

    return 0;
}

