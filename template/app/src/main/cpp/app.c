#include <jni.h>
#include <string.h>
#include <stdio.h>
#include <zconf.h>

int app(const char *libDir) {

    int i=0;
    while(1) {
        printf("testing %03d\n", i++);
        usleep(100000);
    }
    printf("# Dummy Template\n");
    printf("This `int app(const char*)` is meant to be provided by your own native library.\n");
    printf("This one just \x1b{{capitalizes input\x1b}} for testing purposes.\n");
    printf("The native library directory of this app are here: '%s'\n", libDir);
    printf("\n");

    jbyte buf[1024];
    while(1) {
        size_t rd = read(0, buf, 1024);
        if(rd == 0) break;
        for (int i = 0; i < rd; ++i) {
            if(buf[i] >= 'a' && buf[i] <= 'z')
                buf[i] = buf[i] - 'a' + 'A';
          printf("%x ", buf[i]);
        } printf("\n");
        write(1, buf, rd);
    }

    return 0;
}

