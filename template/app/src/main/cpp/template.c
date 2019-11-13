/**
 * This file provides the JNI procedures to set up IO so that stdin, stdout and stderr
 * can be integrated with the app's own UI.
 */
#include <jni.h>
#include <string.h>
#include <android/log.h>
#include <stdio.h>
#include <zconf.h>

int app(const char *libDir);

static int pfd[2]; // output pipe
static int pfdi[2]; // input pipe

/**
 * Called before all others threads start, so that
 * IO is ready when we call launch.
 */
JNIEXPORT void JNICALL
Java_org_call_1cc_android_template_repl_MainActivity_iosetup(JNIEnv *env, jobject instance) {
    static int initialized = 0;
    if(initialized) return;

    //start_logger(); return ; // use this instead if stdout/stderr in the app isn't working

    // make unbuffered
    setvbuf(stdout, 0, _IONBF, 0);
    setvbuf(stderr, 0, _IONBF, 0);
    setvbuf(stdin, 0, _IONBF, 0);

    // copy stdout and stderr
    pipe(pfd);
    dup2(pfd[1], 1);
    dup2(pfd[1], 2);

    // copy stdin
    pipe(pfdi);
    dup2(pfdi[0], 0);

    initialized = 1;
}

JNIEXPORT jbyteArray JNICALL
Java_org_call_1cc_android_template_repl_MainActivity_read(JNIEnv *env, jclass type) {
    ssize_t rdsz;
    jbyte buf[1024];
    if((rdsz = read(pfd[0], buf, sizeof buf - 1)) > 0) {
        jbyteArray ret = (*env)->NewByteArray(env, rdsz); // create byte[] object
        (*env)->SetByteArrayRegion(env, ret, 0, rdsz, buf); // copy
        return ret;
    }
    return 0; // eof
}

JNIEXPORT void JNICALL
Java_org_call_1cc_android_template_repl_MainActivity_write(JNIEnv *env, jobject instance, jbyteArray x_) {
    jbyte *x = (*env)->GetByteArrayElements(env, x_, NULL);
    jsize num_bytes = (*env)->GetArrayLength(env, x_);

    write(pfdi[1], x, num_bytes);

    (*env)->ReleaseByteArrayElements(env, x_, x, 0);
}

/**
 * This is run in its own Java Thread and it meant to block (like a REPL).
 * Output to stdout is redirected to the app UI so it's almost like a terminal.
  */
JNIEXPORT jint JNICALL
Java_org_call_1cc_android_template_repl_MainActivity_launch(JNIEnv *env, jobject instance,
                                                      jstring nativeLibraryDir_) {
    const char *nativeLibraryDir = (*env)->GetStringUTFChars(env, nativeLibraryDir_, 0);
    int ret = app(nativeLibraryDir);
    (*env)->ReleaseStringUTFChars(env, nativeLibraryDir_, nativeLibraryDir);
    return ret;
}
