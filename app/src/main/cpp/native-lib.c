#include <jni.h>
#include <string.h>

JNIEXPORT jstring JNICALL
        Java_org_call_1cc_template_sotest_MainActivity_eval(
        JNIEnv *env,
        jobject this, jstring jinput) {
    char msg[1024] = "from jni! input is: ";
    const char* input = (*env)->GetStringUTFChars(env, jinput, NULL);
    strcat(msg, input);
    (*env)->ReleaseStringUTFChars(env, jinput, input);
    jstring result = (*env)->NewStringUTF(env, msg);
    return result;
}
