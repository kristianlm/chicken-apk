
## Chicken `repl` template app

This builds an empty skeleton .apk file. It has:

- A poor-man's terminal-like GUI
- A native component (libtemplate.so) to bridge the GUI with the apps' own stdin/stdout (pipe/dup2) 
- A dummy native component (libapp.so) that says hello and things

If you just want to play with Chicken, you don't have to build this
since this requires the Android SDK and NDK and this can be painful.

## Building

```
$ cat local.properties # replace with your paths
ndk.dir=/home/user/opt/android/sdk/ndk-bundle
sdk.dir=/home/user/opt/android/sdk
$ ./gradlew assembleDebug # this Should Just Work™
```
