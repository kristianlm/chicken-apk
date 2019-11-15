  [Chicken Scheme]: http://call-cc.org
  [Chicken]: http://call-cc.org
  [GoDot]: http://godotengine.org
  [Docker]: http://docker.com

# CHICKEN Android Template

This project aims to make Android development with [Chicken Scheme]
easier. There have been numerous [prior
attempts](https://github.com/chicken-mobile/chicken-sdl2-android-builder)
at this as this process is considerably clunkier than it should
be. Typically,

1. You need to start off with an Android skeleton
2. You need basic JNI interface
3. The natvie side must initialize the CHICKEN runtime properly (and set repository-home)
4. You have to prefix all `.so` files under `libs/` with `lib`
5. You must generally cross-compile CHICKEN (for each ARCH)
6. You must cross-compile the eggs (for each ARCH)
7. You must cross-compile your own user code (for each ARCH)

This project tries to address 1-6 for you. The approach is perhaps a
little radical, and totally stoken from [GoDot]: We build a single
Android app (`template.apk`) and reuse this for all Chicken projects
on Android. We replace the native parts of the template
(replacing/adding `.so` files) with our own [Chicken] native part and
then modify the package name in the manifest (TODO).

## `template`

The template is the Android app that's intended to be reused for each
[Chicken] Android project.

It currently only has one activity: a REPL interface that interacts
with stdin/stdout. Think of this as a poor implementation of a
terminal emulator. This template could in the future also have an
`SDL2` activity for more serious [Chicken] apps.

The idea is that the template has an Activity for everything you need,
and you remove the ones you don't want at build-time (modify maniffest
still TODO).

To simplify things, this first build stage have been split up and does
not know anything about [Chicken]. If you don't need to modify (or
more likely, fix) the template, you don't need to build this part. If
you do, this stage needs both the Android SDK and the Android NDK.

## `chicken-egg`

This part of the project provides tools to work with apk files. It
lets you re-compress an `.apk` into a new one, modifying the
`AndroidManifest.xml` and adding `.so` in the process. Unfortunately,
modifying the `AndroidManifest.xml` is extremely complicated because
it involves a poorly documented binary XML format with even less
documented semantics (what is the resource-pool, anyone?).

The modules from this is needed by `app-native`. It is not needed in
the target app.

## `app-native`

This part takes the template and fills it up with [Chicken] eggs,
using a cross Chicken for each ARCH (arm7, x86 etc). It needs the
Android NDK, but not the Android SDK, and uses [Docker] to acheive
this.

