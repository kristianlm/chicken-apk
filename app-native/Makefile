
APKFINAL=../chicken-repl.apk
APKTEMPLATE=../template/app/build/outputs/apk/debug/app-debug.apk

all: ${APKFINAL}

# libchicken and friends
lib/x86/libchicken.so:
	docker run --rm -v "${PWD}/lib/armeabi-v7a/:/out" chicken/android sh -c \
		'cp /target/android21/armv7a/lib/chicken/11/*.so \
                   /target/android21/armv7a/lib/libchicken.so \
                   /out/'
	sudo chown "klm:klm" lib -R # TODO: fixme properly	

# libchicken and friends
lib/armeabi-v7a/libchicken.so:
	docker run --rm -v "${PWD}/lib/armeabi-v7a/:/out" chicken/android sh -c \
		'cp /target/android21/armv7a/lib/chicken/11/*.so \
                   /target/android21/armv7a/lib/libchicken.so \
                   /out/'
	sudo chown "klm:klm" lib -R # TODO: fixme properly	

lib/x86/libapp.so: app.scm lib/x86/libchicken.so
	docker run -it --rm -v "${PWD}:/share" chicken/android \
	    /host/android21/i686/bin/csc /share/app.scm -L -llog -shared \
	    -o /share/lib/x86/libapp.so

lib/armeabi-v7a/libapp.so: app.scm lib/armeabi-v7a/libchicken.so
	docker run -it --rm -v "${PWD}:/share" chicken/android \
            /host/android21/armv7a/bin/csc -v /share/app.scm -L -llog -shared \
            -o /share/lib/armeabi-v7a/libapp.so

## TODO: find a way to say that you want arm7, x86 but not arm64 easily.
## now it's a mess
${APKFINAL}: build.scm ${APKTEMPLATE} lib/armeabi-v7a/libapp.so lib/armeabi-v7a/libchicken.so lib/x86/libapp.so
	csi -s build.scm
