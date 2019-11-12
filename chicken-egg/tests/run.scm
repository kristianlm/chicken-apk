(import test (android bxml) (only chicken.blob blob->string string->blob))


(test-group
 "sxml <-> ibax"

 (define sxml
   `(@ns (tens "http://com.example/tens")
         (A (@ (tens att1 "one")
               (att2 "two"))
            (A-A (@)))))

 (define ibax
   `((string-pool utf16 ("tens" "http://com.example/tens" "att1" "one" "att2" "two" "A" "A-A"))
     (resource-map ())
     (<ns> (str 0) (str 1))
     (<element> (str #f) (str 6) (@ ((str 1) (str 2) (str 3)) ((str #f) (str 4) (str 5))))
     (<element> (str #f) (str 7) (@))
     (</element> (str #f) (str 7))
     (</element> (str #f) (str 6))
     (</ns> (str 0) (str 1))))

 (test ibax (sxml->ibax sxml))
 (test sxml (ibax->sxml ibax)))


(test-group
 "ibax <-> string"

 ;; taken from AndroidManifest.xml inside some a small test apk file
 ;; which was built with standard tooling.
 (define bxml
   (blob->string
    #${030008000808000001001c00880400001f0000000000000000000000980000000000000000000000
       0e0000001a000000260000003e0000005c0000007600000090000000b4000000ce000000e8000000
       fe000000240100005a010000640100006a0100007a0100008e010000a0010000d80100001c020000
       360200004a020000a2020000c0020000d40200000e0300006203000074030000a8030000dc030000
       05006c006100620065006c0000000400690063006f006e00000004006e0061006d00650000000a00
       640065006200750067006700610062006c00650000000d006d0069006e00530064006b0056006500
       7200730069006f006e0000000b00760065007200730069006f006e0043006f006400650000000b00
       760065007200730069006f006e004e0061006d006500000010007400610072006700650074005300
       64006b00560065007200730069006f006e0000000b0061006c006c006f0077004200610063006b00
       7500700000000b0073007500700070006f00720074007300520074006c000000090072006f007500
       6e006400490063006f006e000000110063006f006d00700069006c006500530064006b0056006500
       7200730069006f006e000000190063006f006d00700069006c006500530064006b00560065007200
       730069006f006e0043006f00640065006e0061006d0065000000030031002e003000000001003900
       0000060061006300740069006f006e00000008006100630074006900760069007400790000000700
       61006e00640072006f006900640000001a0061006e00640072006f00690064002e0069006e007400
       65006e0074002e0061006300740069006f006e002e004d00410049004e000000200061006e006400
       72006f00690064002e0069006e00740065006e0074002e00630061007400650067006f0072007900
       2e004c00410055004e00430048004500520000000b006100700070006c0069006300610074006900
       6f006e0000000800630061007400650067006f007200790000002a0068007400740070003a002f00
       2f0073006300680065006d00610073002e0061006e00640072006f00690064002e0063006f006d00
       2f00610070006b002f007200650073002f0061006e00640072006f006900640000000d0069006e00
       740065006e0074002d00660069006c00740065007200000008006d0061006e006900660065007300
       740000001b006f00720067002e00630061006c006c005f00630063002e00740065006d0070006c00
       6100740065002e0073006f007400650073007400000028006f00720067002e00630061006c006c00
       5f00630063002e00740065006d0070006c006100740065002e0073006f0074006500730074002e00
       4d00610069006e0041006300740069007600690074007900000007007000610063006b0061006700
       65000000180070006c006100740066006f0072006d004200750069006c0064005600650072007300
       69006f006e0043006f00640065000000180070006c006100740066006f0072006d00420075006900
       6c006400560065007200730069006f006e004e0061006d0065000000080075007300650073002d00
       730064006b000000800108003c0000000100010102000101030001010f0001010c0201011b020101
       1c0201017002010180020101af0301012c0501017205010173050101000110001800000002000000
       ffffffff110000001600000002011000b000000002000000ffffffffffffffff1800000014001400
       07000000000000001600000005000000ffffffff080000100100000016000000060000000d000000
       080000030d000000160000000b000000ffffffff080000101c000000160000000c0000000e000000
       080000030e000000ffffffff1b000000190000000800000319000000ffffffff1c000000ffffffff
       080000101c000000ffffffff1d000000ffffffff0800001009000000020110004c00000007000000
       ffffffffffffffff1e0000001400140002000000000000001600000004000000ffffffff08000010
       150000001600000007000000ffffffff080000101c000000030110001800000007000000ffffffff
       ffffffff1e000000020110009c0000000b000000ffffffffffffffff140000001400140006000000
       000000001600000000000000ffffffff080000010000067f1600000001000000ffffffff08000001
       0000057f1600000003000000ffffffff08000012ffffffff1600000008000000ffffffff08000012
       ffffffff1600000009000000ffffffff08000012ffffffff160000000a000000ffffffff08000001
       0100057f020110003800000012000000ffffffffffffffff10000000140014000100000000000000
       16000000020000001a000000080000031a000000020110002400000013000000ffffffffffffffff
       17000000140014000000000000000000020110003800000014000000ffffffffffffffff0f000000
       14001400010000000000000016000000020000001200000008000003120000000301100018000000
       14000000ffffffffffffffff0f000000020110003800000016000000ffffffffffffffff15000000
       14001400010000000000000016000000020000001300000008000003130000000301100018000000
       16000000ffffffffffffffff15000000030110001800000013000000ffffffffffffffff17000000
       030110001800000012000000ffffffffffffffff1000000003011000180000000b000000ffffffff
       ffffffff14000000030110001800000002000000ffffffffffffffff180000000101100018000000
       02000000ffffffff1100000016000000}))

 (define ibax
   `((string-pool utf16 ("label" "icon" "name" "debuggable" "minSdkVersion" "versionCode"
                         "versionName" "targetSdkVersion" "allowBackup" "supportsRtl" "roundIcon"
                         "compileSdkVersion" "compileSdkVersionCodename" "1.0" "9" "action" "activity"
                         "android" "android.intent.action.MAIN" "android.intent.category.LAUNCHER"
                         "application" "category" "http://schemas.android.com/apk/res/android"
                         "intent-filter" "manifest" "org.call_cc.template.sotest"
                         "org.call_cc.template.sotest.MainActivity" "package"
                         "platformBuildVersionCode" "platformBuildVersionName" "uses-sdk"))
     (resource-map (16842753 16842754 16842755 16842767 16843276 16843291 16843292
                             16843376 16843392 16843695 16844076 16844146 16844147))
     (<ns> (str 17) (str 22))
     (<element> (str #f) (str 24) (@ ((str 22) (str 5) 1)
                                     ((str 22) (str 6) (str 13))
                                     ((str 22) (str 11) 28)
                                     ((str 22) (str 12) (str 14))
                                     ((str #f) (str 27) (str 25))
                                     ((str #f) (str 28) 28)
                                     ((str #f) (str 29) 9)))
     (<element> (str #f) (str 30) (@ ((str 22) (str 4) 21)
                                     ((str 22) (str 7) 28)))
     (</element> (str #f) (str 30))
     (<element> (str #f) (str 20) (@ ((str 22) (str 0) (ref 2131099648))
                                     ((str 22) (str 1) (ref 2131034112))
                                     ((str 22) (str 3) #t)
                                     ((str 22) (str 8) #t)
                                     ((str 22) (str 9) #t)
                                     ((str 22) (str 10) (ref 2131034113))))
     (<element> (str #f) (str 16) (@ ((str 22) (str 2) (str 26))))
     (<element> (str #f) (str 23) (@))
     (<element> (str #f) (str 15) (@ ((str 22) (str 2) (str 18))))
     (</element> (str #f) (str 15))
     (<element> (str #f) (str 21) (@ ((str 22) (str 2) (str 19))))
     (</element> (str #f) (str 21))
     (</element> (str #f) (str 23))
     (</element> (str #f) (str 16))
     (</element> (str #f) (str 20))
     (</element> (str #f) (str 24))
     (</ns> (str 17) (str 22))))

 (test ibax (bxml->ibax bxml))
 ;; I would love to test that bxml = ibax->bxml ibax, but no can do
 ;; because line numbers and comments are ignored.
 (test ibax (bxml->ibax (ibax->bxml ibax)))

 )


(import test (android minizip)
        (only (chicken file) delete-file*)
        (only (chicken port) with-output-to-port)
        (only (chicken io) read-string))

(test-group
 "minizip"

 (define filename "test-5926f235c0f0c4493cc48c82a6a73f4bcd2940bf.zip")
 (delete-file* filename)

 (let ((z (zipper filename)))
   (with-output-to-port (zipper-new z "a.txt")
     (lambda () (display "file a\n")))
   (zipper-new z "b.txt" #:method 'none) (zipper-write z "file b\n")
   (zip-close z))

 (let ((uz (unzipper filename)))
   (let ((port (unzipper-next uz)))
     (test "a.txt" (unzipper-filename uz))
     (test 'deflated   (unzipper-method uz))
     (test "file a\n" (read-string #f port)))
   (let ((port (unzipper-next uz)))
     (test "b.txt" (unzipper-filename uz))
     (test 'none (unzipper-method uz))
     (test "file b\n" (read-string #f port)))))

(test-exit)
