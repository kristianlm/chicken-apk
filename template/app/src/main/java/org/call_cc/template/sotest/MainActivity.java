package org.call_cc.template.sotest;

import android.app.Activity;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.TextPaint;
import android.text.method.LinkMovementMethod;
import android.text.style.ClickableSpan;
import android.util.Log;
import android.view.DragEvent;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Button;
import android.widget.ScrollView;
import android.widget.TextView;

import java.io.File;

public class MainActivity extends Activity {

    private static final String TAG = "MainAcvitivy";
    private Thread thRead;
    private Thread thExec;

    TextView viewTxt;
    TextView viewIn;
    Button viewSnd;
    ScrollView viewScroll;

    static {
        // we need this first so that we can get access to all our JNI functions.
        System.loadLibrary("template");
    }

    private void append(String s) {
        append(new SpannableString(s));
    }
    private void append(final SpannableString s) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                viewTxt.append(s);

                int gap = ((viewTxt.getBottom()) - viewScroll.getScrollY() - viewScroll.getHeight());
                if(gap < viewTxt.getLineHeight()*6) { scrollDown(); }
                // make things fancy:
               er.cont((SpannableStringBuilder) viewTxt.getText());
            }
        });
    }

    private void scrollDown() {
        //viewScroll.setSmoothScrollingEnabled(false);
        //viewScroll.fullScroll(View.FOCUS_DOWN);
    }
//
//    private SpannableString linkCode(final String title, final String code) {
//        SpannableString txt = new SpannableString(title);
//        txt.setSpan(new ClickableSpan() {
//            @Override
//            public void onClick(View widget) {
//                //Toast.makeText(getApplicationContext(), "click click!", Toast.LENGTH_LONG).show();
//                viewIn.setText(code);
//            }
//        }, 0, txt.length(), 0);
//        return txt;
//    }
//
//
//    private SpannableString linkCode(final String code) {
//        return linkCode(code, code);
//    }

    public class EscapeReader {

        int pos = 0;
        int codeStart = 0;
        int titleStart = 0;

        void cont(final SpannableStringBuilder ss) {
            //Log.d(TAG, "cont: pos=" + pos + " " + );
            for( ; pos+3 < ss.length() ; pos++) {
                if(ss.charAt(pos) == '\r') {
                    int i = pos;
                    while(true) {
                        if (ss.charAt(i) == '\n') { i++; break; }
                        else if(i <= 0) break;
                        else i--;
                    }
                    ss.delete(i, pos); pos = i;
                } else if(ss.charAt(pos) == 0x001b) {
                    Log.d(TAG, "cont: " + ss.charAt(pos) + "" + ss.charAt(pos+1) + "" + ss.charAt(pos+2) + "" + ss.charAt(pos+3));
                    //if(pos+3 >= ss.length()) { pos -- ; return; } // can't continue => restart from 0x1b
                    switch(ss.charAt(pos+1)) {
                        // TODO: basic color ascii escape handling here
                        // TODO: handle carriage return
                        // TODO: handle clear screen
                        case '}': {
                            // TODO: get second } too
                            int start = codeStart;
                            final String code = ss.subSequence(codeStart, pos).toString();
                            if(titleStart > 0) {
                                // remove code from output and just
                                // since codeStart~pos is before pos, we must move pos along with us
//                                Log.d(TAG, "cont: delete4 len " + (pos - codeStart));
                                ss.delete(codeStart, pos); pos = codeStart;

                                start = titleStart;
                            }

//                            Log.d(TAG, "cont: delete 1 " + ss.charAt(pos+1) + "…" + ss.charAt(pos+2) + "…" + ss.charAt(pos+3));
                            ss.delete(pos, pos+3);

                            ss.setSpan(new ClickableSpan() {
                                @Override
                                public void onClick(View widget) {
                                    viewIn.setText(code);
                                }

                                @Override
                                public void updateDrawState(TextPaint ds) {
                                    super.updateDrawState(ds);
                                    ds.setUnderlineText(false);
                                }
                            }, start, pos, 0);

                            titleStart = 0;
                            break;
                        }
                        case '{':
                            switch (ss.charAt(pos+2)) {
                                case '#': // ␛{# => set title
                                    titleStart = codeStart;
                                    codeStart = pos;
                                    //ss.delete(pos, pos+3);
                                case '{': // // ␛{{ => new code block
                                    codeStart = pos;
//                                    Log.d(TAG, "cont: delete 2");
                                    ss.delete(pos, pos+3);
                                    break;
                                default:
                            }
                            break;
                        default:
                    }
                }
            }
        }
    }

    EscapeReader er = new EscapeReader();

    void loadLibraryMaybe(String lib) {
        if(new File(getApplicationInfo().nativeLibraryDir + "/lib" + lib + ".so").exists()) System.loadLibrary(lib);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        getActionBar().hide();

        iosetup();

        // note template.so loaded statically
        //loadLibraryMaybe("app");      // libapp.so is loaded by libtemplate.so
        loadLibraryMaybe("init");     // just in case you need something extra
        loadLibraryMaybe("chicken");  // this should start first

        viewTxt = findViewById(R.id.out);
        viewIn  = findViewById(R.id.in);
        viewScroll = findViewById(R.id.scroll);
        viewSnd = findViewById(R.id.snd);

        viewTxt.setText("", TextView.BufferType.EDITABLE);
        viewTxt.setMovementMethod(LinkMovementMethod.getInstance());

        final View v = findViewById(R.id.glider);

        // allow resizing viewIn
        v.setOnTouchListener(new View.OnTouchListener() {
            float last = 0;
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                Log.d(TAG, "onTouch: " + event);
                if(event.getAction() == MotionEvent.ACTION_DOWN) {
                    last = event.getRawY();
                } else if(event.getAction() == MotionEvent.ACTION_MOVE) {
                    final int d = (int) (event.getRawY() - last); last = event.getRawY();
                    viewIn.setHeight(viewIn.getHeight() + d);
                    //last += v.getY() - viewIn.getBottom();
                    v.setY(v.getY() + d);
                } else if(event.getAction() == MotionEvent.ACTION_UP) {
                    final View vv = v;
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            vv.setY(viewIn.getBottom());
                        }
                    });
                }
                return false;
            }
        });


        viewIn.setText("(import srfi-18 nrepl)\n(thread-start! (lambda () (nrepl 8888)))");
        v.post(new Runnable() {
            @Override
            public void run() {
                v.setY(viewIn.getBottom());
            }
        });
        thRead = new Thread(new Runnable() {
            @Override
            public void run() {
                while(true) {
                    append(new SpannableString(new String(read())));
                }
            }
        });
        thRead.start();

        final String libDir = getApplicationInfo().nativeLibraryDir;
        thExec = new Thread(new Runnable() {
            @Override
            public void run() {
                final int ret = launch(libDir);
                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        append(new SpannableString("<thread exited with " + ret + ">\n"));
                        // TODO: disable send button
                    }
                });
            }
        });
        thExec.start();
    }

    private int inputcount = 0;
    public void onSend(View view) {
        final String src = viewIn.getText().toString();
        append("\u001b{{;#<" + inputcount++ + ">\u001b{#" + src + "\u001b}}\n");
        write(src.getBytes());
        write(new byte[]{'\n'}); // newline
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                viewSnd.setText(inputcount-1 + ">");
                scrollDown();
            }
        });
    }

    public native void iosetup();
    public native int launch(String nativeLibraryDir);
    public static native byte[] read();
    public native void write(byte[] x);
}
