package org.call_cc.android.template.repl;

import android.content.Context;
import android.graphics.Canvas;
import android.text.SpannableStringBuilder;
import android.util.AttributeSet;
import android.widget.TextView;

// TODO: move EscapeSequence handling here
public class OutputTextView extends TextView {
    public OutputTextView(Context context) {
        super(context);
    }

    public OutputTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public OutputTextView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public OutputTextView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    // default behaviour is to force the scroll position so that the cursor is visible - I think
    // there is a pretend cursor position when we click on "ClickableSpans".
    // this causes autoscrolling to fail because the TextView is forcing the scroll back up to
    // the last clicked span.
    @Override
    public boolean bringPointIntoView(int offset) {
        return false;
    }

    @Override
    protected void onDraw(Canvas canvas) {
        long nt = System.nanoTime();
        super.onDraw(canvas);
        nt = System.nanoTime() - nt;
        if(nt > 1000000* 20/*ms*/ ) {
            CharSequence t = getText();
            if(t.length() > 1024* 10/*KiB*/) {
                if (t instanceof SpannableStringBuilder) {
                    SpannableStringBuilder ssb = (SpannableStringBuilder) t;
                    // we're taking too long to draw text, probably because it's too much text there.
                    // so we delete half of it. simple and stupid, but keeps the FPS from dropping.
                    int start = (ssb.length()/2);
                    int pos = start;
                    for( ; pos < start+1024; pos++ )
                        if(ssb.charAt(pos) == '\n')
                            break;
                    ssb.delete(0, pos);
                }
            }
        }
    }


}
