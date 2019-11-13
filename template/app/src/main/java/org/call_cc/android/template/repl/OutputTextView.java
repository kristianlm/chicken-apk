package org.call_cc.android.template.repl;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

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
}
