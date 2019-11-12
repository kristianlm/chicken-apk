package org.call_cc.template.sotest;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.widget.ScrollView;

public class AutoScroller extends ScrollView {
    private static final String TAG = "MyScroll";
    boolean scrolling = false;
    boolean autoscroll = true;

    public abstract class ACircle {
        final public Paint p = new Paint();
        public ACircle() {
            p.setColor(0x40FFFFFF);
        }
        abstract int x(); // screen coords (ignores scrolling)
        abstract int y();
        abstract int r();
        boolean inside(int x0, int y0) {
            x0 -= x();
            y0 -= y();
            int r2 = (x0*x0) + (y0*y0);
            if(r2 < r()*r())  return true;
            else return false;
        }
        public void draw(Canvas canvas) {
            canvas.drawCircle(getScrollX()+x(), getScrollY()+y(), r(), p);
        }
    }

    ACircle circleTop = new ACircle() {
        @Override int r() {return (getWidth()>getHeight()?getHeight():getWidth())/10;}
        @Override int x() {return r();}
        @Override int y() {return r();}
    };
    ACircle circleBottom = new ACircle() {
        @Override int r() {return circleTop.r(); }
        @Override int x() {return r();}
        @Override int y() {return getHeight()-r();}
    };

    public AutoScroller(Context context) {
        super(context);
    }

    public AutoScroller(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public AutoScroller(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }
    public AutoScroller(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        autoscroll = false;
        scrolling = true;
        if(ev.getAction() == MotionEvent.ACTION_UP)
            scrolling = false;

        if(circleTop.inside((int)ev.getX(), (int)ev.getY())) {
            if(ev.getAction() == MotionEvent.ACTION_UP) {
                scrollToTop();
            }
            circleTop.p.setColor(0x40FF00FF);
        } else {
            circleTop.p.setColor(0x40FFFFFF);
        }

        if(circleBottom.inside((int)ev.getX(), (int)ev.getY())) {
            if(ev.getAction() == MotionEvent.ACTION_UP) {
                scrollToBottom();
                autoscroll = true;
            }
            circleBottom.p.setColor(0x40FF00FF);
        } else {
            circleBottom.p.setColor(0x40FFFFFF);
        }
        return super.onTouchEvent(ev);
    }

    public void scrollToTop() {
        clearAnimation(); // I wish I could mScroll.finish() but no charm
        scrollTo(getScrollX(), 0);
    }
    public void scrollToBottom() {
        clearAnimation();
        scrollTo(getScrollX(), getChildAt(getChildCount()-1).getBottom() - getHeight());
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        if(autoscroll) {
            scrollToBottom();
        }
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }


    @Override
    protected void onDraw(Canvas canvas) {
        if(scrolling) {
            circleTop.draw(canvas);
            circleBottom.draw(canvas);
        }
        super.onDraw(canvas);
    }
}
