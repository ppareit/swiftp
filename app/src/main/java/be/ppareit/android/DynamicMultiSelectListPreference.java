package be.ppareit.android;

import android.content.Context;
import android.preference.MultiSelectListPreference;
import android.util.AttributeSet;
import android.view.View;

import net.vrallev.android.cat.Cat;

/**
 * Created by ppareit on 31/12/15.
 */
public class DynamicMultiSelectListPreference extends MultiSelectListPreference {

    OnPopulateListener mPopulateListener;

    public DynamicMultiSelectListPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    protected View onCreateDialogView() {
        Cat.d("Creating dialog view");

        mPopulateListener.onPopulate(this);

        return super.onCreateDialogView();
    }

    public void setOnPopulateListener(OnPopulateListener listener) {
        mPopulateListener = listener;
    }

    public interface OnPopulateListener {
        void onPopulate(DynamicMultiSelectListPreference preference);
    }
}
