/*******************************************************************************
 * Copyright (c) 2012-2017 Pieter Pareit.
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * <p>
 * Contributors:
 * Pieter Pareit - initial API and implementation
 ******************************************************************************/

package be.ppareit.android;

import android.content.Context;
import android.preference.MultiSelectListPreference;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;


/**
 * This is the same as the sdk MultiSelectListPreference but each time
 * it shows, it will repopulate the list. Use the setOnPopulateListener()
 * to attach a listener.
 */
public class DynamicMultiSelectListPreference extends MultiSelectListPreference {

    private OnPopulateListener mPopulateListener;

    public DynamicMultiSelectListPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    protected View onCreateDialogView() {
        Log.d("swiftp", "Creating dialog view");

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
