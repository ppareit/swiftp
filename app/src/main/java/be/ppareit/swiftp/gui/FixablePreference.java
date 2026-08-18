/*
Copyright 2026 Pieter Pareit

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
 */

package be.ppareit.swiftp.gui;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;

import androidx.preference.Preference;
import androidx.preference.PreferenceViewHolder;

import be.ppareit.swiftp.R;

/** A settings row with a FIX button on the right while its setting is in a bad state. */
public class FixablePreference extends Preference {

    private boolean mShowFix = false;

    public FixablePreference(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public FixablePreference(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    /** Shows or hides the button, rebinding the row when that changes something. */
    public void setShowFix(boolean showFix) {
        if (mShowFix == showFix) return;
        mShowFix = showFix;
        notifyChanged();
    }

    @Override
    public void onBindViewHolder(PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);
        View fix = holder.findViewById(R.id.preference_fix);
        if (fix != null) fix.setVisibility(mShowFix ? View.VISIBLE : View.GONE);
    }
}
