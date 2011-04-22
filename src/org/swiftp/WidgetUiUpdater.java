package org.swiftp;

import java.util.HashSet;

import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.widget.RemoteViews;


/**
 * This class maintains Widget Updates. It obtains information about UI Updates from UiUpdater.class.
 * For starting getting UiUpdates, method {@link #registerAtUiUpdater()} have to be executed once.
 * 
 * Why does this Class exist?
 * This class exists, because I was not able to store {@link #handler} for every single Widget. 
 * There was a Problem with unregistering handlers, when a Widget was deleted. 
 */
public class WidgetUiUpdater {
	private static HashSet<Integer> widgetIds = new HashSet<Integer>();
	private static Context context;
	private static AppWidgetManager appWidgetManager;
	
    private static Handler handler = new Handler() {
    	public void handleMessage(Message msg) {
    		switch(msg.what) {
    		case 0:  // We are being told to do a UI update
    			// If more than one UI update is queued up, we only need to do one.
    			removeMessages(0);
    			updateWidgetUi();
    			break;
    		case 1:  // We are being told to display an error message
    			removeMessages(1);
    		}
    	}
    };

	
    /**
     * Registers new Widgets, for updating them when needed. UiUpdater sends UI update messages.
     * @param newWidgetIds
     * @param newContext
     * @param newAppWidgetManager
     */
	protected static void registerWidgets(int[] newWidgetIds, Context newContext, AppWidgetManager newAppWidgetManager){
		context = newContext;
		appWidgetManager = newAppWidgetManager;
		
		for(int newWidgetId:newWidgetIds){
			widgetIds.add(new Integer(newWidgetId));
		}
	}
	
	/**
	 * Unregisters deleted Widgets.
	 * @param newWidgetIds
	 */
	protected static void unregisterWidgets(int[] newWidgetIds){
		
		if(!widgetIds.isEmpty()){
			for(int newWidgetId: newWidgetIds){
				widgetIds.remove(new Integer(newWidgetId));
			}
		}
		
	}
	
	/**
	 * Unregister all Widgets on the List.
	 */
	protected static void unregisterAllWidgets(){
		widgetIds.clear();
	}
	
	/**
	 * Start listening for UI Updates, to know, when updating the Widgets
	 */
	protected static void registerAtUiUpdater(){
		UiUpdater.registerClient(handler);
	}
	
	/**
	 * Stop listening for UI Updates.
	 */
	protected static void unregisterAtUiUpdater(){
		UiUpdater.unregisterClient(handler);
	}

	/**
	 * Updates all Widgets, when static UiUpdater (Observer Pattern) tells to.
	 */
	private static void updateWidgetUi(){	

		//tell all Widgets, to Update themselves. Right State is set in the onUpdate handler.
		RemoteViews remoteViews = new RemoteViews(context.getPackageName(), R.layout.widget);
		
		//set the right state, according to the FTP Server
		if(FTPServerService.isRunning()){
			remoteViews.setViewVisibility(R.id.widget_button_on, View.VISIBLE);
			remoteViews.setViewVisibility(R.id.widget_button_off, View.GONE);
		}else{
			remoteViews.setViewVisibility(R.id.widget_button_on, View.GONE);
			remoteViews.setViewVisibility(R.id.widget_button_off, View.VISIBLE);
		}
			
		for(Integer widgetId:widgetIds){
			appWidgetManager.updateAppWidget(widgetId.intValue(), remoteViews);
		}
		
	}

}
