package org.swiftp;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.Context;
import android.content.Intent;
import android.view.View;
import android.widget.RemoteViews;

/**
 * Class handles Widget Events.
 * 
 */
public class WidgetProvider extends AppWidgetProvider{

	public static String ACTION_WIDGET_BUTTON = "actionWidgetButton";
		
    
	@Override
	public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
		
//		Log.d("MyActivity", "onUpdate");
//		Toast.makeText(context, "onUpdate", Toast.LENGTH_SHORT).show();
		
		//register new Widgets, for them to be Updated by WidgetUiUpdater
		WidgetUiUpdater.registerWidgets(appWidgetIds, context, appWidgetManager);
		
		
		//add ButtonListener
		RemoteViews remoteViews = new RemoteViews(context.getPackageName(), R.layout.widget);
		
		Intent active = new Intent(context, WidgetProvider.class);
		active.setAction(ACTION_WIDGET_BUTTON);
		
		PendingIntent actionPendingIntent = PendingIntent.getBroadcast(context, 0, active, 0);	
		remoteViews.setOnClickPendingIntent(R.id.widget_button_off, actionPendingIntent);
		remoteViews.setOnClickPendingIntent(R.id.widget_button_on, actionPendingIntent);


		
		//set the right state, according to the FTP Server
		if(FTPServerService.isRunning()){
			remoteViews.setViewVisibility(R.id.widget_button_on, View.VISIBLE);
			remoteViews.setViewVisibility(R.id.widget_button_off, View.GONE);
		}else{
			remoteViews.setViewVisibility(R.id.widget_button_on, View.GONE);
			remoteViews.setViewVisibility(R.id.widget_button_off, View.VISIBLE);
		}		
		appWidgetManager.updateAppWidget(appWidgetIds, remoteViews);
		
		super.onUpdate(context, appWidgetManager, appWidgetIds);
	}
	
	@Override
	public void onReceive(Context context, Intent intent) {		
		
//		Log.d("MyActivity", "onReceive");
//		Toast.makeText(context, "onReceive, Action: "+ intent.getAction(), Toast.LENGTH_SHORT).show();
		
		if( intent.getAction().equals(ACTION_WIDGET_BUTTON) ){
			
			/*
			 * After handling onReceive this Object will be destroyed by the OS.
			 * BroadcastReceivers and WidgetProviders shouldn't handle asynchronous actions, like UI Updates.  
			 */
			
			//start or stop FTP Service 
    		Intent intentFTP = new Intent(context,	FTPServerService.class);
			
    		if(!FTPServerService.isRunning()) {
    			context.startService(intentFTP);
    		}
    		else {
    			context.stopService(intentFTP);
    		}
			UiUpdater.updateClients();
		}
		super.onReceive(context, intent);
	}
	
	@Override
	public void onDeleted(Context context, int[] appWidgetIds) {	
		
//		Log.d("MyActivity", "onDeleted");
//		Toast.makeText(context, "onDeleted", Toast.LENGTH_SHORT).show();
		
		WidgetUiUpdater.unregisterWidgets(appWidgetIds);
		super.onDeleted(context, appWidgetIds);
	}
	
	@Override
	public void onEnabled(Context context) {
		
//		Log.d("MyActivity", "onEnabled");
//		Toast.makeText(context, "onEnabled", Toast.LENGTH_SHORT).show();
		
		//register the WidgetUiUpdater for the UiUpdater Messages
		WidgetUiUpdater.registerAtUiUpdater();
		super.onEnabled(context);
	}
	
	@Override
	public void onDisabled(Context context) {
		
//		Log.d("MyActivity", "onDisabled");
//		Toast.makeText(context, "onDisabled", Toast.LENGTH_SHORT).show();
		
		//unregister the WidgetUiUpdater from the UiUpdater Messages
		WidgetUiUpdater.unregisterAtUiUpdater();
		
		//and clean the WidgetIds list, if it is still not empty
		WidgetUiUpdater.unregisterAllWidgets();
		super.onDisabled(context);
	}
	

}
