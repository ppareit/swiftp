# Make sure certain things are not dumped into logcat for signed builds eg password
-assumenosideeffects class android.util.Log {
    public static boolean isLoggable(java.lang.String, int);
    public static *** d(...);
    public static *** v(...);
    public static *** i(...);
    public static *** w(...);
    public static *** e(...);
}
-assumenosideeffects class net.vrallev.android.cat.Cat {
    public static *** d(...);
    public static *** v(...);
    public static *** i(...);
    public static *** w(...);
    public static *** e(...);
}