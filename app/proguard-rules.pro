# Fix missing constructor error
-keepclassmembers,allowobfuscation class be.ppareit.swiftp.server.* {
  <init>(...);
}

# Fix missing type error
-keep class com.google.gson.reflect.TypeToken
-keep class * extends com.google.gson.reflect.TypeToken
-keep public class * implements java.lang.reflect.Type
