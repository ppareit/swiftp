# Compile-time only annotations that R8 cannot resolve. They are never needed at
# runtime, so suppressing the warnings is safe.
# Tink, pulled in by androidx.security-crypto:
-dontwarn javax.annotation.Nullable
-dontwarn javax.annotation.concurrent.GuardedBy
# Dropped from com.twofortyfouram:android-annotation in 3.0.0, still referenced by
# android-spackle:
-dontwarn com.twofortyfouram.annotation.VisibleForTesting

# Fix missing constructor error
-keepclassmembers,allowobfuscation class be.ppareit.swiftp.server.* {
  <init>(...);
}

# Fix missing type error
-keep class com.google.gson.reflect.TypeToken
-keep class * extends com.google.gson.reflect.TypeToken
-keep public class * implements java.lang.reflect.Type
