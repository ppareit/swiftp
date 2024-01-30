# Fix missing constructor error
-keepclassmembers,allowobfuscation class be.ppareit.swiftp.server.* {
  <init>(...);
}