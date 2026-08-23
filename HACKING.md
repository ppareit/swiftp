# Contributing to SwiFTP

## Development setup

SwiFTP is an Android application built with Gradle. To work on it, install:

- Android Studio with the Android SDK
- Git

The build uses the checked-in Gradle wrapper and a Java 21 toolchain. Gradle can
download the matching toolchain when necessary, so no IDE plugins are required.

Fork and clone the repository, then open the repository root in Android Studio:

```shell
git clone git@github.com:<your-account>/swiftp.git
cd swiftp
```

Allow Android Studio to complete the Gradle sync and install any missing Android
SDK components it requests.

## Build variants

The app has two product flavors:

| Flavor | Application ID | Purpose |
| --- | --- | --- |
| `fdroid_free` | `be.ppareit.swiftp_free` | F-Droid/free build, including Tasker support |
| `playstore_paid` | `be.ppareit.swiftp` | Google Play paid build |

Build every variant, as CI does:

```shell
./gradlew assemble
```

To build just one debug APK:

```shell
./gradlew assembleFdroid_freeDebug
./gradlew assemblePlaystore_paidDebug
```

Generated APKs are written below `app/build/outputs/apk/`.

## Running and checking changes

With a device or emulator connected through ADB, install and launch a debug
variant with one of the project tasks:

```shell
./gradlew runFdroid_freeDebug
./gradlew runPlaystore_paidDebug
```

Run the local verification tasks before submitting a change:

```shell
./gradlew check
```

If connected-device tests are added or changed, run the appropriate flavor:

```shell
./gradlew connectedFdroid_freeDebugAndroidTest
./gradlew connectedPlaystore_paidDebugAndroidTest
```

FTP behavior should also be exercised with real clients. Useful coverage
includes passive and active transfers, uploads, downloads, directory operations,
resume support, and FTPS where relevant.

Testing happens best with a real device. Testing with an emulator has some
restrictions. See next paragraph on how to test through an emulator. I am
only able to test with a Linux client, so it is appreciated if you test Windows
and Mac as clients also.

For testing an emulator from the host, `scripts/swiftp-emulator-bridge.sh`
forwards the control and passive data ports and publishes the FTP service. The
Linux host needs `adb`, `avahi-publish-service`, `ip`, `socat`, and `sudo`. Run
the script (needs root), then follow its printed SwiFTP settings and connection URL.

## Making changes

Keep changes focused and preserve compatibility with both product flavors. New
user-visible text belongs in `app/src/main/res/values/strings.xml`, update
translations when possible, but do not replace existing translations with
machine-generated text. Personally I don't like to add machine generated
translations and like to wait for real translations, but you do you. 

I am not opposed to using an LLM to help prepare a contribution. You take full
ownership of the resulting code: review it thoroughly, understand every change,
and be prepared to maintain it. Do not add `Co-authored-by` or similar
attribution for an LLM.

The source code is licensed under GPLv3 or later, see [COPYING](COPYING). The
existing artwork is separately copyrighted by me and requires permission for
reuse outside this project.

Contributions are welcome as GitHub pull requests. I prefer pull request
as they don't get forgotten. You can always mail me at <pieter.pareit@gmail.com>.

## Release checklist

(This is my personal checklist, a for a PR you can ignore the following)

1. Update `versionCode` and `versionName` in `app/build.gradle`.
2. Update `CHANGES` and any affected user or developer documentation.
3. Run `./gradlew clean check assembleRelease`.
4. Test both flavors on a clean install and verify upgrades from the previous
   release where persisted settings or storage behavior could be affected.
5. Exercise FTP and FTPS with representative Android devices and desktop FTP
   clients, including FileZilla and command-line clients.
6. Build the required signed release APK or app bundle and publish it to the
   appropriate store.
