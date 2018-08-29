PKGURL=https://github.com/mik3y/usb-serial-for-android.git
PKGHASH=b96f9ca

package_download $PKGURL $PKGHASH
package_patch

USBSER_LIBNAME=usb-serial-for-android

cd usbSerialForAndroid/src/main/java
# fix the place where the codes needs to go 
mkdir src
mv com src

if [ -f $SYS_PREFIX/lib/$USBSER_LIBNAME.a ]; then
  echo " => removing old library..."
  rm $SYS_PREFIX/lib/$USBSER_LIBNAME.a
fi
if [ -f $SYS_PREFIX/lib/$USBSER_LIBNAME.jar ]; then
  echo " => removing old jar library..."
  rm $SYS_PREFIX/lib/$USBSER_LIBNAME.jar
fi

echo " => configuring source..."
if [ "$SYS_ANDROIDAPI" -lt "12" ]; then
  assert "USB Serial functions require API Level 12+. Please adjust your ANDROIDAPI setting in the SETUP file."
fi

USBSER_TARGET=`$SYS_ANDROIDSDK/tools/android list targets | grep "^id:" | grep "android-$SYS_ANDROIDAPI" | cut -f 2 -d " "`
veval "$SYS_ANDROIDSDK/tools/android -s create lib-project --name usbserial \
--target $USBSER_TARGET \
--path . \
--package com.hoho.android.usbserial"
asserterror $? "android create failed"

echo " => compiling source..."
echo "java.target = $JAVAVERSION" >> ant.properties
echo "java.source = $JAVAVERSION" >> ant.properties
veval "ant release"
asserterror $? "compilation failed"

echo " => installing..."
assertfile bin/classes.jar
cp bin/classes.jar $SYS_PREFIX/lib/$USBSER_LIBNAME.jar
ar q $SYS_PREFIX/lib/$USBSER_LIBNAME.a bin/classes.jar

cd ..

package_cleanup

#eof
