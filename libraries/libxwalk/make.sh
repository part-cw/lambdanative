# support for crosswalk library 
# Android only at the moment

PKGURL=https://download.01.org/crosswalk/releases/crosswalk/android/stable/latest/crosswalk-22.52.561.4.zip
PKGFILE=$(basename $PKGURL)
PKGHASH=637b61ddfa83a535d9dd239e1d060bb618769516

package_download $PKGURL $PKGHASH

XWALKLIB="libxwalk"

XWALKLIBPATH=$SYS_PREFIX/lib/$XWALKLIB
if [ ! -d $XWALKLIBPATH ]; then
  mkdir $XWALKLIBPATH
fi

echo " => configuring source..."
cd xwalk_core_library
veval "$SYS_ANDROIDSDK/tools/android update lib-project -p . --target android-$ANDROIDAPI"
asserterror $?
cp -R ./* $XWALKLIBPATH
cd ..

package_cleanup

#eof
