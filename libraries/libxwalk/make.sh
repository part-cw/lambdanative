# support for crosswalk library 
# Android only at the moment

PKGURL=https://download.01.org/crosswalk/releases/crosswalk/android/stable/latest/crosswalk-23.53.589.4.zip
PKGHASH=be1514ccb6180624aa516f59b055bea3bb066ef1
PKGFILE=$(basename $PKGURL)

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
