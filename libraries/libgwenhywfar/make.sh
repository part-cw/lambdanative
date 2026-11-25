PKGURL=https://www.aquamaniac.de/rdm/attachments/download/630/gwenhywfar-5.14.1.tar.gz
PKGHASH=1a144eb0e760a56344431e30b1e9af4fd09948cd

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --disable-tests --disable-doc --disable-doc --disable-tools  

package_make

package_make install

package_cleanup

#eof
