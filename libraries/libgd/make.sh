PKGURL=https://bitbucket.org/libgd/gd-libgd/downloads/libgd-2.1.0.tar.gz
PKGHASH=a0f3053724403aef9e126f4aa5c662573e5836cd

package_download $PKGURL $PKGHASH

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

# prevent DLL hell on win32
cp src/gd.h tmp
cat tmp | sed 's/^#define BGD_DECLARE(rt) BGD_EXPORT_DATA_PROT rt BGD_STDCALL/#define BGD_DECLARE(rt) rt/g' > src/gd.h
rm tmp

autoconf

package_configure $EXTRACONF --disable-shared --enable-static --with-png=$SYS_PREFIX --with-freetype=$SYS_PREFIX --with-jpeg=$SYS_PREFIX --without-tiff --without-xpm --without-fontconfig

cd src
package_make libgd.la
asserterror $? "compilation failed"

assertfile ./.libs/libgd.a
cp ./.libs/libgd.a $SYS_PREFIX/lib
cp gd.h $SYS_PREFIX/include
cp gdfx.h $SYS_PREFIX/include
cp gd_io.h $SYS_PREFIX/include

cd ..

package_cleanup

#eof
