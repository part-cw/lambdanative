PKGURL=https://github.com/strophe/libstrophe/releases/download/0.14.0/libstrophe-0.14.0.tar.gz
PKGHASH=156a53fd4545a1c231f5b8c5abe0a8098edf34a4

package_download $PKGURL $PKGHASH
package_patch
autoconf
package_configure --disable-shared --enable-static --disable-examples --disable-cares --with-libxml2
package_make

cp .libs/libstrophe.a $SYS_PREFIX/lib
cp -L -R strophe.h $SYS_PREFIX/include

package_cleanup

#eof
