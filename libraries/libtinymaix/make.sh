PKGURL=https://github.com/sipeed/TinyMaix/archive/0532eceb92097dc97fb39b1d01a41f6736eb51bb/tinymaix-0532ece.tar.gz
PKGHASH=0b67129f86c967b723722e6350815a184727c4c7

package_download $PKGURL $PKGHASH

package_patch

package_cmake

package_cmake_build

cp include/tinymaix.h $SYS_PREFIX/include
cp include/tm_port.h $SYS_PREFIX/include
cp build/libtinymaix.a $SYS_PREFIX/lib

package_cleanup

#eof
