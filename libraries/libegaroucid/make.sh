PKGURL=https://github.com/Nyanyan/Egaroucid/archive/v7.8.3/egaroucid-7.8.3.tar.gz
PKGHASH=dc7b83e2ccd4604731baf9056feee3c227e71bc1

package_download $PKGURL $PKGHASH

package_patch

package_cmake -DBUILD_ENGINE_LIB=ON -DBUILD_CONSOLE=OFF -DBUILD_GUI=OFF -DBUILD_SHARED_LIBS=ON -DHAS_NO_AVX2=ON

package_cmake_build

install include/egaroucid/egaroucid.h -D -t $SYS_PREFIX/include/egaroucid
install build/libegaroucid.a -D -t $SYS_PREFIX/lib

package_cleanup
