PKGURL=https://github.com/threatstack/libmagic/archive/5.18.tar.gz
PKGHASH=ce734a1cc24bddbfd8b21f4ffdfe721ab74eeed9

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" ]; then
    EXTRACONF="$EXTRACONF --host=$SYS_ARCH"
fi

# pretend libgnurx exists by masking libpcre

test -L $SYS_PREFIX/include/regex.h || ln -s pcreposix.h $SYS_PREFIX/include/regex.h

# Mabye instead of fooling the package, it should be patched
rmifexists $SYS_PREFIX/lib/libgnurx.a
rmifexists $SYS_PREFIX/lib/libgnurx
mkdir $SYS_PREFIX/lib/libgnurx
( cd $SYS_PREFIX/lib/libgnurx
  mkdir a
  cd a
  $SYS_AR -x ../../libpcre.a
  cd ..
  mkdir b
  cd b
  $SYS_AR -x ../../libpcreposix.a
  cd ..
  $SYS_AR rc ../libgnurx.a a/*.o b/*.o
)

rm -r $SYS_PREFIX/lib/libgnurx

if [ `file --version|head -1` != file-5.18 ]; then
    if [ ! -f  $SYS_HOSTPREFIX/bin/file ]; then
	./configure --enable-static --disable-shared 'CFLAGS=-fPIC -O3 -DPCRE_STATIC'
	package_make
	cp src/file $SYS_HOSTPREFIX/bin || exit 1
	package_cleanup
	package_download $PKGURL $PKGHASH
	package_patch
    fi
    FILE_COMPILE=$SYS_HOSTPREFIX/bin/file
fi

# configure

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" -a "X$FILE_COMPILE" != "X" ]; then
    echo patching magic/Makefile.in to use $FILE_COMPILE
    sed -i -es%'@IS_CROSS_COMPILE_TRUE@FILE_COMPILE = file${EXEEXT}'%"@IS_CROSS_COMPILE_TRUE@FILE_COMPILE = ${FILE_COMPILE}"% magic/Makefile.in
fi

package_configure --enable-static --disable-shared $EXTRACONF "'CFLAGS=-fPIC -O3 -DPCRE_STATIC -DNOSHLWAPI'"


# build

package_make

# install

cp src/.libs/libmagic.a $SYS_PREFIX/lib
cp src/magic.h $SYS_PREFIX/include
test -f src/file.exe && cp src/file.exe $SYS_PREFIX/bin
test -d $SYS_PREFIX/etc || mkdir $SYS_PREFIX/etc
cp magic/magic.mgc $SYS_PREFIX/etc

#  Cleanup the fooling
rm -f $SYS_PREFIX/lib/libgnurx.a $SYS_PREFIX/include/regex.h

package_cleanup

unset EXTRACONF

#eof
