PKGURL=http://www.lua.org/ftp/lua-5.3.0.tar.gz
PKGHASH=1c46d1c78c44039939e820126b86a6ae12dadfba

package_download $PKGURL $PKGHASH

package_patch

cd src

lua_objs="lapi.o lcode.o lctype.o ldebug.o ldo.o ldump.o lfunc.o lgc.o llex.o  \
        lmem.o lobject.o lopcodes.o lparser.o lstate.o lstring.o ltable.o \
        ltm.o lundump.o lvm.o lzio.o \
        lauxlib.o lbaselib.o lbitlib.o lcorolib.o ldblib.o liolib.o \
        lmathlib.o loslib.o lstrlib.o ltablib.o lutf8lib.o loadlib.o linit.o "

qccprefix=
if [ $SYS_PLATFORM = bb10 ] || [ $SYS_PLATFORM = playbook ]; then
  qccprefix="-Wc,"
fi

for lua_obj in $lua_objs; do
  lua_src=`echo $lua_obj | sed 's/o$/c/'`
  veval "$SYS_CC ${qccprefix}-std=gnu99 -DLUA_COMPAT_5_2 -DLUA_C89_NUMBERS -c $lua_src"
  asserterror $? "compilation failed"
done

veval "$SYS_AR rcu $SYS_PREFIX/lib/liblua.a $lua_objs"
veval "$SYS_RANLIB $SYS_PREFIX/lib/liblua.a"

assertfile "$SYS_PREFIX/lib/liblua.a"

cp lua.h $SYS_PREFIX/include
cp luaconf.h $SYS_PREFIX/include
cp lauxlib.h $SYS_PREFIX/include
cp lualib.h $SYS_PREFIX/include

cd ..

package_cleanup

#eof
