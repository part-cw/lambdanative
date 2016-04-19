
tweetnacl_url="https://tweetnacl.cr.yp.to/20140427"

tweetnacl_c="$SYS_PREFIXROOT/packages/tweetnacl.c"
tweetnacl_h="$SYS_PREFIXROOT/packages/tweetnacl.h"

if [ ! -f "$tweetnacl_c" ]; then
  wget -O "$tweetnacl_c" $tweetnacl_url/tweetnacl.c
fi
if [ ! -f "$tweetnacl_h" ]; then
  wget -O "$tweetnacl_h" $tweetnacl_url/tweetnacl.h
fi

package_from_source "$tweetnacl_c" "$tweetnacl_h"

rm *.o 2> /dev/null
veval "$SYS_CC -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libtweetnacl.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libtweetnacl.a"
asserterror $? "compilation failed"

cp libtweetnacl.a $SYS_PREFIX/lib
cp tweetnacl.h $SYS_PREFIX/include

package_cleanup

#eof
