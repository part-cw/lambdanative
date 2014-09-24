
VERSION=1.3.4
PKGURL="http://mosquitto.org/files/source/mosquitto-${VERSION}.tar.gz"
PKGHASH=b818672cc0db723995d7c3201ef6962931dd891a

package_download $PKGURL $PKGHASH

rm config.h
package_patch

TIMESTAMP=`date "+%F %T%z"`
options="-DWITH_TLS -DWITH_TLS_PSK"

echo "==> Building library source..."
cd lib

$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -c *.c -I. -I.. -I$SYS_PREFIX/include
asserterror $? "compilation failed"
$SYS_AR ru $SYS_PREFIX/lib/libmosquitto.a *.o 2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libmosquitto.a  2> /dev/null
cp mosquitto.h $SYS_PREFIX/include/mosquitto.h

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ] || [ "$SYS_PLATFORM" = linux486 ] ||  [ "$SYS_PLATFORM" = carlson-minot ]; then

echo "==> Building tools..."

libs="-lssl -lcrypto -lm"
if [ $SYS_PLATFORM = linux ] || [ $SYS_PLATFORM = linux486 ] || [ $SYS_PLATFORM = carlson-minot ]; then
libs="$libs -ldl -lrt"
fi
libs="$libs -lm"

# clients
cd ../client
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_pub pub_client.c -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_sub sub_client.c -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs

# broker
cd ../src
mv mosquitto_passwd.c mosquitto_passwd.c.tmp
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  -DWITH_BROKER -DWITH_SYS_TREE -DWITH_PERSISTENCE -DWITH_BRIDGE -DWITH_THREADING -DWITH_MEMORY_TRACKING \
  $options -o $SYS_PREFIX/bin/mosquitto *.c  \
  ../lib/memory_mosq.c ../lib/net_mosq.c ../lib/read_handle_shared.c ../lib/send_client_mosq.c \
  ../lib/send_mosq.c ../lib/time_mosq.c ../lib/tls_mosq.c ../lib/util_mosq.c ../lib/will_mosq.c \
  -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib $libs

mv mosquitto_passwd.c.tmp mosquitto_passwd.c
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_passwd mosquitto_passwd.c  -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib $libs

fi

cd ..
package_cleanup

#eof
