VERSION=1.6.12
PKGURL="http://mosquitto.org/files/source/mosquitto-${VERSION}.tar.gz"
PKGHASH=b8c047985a33f3c2e9855079030f91d6eed00c47

package_download $PKGURL $PKGHASH

package_patch

TIMESTAMP=`date "+%F %T%z"`
options="-DWITH_TLS -DWITH_TLS_PSK"

echo "==> Building library source..."
cd lib

 $SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -c *.c -I. -I.. -I ../src/deps -I$SYS_PREFIX/include
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

# clients
cd ../client
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_pub pub_client.c pub_shared.c client_props.c client_shared.c -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_sub sub_client.c sub_client_output.c client_props.c client_shared.c -I. -I.. -I../lib \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs

# broker
cd ../src
mv mosquitto_passwd.c mosquitto_passwd.c.tmp
mv plugin_defer.c plugin_defer.c.tmp
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  -DWITH_BROKER -DWITH_SYS_TREE -DWITH_PERSISTENCE -DWITH_BRIDGE -DWITH_THREADING -DWITH_MEMORY_TRACKING \
  $options -o $SYS_PREFIX/bin/mosquitto *.c  \
  ../lib/alias_mosq.c ../lib/handle_ping.c ../lib/handle_pubackcomp.c ../lib/handle_pubrec.c \
  ../lib/handle_pubrel.c ../lib/handle_suback.c ../lib/handle_unsuback.c ../lib/memory_mosq.c \
  ../lib/misc_mosq.c ../lib/net_mosq_ocsp.c ../lib/net_mosq.c ../lib/packet_datatypes.c \
  ../lib/packet_mosq.c ../lib/property_mosq.c ../lib/send_connect.c ../lib/send_disconnect.c \
  ../lib/send_mosq.c ../lib/send_publish.c ../lib/send_subscribe.c ../lib/send_unsubscribe.c \
  ../lib/time_mosq.c ../lib/tls_mosq.c ../lib/util_mosq.c ../lib/util_topic.c ../lib/utf8_mosq.c \
  ../lib/will_mosq.c \
  -I. -I.. -I../lib -I./deps \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib $libs

mv mosquitto_passwd.c.tmp mosquitto_passwd.c
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_passwd mosquitto_passwd.c ../lib/misc_mosq.c -I. -I.. -I../lib -I./deps \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib $libs

fi

cd ..
package_cleanup

#eof
