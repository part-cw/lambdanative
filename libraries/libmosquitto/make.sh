PKGURL=https://github.com/eclipse-mosquitto/mosquitto/archive/refs/tags/v2.1.2.tar.gz
PKGHASH=36ca9a88463895ad25527a0a7c7f9653031f716e

package_download $PKGURL $PKGHASH

package_patch

TIMESTAMP=`date "+%F %T%z"`
options="-DWITH_TLS -DWITH_TLS_PSK -DOPENSSL_API_COMPAT=0x10100000L"

echo "==> Building library source..."
cd lib
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -c *.c ../libcommon/*.c -I. -I.. -I../include -I../libcommon \
  -I../common -I../deps -I$SYS_PREFIX/include
asserterror $? "compilation failed"
rm -f $SYS_PREFIX/lib/libmosquitto.a
$SYS_AR ru $SYS_PREFIX/lib/libmosquitto.a *.o 2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libmosquitto.a  2> /dev/null
mkdir -p $SYS_PREFIX/include/mosquitto
cp ../include/*.h $SYS_PREFIX/include/
cp ../include/mosquitto/*.h $SYS_PREFIX/include/mosquitto/

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ] || [ "$SYS_PLATFORM" = linux486 ] ||  [ "$SYS_PLATFORM" = carlson-minot ]; then

echo "==> Building tools..."

libs="-lssl -lcrypto -lcjson -lm"
if [ $SYS_PLATFORM = linux ] || [ $SYS_PLATFORM = linux486 ] || [ $SYS_PLATFORM = carlson-minot ]; then
libs="$libs -ldl -lrt"
fi

# clients
cd ../client
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_pub pub_client.c pub_shared.c client_props.c client_shared.c \
  -I. -I.. -I../include -I../lib -I../libcommon -I../deps \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  $options -o $SYS_PREFIX/bin/mosquitto_sub sub_client.c sub_client_output.c client_props.c client_shared.c \
  -I. -I.. -I../include -I../lib -I../libcommon -I../deps \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib  -lmosquitto $libs

# broker
cd ../src
$SYS_CC -DVERSION="\"${VERSION}\"" -DTIMESTAMP="\"${TIMESTAMP}\"" \
  -DWITH_BROKER -DWITH_SYS_TREE -DWITH_PERSISTENCE -DWITH_BRIDGE -DWITH_THREADING -DWITH_MEMORY_TRACKING \
  $options -o $SYS_PREFIX/bin/mosquitto *.c ../common/json_help.c \
  ../plugins/acl-file/acl_check.c ../plugins/acl-file/acl_parse.c \
  ../plugins/password-file/password_check.c ../plugins/password-file/password_parse.c \
  ../lib/alias_mosq.c ../lib/handle_ping.c ../lib/handle_pubackcomp.c \
  ../lib/handle_pubrec.c ../lib/handle_pubrel.c ../lib/handle_suback.c \
  ../lib/handle_unsuback.c ../lib/net_mosq_ocsp.c ../lib/net_mosq.c ../lib/net_ws.c \
  ../lib/packet_datatypes.c ../lib/packet_mosq.c ../lib/property_mosq.c \
  ../lib/send_connect.c ../lib/send_disconnect.c ../lib/send_mosq.c \
  ../lib/send_publish.c ../lib/send_subscribe.c ../lib/send_unsubscribe.c \
  ../lib/tls_mosq.c ../lib/util_mosq.c ../lib/will_mosq.c ../libcommon/*.c \
  -I. -I.. -I../include -I../lib -I../libcommon -I../common -I../deps \
  -I$SYS_PREFIX/include  -L$SYS_PREFIX/lib $libs

fi

cd ..
package_cleanup

#eof
