VERSION=1.3.6
PKGURL=https://github.com/eclipse/paho.mqtt.c/archive/v${VERSION}.tar.gz
PKGHASH=dad215b485f143b5ce1e181336372e5cb6930665

package_download $PKGURL $PKGHASH

#package_patch

cd src
TIMESTAMP=`date "+%a %d %b %Y %T %Z"`
sed -e "s/@CLIENT_VERSION@/$VERSION/g" -e "s/@BUILD_TIMESTAMP@/$TIMESTAMP/g" VersionInfo.h.in > VersionInfo.h
pahomqtt_srcs=`ls -1 *.c | sed '/MQTTVersion.c/d' | tr '\n' ' '`
veval "$SYS_CC -I$SYS_PREFIX/include -DOPENSSL -c $pahomqtt_srcs"
asserterror $? "compilation failed"
veval "$SYS_AR ru $SYS_PREFIX/lib/libpahomqtt.a *.o"
asserterror $? "ar failed"
veval "$SYS_RANLIB $SYS_PREFIX/lib/libpahomqtt.a"
asserterror $? "ranlib failed"

assertfile $SYS_PREFIX/lib/libpahomqtt.a

cp MQTT*.h $SYS_PREFIX/include
cd ..

package_cleanup

#eof
