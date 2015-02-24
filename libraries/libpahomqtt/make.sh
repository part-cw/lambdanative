PKGURL=https://github.com/eclipse/paho.mqtt.c/archive/v1.0.3.tar.gz
PKGHASH=afe88aaf0500030f880ee3cb0a81efd44245ac00

package_download $PKGURL $PKGHASH

package_patch

cd src
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
