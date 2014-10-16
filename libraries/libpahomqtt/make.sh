
PKGURL=https://github.com/eclipse/paho.mqtt.c.git
PKGHASH=8ffd07f12cfbea0a8dfa9ffdb3fbe8ca74acbbf0

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
