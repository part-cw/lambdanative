#!/bin/sh

javac -source 1.5 -target 1.5 -bootclasspath /home/daniel/programas/android-sdk-linux/platforms/android-8/android.jar com/example/myprocess/MyProcess.java

jar -cf com.example.myprocess.jar com/example/myprocess/*.class
