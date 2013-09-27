#!/bin/bash
trap '' SIGINT
java -Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M -Dfile.encoding=UTF-8 -jar ./sbt-launch.jar run
