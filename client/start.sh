#!/bin/bash
trap '' SIGINT
SBT_OPTS="-Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M -Dfile.encoding=UTF-8" java -jar ./sbt-launch.jar run
