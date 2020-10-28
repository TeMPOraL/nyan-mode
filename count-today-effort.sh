#!/bin/bash

TODAY=`date +"%Y%m%d"`
ls -l ~/Dropbox/myNote/zk/ | grep "$TODAY" | grep -v "~" |  awk '{print $9}' | wc -l > ~/.emacs.d/nyan-mode/log.today_effort 2>&1
