#!/bin/bash

cd ~/Dropbox/myNote/zk/ && git diff ~/Dropbox/myNote/zk/ > ~/.emacs.d/nyan-mode/tmp.log 2>&1
grep ':ID:       ' ~/.emacs.d/nyan-mode/tmp.log | wc -l > ~/.emacs.d/nyan-mode/log.today_effort 2>&1
