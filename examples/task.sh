#!/bin/sh

batch() {
    for i in $(seq $1 $2)
    do
	echo $i
	sbcl --load 'project-stats-task.lisp' --eval "(cl-mediawiki-util:task-run $i)"
    done
    echo done
}

case $1 in
    1)
	batch 0 3
	;;
    2)
	batch 4 8
	;;
    3)
	batch 9 12
	;;
esac
