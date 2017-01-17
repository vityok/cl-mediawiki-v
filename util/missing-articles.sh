#!/bin/sh

## The script gets two arguments and produces a new file listing
## articles from cat.txt that are missing in the all.txt file.

## missing-articles.sh cat.txt all.txt

## For example, if all.txt lists articles from a certain project and
## cat.txt lists contents of some category, by running this script it
## is possible to find articles from the given category that are not
## yet listed in the project.

# The script works in two steps: 1. find articles from cat.txt that
# are already present in the all.txt, 2. subtract them from the
# all.txt

cp $1 $1.tmp

cat $2 >> $1.tmp

sort $1.tmp > $1.tmp.sorted

uniq -d < $1.tmp.sorted > $1.present

# the $1.present now lists articles from the cat.txt that are already
# present in the project, now substract them from the original set

cat $1.present >> $1

sort $1 > $1.sorted

uniq -u $1.sorted > $1.missing

# cleanup

rm $1.tmp $1.tmp.sorted $1.present $1.sorted

echo $1.missing

# EOF
