#!/bin/bash

cd /home/chubaduba/BLOG/
git init
git add --all

commit=''
while [ "$commit" = "" ]; do
    echo -n "Введите commit: "
    read commit

git commit -m "$commit"
git push -u origin master

done
