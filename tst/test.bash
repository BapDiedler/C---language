#!/bin/bash
#By Antonin LOUBIERE
cd ../

ERRS_FILES=

for file in tst/ok/*.c
do
    echo -e "[\e[35mBEG\e[0m] \e[34m$file\e[0m"
    ./src/ccomp $file 2> ${file%.c}.log
    if [ $? -eq 0 ]
    then
        echo -e "[\e[32mOK \e[0m] \e[34m${file%.c}.log\e[0m"
    else
        cat ${file%.c}.log
        echo -e "[\e[31mERR\e[0m] \e[34m${file%.c}.log\e[0m"
        ERRS_FILES="$ERRS_FILES\n - $file"
    fi
done

for err_code in 2 3
do
    for file in tst/err-$err_code/*.c
    do
        echo -e "[\e[35mBEG\e[0m] \e[34m$file\e[0m"
        ./src/ccomp $file 2> ${file%.c}.log
        if [ $? -eq $err_code ]
        then
            echo -e "[\e[32mOK \e[0m] \e[34m${file%.c}.log\e[0m"
        else
            cat ${file%.c}.log
            echo -e "[\e[31mERR\e[0m] \e[34m${file%.c}.log\e[0m"
            ERRS_FILES="$ERRS_FILES\n - $file"
        fi
    done
done

if [ -n "$ERRS_FILES" ]
then
    echo -e "\n\e[31mErreurs :\e[0m"
    echo -e $ERRS_FILES
else
    echo -e "\n\e[32mSuccés !\e[0m"
fi