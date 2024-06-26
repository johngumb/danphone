#!/bin/bash

CMDFIFO=/tmp/danphone-cmdseq
PIDFILE=/tmp/cmdseq.pid

MAXCHAR=300

start()
{
    if [ -f ${PIDFILE} ]; then
        if kill -0 $(cat ${PIDFILE}); then
            echo "$0: already running"
            exit 1
        else
            rm -f ${PIDFILE}
        fi
    fi
          
    if [ ! -p ${CMDFIFO} ]; then
        mkfifo ${CMDFIFO}
    fi

    $(pwd)/$0 &
    echo $! > ${PIDFILE}
}

stop()
{
    if [ -f ${PIDFILE} ]; then
        pid=$(cat ${PIDFILE})

        if kill ${pid}; then
            rm -f ${PIDFILE}
        fi
    fi

    rm -f ${CMDFIFO}
}

case $1 in
    start) 
        start
        exit $?
        ;;
    stop) 
        stop
        exit $?
        ;;
    restart) 
        stop
        start
        exit $?
        ;;
esac

locdate()
{
    date +%H:%M:%S:%N
}

execute_cmd()
{
    expect_success=$1
    shift
    pernode_fifo=$1
    shift
    CMD="$1"
    let attempts=0
    let maxattempts=2

    if ${expect_success}; then
        let maxattempts=3
    else
        let maxattempts=1
    fi

    while [ ${attempts} -lt ${maxattempts} ]; do
        echo $(locdate) ${CMD}
        if ${CMD}; then
            break
        fi
        let attempts=${attempts}+1
    done
}

waitpid()
{
    while kill -0 $1 2>/dev/null; do
        sleep 0.1
    done
}

while true; do
    if read -n ${MAXCHAR} ALLFULLCMD < ${CMDFIFO}; then
        jacknode=$(echo ${ALLFULLCMD} | awk '{print $1}')
        expect_success=$(echo ${ALLFULLCMD} | awk '{print $2}')
        FULLCMDSTR=$(echo ${ALLFULLCMD} | awk '{$1=""; $2=""; print $0}')
        echo FULLCMDSTR $FULLCMDSTR
        IFS=";" FULLCMDARR=(${FULLCMDSTR}); unset IFS
        for CMD in "${FULLCMDARR[@]}"; do

            first=$(echo ${CMD} | awk '{print $1}')
            pernode_fifo="/tmp/${jacknode}_recfifo"

            if [ "${first}" == "jack_capture" ]; then
                echo $(locdate) $CMD
                let attempts=0
                while [ ${attempts} -lt 10 ]; do
                    ${CMD} &
                    newproc=$!
                    if kill -0 ${newproc}; then
                        echo "$0: newproc" ${newproc}
                        echo ${newproc} > ${pernode_fifo}
                        break
                    fi
                    sleep 0.1
                    echo ${CMD} "failed..retrying"
                    let attempts=${attempts}+1
                done
            elif [ "${first}" == "kill" ]; then
                pid=$(echo ${CMD} | awk '{print $NF}')
                execute_cmd true ${pernode_fifo} "${CMD}"

                # need to wait for process to die
                waitpid ${pid}

                # let requesting process know recording has stopped
                echo ${pid} > ${pernode_fifo}
            elif [ "${first}" == "synchr" ]; then
                token=$(echo ${CMD} | awk '{print $NF}')

                # Let requesting process know we are synchronised
                # by passing back the opaque token from the
                # synchr request.
                echo ${token} > ${pernode_fifo}
            else
                execute_cmd ${expect_success} ${pernode_fifo} "${CMD}"
            fi
        done
    else
        read_rc=$?
        echo "$0: read failed rc ${read_rc}"
        sleep 0.1
    fi
done

