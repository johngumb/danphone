#!/bin/bash

CMDFIFO=/tmp/danphone-cmdseq
PIDFILE=/tmp/cmdseq.pid

MAXCHAR=100

start()
{
    if [ -f ${PIDFILE} ]; then
        if kill -0 $(cat ${PIDFILE}); then
            echo "already running"
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
    exit $?
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

    exit $?
}

case $1 in
    start) start
        ;;
    stop) stop
        ;;
esac


execute_cmd()
{
    CMD="$1"

    echo ${CMD}
    ${CMD}
}

waitpid()
{
    while kill -0 $1; do
        sleep 0.1
    done
}

while true; do
      if read -n ${MAXCHAR} FULLCMD < ${CMDFIFO}; then
          echo FULLCMD $FULLCMD
          fromnode=$(echo ${FULLCMD} | awk '{print $1}')
          CMD=$(echo ${FULLCMD} | awk '{$1=""; print $0}')
          first=$(echo ${CMD} | awk '{print $1}')
          pernode_fifo="/tmp/${fromnode}_recfifo"

          if [ "${first}" == "jack_capture" ]; then
              ${CMD} &
              echo $! > ${pernode_fifo}
          elif [ "${first}" == "kill" ]; then
              pid=$(echo ${CMD} | awk '{print $NF}')
              execute_cmd "${CMD}"

              # need to wait for process to die
              waitpid ${pid}

              # let requesting process know recording has stopped
              echo ${pid} > ${pernode_fifo}
          else
              execute_cmd "${CMD}"
          fi
      fi
      # readstat=$?
      # if [ ${readstat} -ne 0 ]; then
      #     echo "$0: read failed"
      #     sleep 0.1
      # fi
done

