#!/bin/bash

CMDFIFO=/tmp/danphone-cmdseq

MAXCHAR=100

if [ ! -p ${CMDFIFO} ]; then
   mkfifo ${CMDFIFO}
fi

execute_cmd()
{
    CMD="$1"
    let attempts=0
    let maxattempts=10

    while [ ${attempts} -lt ${maxattempts} ]; do
        if ${CMD}; then
            break
        fi
        sleep 0.1
        let attempts=${attempts}+1
    done
}

waitpid()
{
    while kill -0 $1; do
        sleep 0.1
    done
}

while true; do
      if read -n ${MAXCHAR} FULLCMD < ${CMDFIFO}; then
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
      readstat=$?
      if [ ${readstat} -ne 0 ]; then
          echo "$0: read failed"
          sleep 0.1
      done
done

