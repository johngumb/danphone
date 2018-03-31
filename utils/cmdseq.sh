#!/bin/bash

CMDFIFO=/tmp/danphone-cmdseq

MAXCHAR=100

if [ ! -p ${CMDFIFO} ]; then
   mkfifo ${CMDFIFO}
fi

let maxattempts=10

execute_cmd()
{
    CMD="$1"
    let attempts=0
    while [ ${attempts} -lt ${maxattempts} ]; do
        if ${CMD}; then
            break
        fi
        sleep 0.1
        let attempts=${attempts}+1
    done
}

while true; do
      if read -n ${MAXCHAR} FULLCMD < ${CMDFIFO}; then
          fromnode=$(echo ${FULLCMD} | awk '{print $1}')
          CMD=$(echo ${FULLCMD} | awk '{$1= ""; print $0}')
          first=$(echo ${CMD} | awk '{print $1}')

          if [ "${first}" == "jack_capture" ]; then
              pernode_fifo="/tmp/${fromnode}_recfifo"
              ${CMD} &
              echo $! > ${pernode_fifo}
          elif [ "${first}" == "kill" ]; then
              pernode_fifo="/tmp/${fromnode}_recfifo"
              pid=$(echo ${CMD} | awk '{print $2}')
              execute_cmd "${CMD}"
              echo ${pid} > ${pernode_fifo}
          else
              execute_cmd "${CMD}"
          fi
      fi
done

