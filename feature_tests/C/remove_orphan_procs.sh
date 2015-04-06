# !/bin/bash

exec_name="test_shmem_global_exit.x"
oshrun -n 2 ./test_shmem_global_exit.x

ret_stat="$?"
orphan_ids="`pgrep  $exec_name`"

if [ -z "$orphan_ids" ]
then
  if [ $ret_stat = "99" ]
  then
    printf "Test shmem_global_exit: Passed\n";
  else
    printf "Test shmem_global_exit: Failed\n";
  fi
else
  # PE cleanup in case of failure
  for proc in  $orphan_ids
  do
      kill -9 $proc &>/dev/null
  done
fi

sleep 2 # arbitrary wait to allow process teardown

