#!/bin/bash

priv_key=$1
hostname=$2
remote_command=$3

# Canceling an ssh shellout
#
# The jobs_command gen_server, by using erlang:open_port, knows the OS pid of
# the spawned command. This will be the pid of the bash instances executing this
# script. In the happy path, this script will simply wait for the ssh call to
# finish, and exit itself.
# To kill the ssh command that is spawned as part of this script, we'll set up
# a trap  (https://www.gnu.org/software/bash/manual/bashref.html#index-trap),
# which will kill the spawned ssh call when the bash instance executing this
# script is killed. (Note that "killing" here does NOT mean `kill -9`, or
# SIGKILL, since there's no way to cleanup when you're killed.)

temp_dir=`mktemp -d`
echo "$priv_key" > $temp_dir/key
chmod 700 $temp_dir
chmod 600 $temp_dir/key

cat << EOF > $temp_dir/config
Host job
    HostName $hostname
    User job_runner
    LogLevel error
    StrictHostKeyChecking no
    UserKnownHostsFile /dev/null
    IdentityFile $temp_dir/key
    ServerAliveInterval 10
    ServerAliveCountMax 3
EOF

# This will be executed by bash when it is exiting -- either because it has
# finished normally, OR because it was killed by a signal (except for SIGKILL,
# as in `kill -9`).
function cleanup {
  if [ -s $temp_dir/pid ]; then
    kill $(cat $temp_dir/pid)
  fi
  rm -rf $temp_dir
}

trap cleanup exit

# Test that we can login without a remote PTY first, this will catch expired password and avoid the prompt to change them
ssh -T -F $temp_dir/config job "sh -c \"echo -n Validating login on [$hostname]... && sudo ls\" "
login_exit_code=$?
if [ $login_exit_code != 0 ]; then
  echo "Login validation failed, returned $login_exit_code"
  exit $login_exit_code
fi
echo "Validated!"

# `-tt` forces the allocation of a PTY on the _remote end_. This is _vital_ for
# having remote processes stopped when the client goes away (because it has
# been killed or networking has gone away).
# This should be equal to using `RequireTTY force` in the above config, but
# for this: https://bugs.launchpad.net/ubuntu/+source/openssh/+bug/1080621

# The `sh -c` indirection is necessary, too: only when the parent process is a
# shell does sudo relay the SIGHUP signal it gets when the sshd parent process
# goes away (fixed in sudo 1.8.15 https://bugzilla.sudo.ws/show_bug.cgi?id=719,
# but at least Ubuntu 14.04 only ships 1.8.9p5).

# The "echo ... &&" is necessary for when sh is actually bash -- without this,
# bash would recognize that it could just do exec() and would just do it.
ssh -tt -F $temp_dir/config job "sh -c \"echo Executing remotely on [$hostname]... && sudo $remote_command\" " &

ssh_pid=$!
echo "$ssh_pid" > $temp_dir/pid
wait "$ssh_pid"

ssh_command_result=$?
rm $temp_dir/pid
exit $ssh_command_result
