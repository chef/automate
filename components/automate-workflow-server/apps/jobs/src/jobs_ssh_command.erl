-module(jobs_ssh_command).

-include("jobs_types.hrl").

% Public API
-export([
         generate_ssh_command/2
        ]).

-spec generate_ssh_command(jobs_runner(), binary()) -> string().
generate_ssh_command(#runner{private_key = PrivKey, hostname = Hostname}, Command) ->
    IoList = [[code:priv_dir(jobs), "/ssh_helper.sh"], " '", PrivKey, "' ", Hostname, " '", Command, "'"],
    chef_utils:iodata_to_str(IoList).
