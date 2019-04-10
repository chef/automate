-module(deliv_event_tests).

-include_lib("eunit/include/eunit.hrl").

module_test_() ->
    {setup,
     fun() ->
             error_logger:tty(false),
             application:start(gproc)
     end,
     fun(ok) ->
             application:stop(gproc),
             error_logger:tty(true);
        (_) ->
             ok
     end,
     [{"basic pub/sub",
       fun() ->
               Parent = self(),
               Pids = [spawn_subscriber(Parent, test_msg, 1),
                       spawn_subscriber(Parent, test_msg, 1)],
               do_pub(test_msg, ping),
               Results = gather(Pids),
               Got = [ Msg || {_, {_, test_msg, Msg}} <- Results ],
               ?assertEqual([ping, ping], Got)
       end},

      {"multi-subscribe",
       fun() ->
               Pid = spawn_subscriber(self(), [test_1, test_2], 2),
               %% send the same message so we don't have to worry
               %% about order of arrival. Message sending and
               %% receiving is async.
               do_pub(test_1, ping),
               do_pub(test_2, ping),
               Results = gather_with_count([{Pid, 2}]),
               Got = [ Msg || {_, {_, _, Msg}} <- Results],
               ?assertEqual([ping, ping], Got)
       end},

      {"unsubscribe before first message",
       fun() ->
               Pid = spawn_subscriber(self(), test_msg, 1),
               Pid ! {unsubscribe, test_msg},
               do_pub(test_msg, ping),
               {_, Got} = hd(gather([Pid])),
               ?assertEqual({unsubscribed, test_msg}, Got)
       end},

      {"unsubscribe after first message should receive no more messages",
       fun() ->
               Pid = spawn_subscriber(self(), test_msg, 2),
               do_pub(test_msg, ping),
               Pid ! {unsubscribe, test_msg},
               do_pub(test_msg, again),
               Results = gather_with_count([{Pid, 2}]),
               Got = [ RawMsg || {_, RawMsg} <- Results ],
               TestMsgAgainPresent = lists:any(fun({test_msg, again}) -> true;
                                                (_) -> false
                                             end, Got),
               ?assertEqual(false, TestMsgAgainPresent)
       end}

     ]}.
%% Its possible for the spawning of the process (which happens async)
%% to take longer than the test process expects. In this condition,
%% the do_pub is happening before the spawned process is subscribed
%% and the test will fail out with a timeout. To fix this, we put
%% in a liveness test via a message back to the spawner.
%% This prevents the test from continuing before the spawner is
%% registered.
spawn_subscriber(Parent, Key, N) ->
    LivenessPid = self(),
    Pid = spawn(fun() ->
                  deliv_event:subscribe(Key),
                  LivenessPid ! {self(), alive},
                  sub_loop(N, Parent)
          end),
    receive
      {Pid, alive} ->
        Pid
    end.


sub_loop(0, _) ->
    ok;
sub_loop(N, Parent) ->
    receive
        {unsubscribe, Key} ->
            deliv_event:unsubscribe(Key),
            Parent ! {self(), {unsubscribed, Key}};
        Msg ->
            Parent ! {self(), Msg}
    end,
    sub_loop(N - 1, Parent).

%% See note on spawn_subscriber/3
do_pub(Key, Msg) ->
    LivenessPid = self(),
    Pid = spawn(fun() ->
                        deliv_event:publish(Key, Msg),
                        LivenessPid ! {self(), alive}
                end),
    receive
      {Pid, alive} ->
        Pid
    end.

gather_with_count(Pids) ->
    lists:flatten([
                   [ gather([Pid]) || _ <- lists:seq(1, N) ]
                   || {Pid, N} <- Pids
                  ]).

gather(Pids) ->
    [
     receive
         {Pid, Msg} ->
             {Pid, Msg}
     end
     || Pid <- Pids ].
