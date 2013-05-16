
-module(generic_crud_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DispatchConf = filename:join([code:priv_dir(generic_crud), "dispatch.conf"]),
    {ok, Dispatch} = file:consult(DispatchConf),
    WebConfig = [
                 {ip, "0.0.0.0"},
                 {port, "8080"},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    WebMachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebConfig]},
                  permanent, 5000, worker, [mochiweb_socket_server]},
    {ok, {{one_for_one, 5, 10}, [WebMachine]}}.
