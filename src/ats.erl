-module(ats).
-author("khanhhua").

-behaviour(application).
-include("include/records.hrl").

-export([start/2, stop/1]).
-export([simple_loop/1]).

-define(TCP_PORT, 4441).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(MENU_MAIN, 100).
-define(SCREEN_POST_POSITION, 101).
-define(SCREEN_SEARCH_POSITIONS, 102).
-define(MENU_SCREEN_SEARCH_POSITIONS_SUBMENU, 10201).
-define(SCREEN_APPLY_FOR_POSITION, 10202).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  ats_position_service:start_link(),
  ats_candidate_service:start_link(),

  Pid = spawn(?MODULE, simple_loop, [{}]),
  io:format("ATS Applicant Tracking System initialized...~n"),
  {ok, Pid}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
simple_loop(State) ->
  {ok, LSocket} = gen_tcp:listen(?TCP_PORT, ?TCP_OPTIONS),
  accept(LSocket).

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  io:format("Client connection established...~n"),
  spawn(fun() -> connection_handler(Socket) end),

  accept(LSocket).

connection_handler(Socket) ->
  Client = #client{screen = ?MENU_MAIN, socket = Socket},
  case loop(Client) of
    terminated -> gen_tcp:close(Socket);
    _ -> ok
  end.


loop (Client) ->
  Client2 = ui(Client),
  if
    Client2#client.terminated =:= true -> terminated;
    true -> case Client2#client.command of
      {search_positions, QueryTerms} ->
        Query = #positionQuery{minSalary = proplists:get_value(minSalary, QueryTerms),
                               positionName = proplists:get_value(positionName, QueryTerms)},
        Positions = ats_position_service:search(Query, 20),
        client_write(Client2,
          "=============================~n"
          "Positions found~n"
          "-----------------------------~n" ++
          lists:join("",
            lists:map(
              fun (#position{name=Name, salary=Salary, id=Id}) ->
                io_lib:format("~B. ~-20.s ~s~n", [Id, Name, Salary])
              end,
              Positions
            )
          ) ++
          "=============================~n"
        ),
        loop(Client2);

      {create_position, PositionProps} ->
        Position = #position{name = proplists:get_value(name, PositionProps),
                             salary = proplists:get_value(salary, PositionProps)},
        ok = ats_position_service:create(Position),
        loop(Client2);

      {create_candidate, ApplicantProps} ->
        Applicant = ats_candidate_service:get_template(),
        Applicant2 = Applicant#candidate{name = proplists:get_value(name, ApplicantProps),
                                         yob = proplists:get_value(yob, ApplicantProps),
                                         position_ids = [proplists:get_value(position_id, ApplicantProps)]},
        ok = ats_candidate_service:register(Applicant2),
        loop(Client2);

      _ -> loop(Client2)
    end
  end.

ui(Client) when Client#client.screen =:= ?MENU_MAIN ->
  client_write(Client,
    "ATS Applicant Tracking System~n"
    "=============================~n"
    "1. Search for positions~n"
    "2. Post a new position~n"
    "X. Quit~n"
    "Enter: "),
  Choice =case client_read(Client) of
    "1" -> ?SCREEN_SEARCH_POSITIONS;
    "2" -> ?SCREEN_POST_POSITION;
    "X" -> terminate
  end,
  case Choice of
    terminate -> Client#client{terminated = true, command = undefined}; % Return a new instance of Client
    Screen -> Client#client{screen = Screen, command = undefined} % Return a new instance of Client
  end;

ui(Client) when Client#client.screen =:= ?SCREEN_SEARCH_POSITIONS ->
  client_write(Client,
    "ATS Applicant Tracking System~n"
    "=============================~n"
    "1. Search for positions~n"
    "-----------------------------~n"
    "Min Salary: "),
  MinSalary = client_read(Client),
  client_write(Client,
    "Position name: "),
  PositionName = client_read(Client),
  io:format("Searching for positions..."),
  Client#client{
    screen = ?MENU_SCREEN_SEARCH_POSITIONS_SUBMENU,
    command = {search_positions, [{minSalary, MinSalary}, {positionName, PositionName}]}
  };

ui(Client) when Client#client.screen =:= ?MENU_SCREEN_SEARCH_POSITIONS_SUBMENU ->
  client_write(Client,
    "ATS Applicant Tracking System~n"
    "=============================~n"
    "1. Search for positions~n"
    "  0. Back to main menu~n"
    "  1. Apply for a position~n"
    "-----------------------------~n"
    "Enter: "),
  Choice =case client_read(Client) of
    "0" -> ?MENU_MAIN;
    "1" -> ?SCREEN_APPLY_FOR_POSITION;
    "X" -> terminate
  end,
  case Choice of
    terminate -> Client#client{terminated = true, command = undefined}; % Return a new instance of Client
    Screen -> Client#client{screen = Screen, command = undefined} % Return a new instance of Client
  end;

ui(Client) when Client#client.screen =:= ?SCREEN_APPLY_FOR_POSITION ->
  client_write(Client,
    "ATS Applicant Tracking System~n"
    "=============================~n"
    "1. Search for positions~n"
    "  1. Apply for a position~n"
    "-----------------------------~n"
    "Candidate name: "),
  CandidateName = client_read(Client),
  client_write(Client,
    "Candidate year of birth: "),
  CandidatYob = client_read(Client),
  client_write(Client,
    "Position ID: "),
  PositionId = client_read(Client),

  io:format("Applying for a position..."),
  Client#client{
    screen = ?MENU_MAIN,
    command = {create_candidate, [{name, CandidateName},
                                  {yob, CandidatYob},
                                  {position_id, PositionId}]}
  };

ui(Client) when Client#client.screen =:= ?SCREEN_POST_POSITION ->
  client_write(Client,
    "ATS Applicant Tracking System~n"
    "=============================~n"
    "2. Post a new position~n"
    "-----------------------------~n"
    "Position name: "),
  PositionName = client_read(Client),
  client_write(Client,
    "Position salary: "),
  PostionSalary = client_read(Client),
  io:format("Creating a new position..."),
  Client#client{
    screen = ?MENU_MAIN,
    command = {create_position, [{name, PositionName}, {salary, PostionSalary}]}
  }.

client_write(Client, String) ->
  gen_tcp:send(Client#client.socket, io_lib:format(String, [])).

client_read(Client) ->
  case gen_tcp:recv(Client#client.socket, 0) of
    {ok, Read} ->
      io:format("Received from client: ~p~n", [Read]),
      [Ret] = string:tokens(binary_to_list(Read), "\n\r"),
      io:format("... transformed to: ~p~n", [Ret]),
      Ret;
    {error,closed} -> "QUIT"
  end.
