-module(behtree).

-behaviour(gen_server).

%% API
-export([start/2,
         start/3,
         get_status/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         run_action_wrapper/2]).

-record(state, {node_type,
                status,
                children = [],
                sequence_num = 1,
                parent,
                func}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Id, NodeType) ->
    gen_server:start({global, {behtree, Id}}, ?MODULE, [NodeType], []).

start(Id, NodeType, Func) ->
    gen_server:start({global, {behtree, Id}}, ?MODULE, [NodeType, Func], []).

get_status(Pid) ->
    gen_server:call(Pid, {get_status}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([action, Func]) ->
    State = #state{ node_type = action,
                    status = ready,
                    func = Func},

    {ok, State};

init([NodeType]) ->

    State = #state{ node_type = NodeType,
                    status = ready},

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_status}, _From, State) ->
    {reply, State#state.status, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_child, Node}, State) ->
    Children = State#state.children,    
    NewChildren = lists:append(Children, [{length(Children), Node}]),
    NewState = State#state {children = NewChildren},
    {noreply, NewState};

handle_cast(run, State) ->
    NewState = run(State),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
     
    NewStatus = case Info of 
                    success ->
                        success;
                    failure ->
                        failure;
                    _ ->
                        erlang:error("Invalid result from action node")
                end,

    NewState = State#state {status = NewStatus},

    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
run(State) ->
    NewState = case State#state.node_type of
                   action ->
                        run_action(State#state.func),
                        State#state {status = running};
                   sequence ->
                        UpdatedState = run_sequence(State),                      
                        UpdatedState#state {status = running}
                end,
    NewState.

run_action(Func) ->
    
    spawn(?MODULE, run_action_wrapper, [Func, self()]).

run_action_wrapper(Func, CallerPID) ->

    Result = Func(),
    io:fwrite("Result: ~w~n", [Result]),
    CallerPID ! Result.

run_sequence(State) ->
    Children = State#state.children,
    SequenceNum = State#state.sequence_num,

    {ChildNum, ChildPid} = lists:nth(SequenceNum, Children),
    ChildStatus = behtree:get_status(ChildPid),
    io:fwrite("ChildStatus: ~w~n", [ChildStatus]),
    NewState = case ChildStatus of
                   ready ->
                       gen_server:cast(ChildPid, run),
                       State;
                   success ->
                       case ChildNum =:= length(Children) of
                           true ->
                               io:fwrite("Sequence success~n"),
                               State#state {status = success};
                           false ->                               
                               io:fwrite("Next sequence~n"),
                               State#state {sequence_num = SequenceNum + 1}
                       end;
                   failure ->
                       io:fwrite("Sequence failure~n"),
                       State#state {status = failure};
                   running ->
                       State
               end,
    NewState.

