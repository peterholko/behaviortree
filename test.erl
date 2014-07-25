-module(test).

-export([run/0]).


run() ->
    F = fun() -> timer:sleep(5000), io:fwrite("Hello World~n") end,
    H = fun() -> timer:sleep(7000), io:fwrite("Peter Holko~n") end,
    {ok, Pid1} = behtree:setup(1, action, F), 
    {ok, Pid2} = behtree:setup(2, action, H),
    {ok, SPid} = behtree:start(3, sequence),    
    io:fwrite("Adding children~n"),
    gen_server:cast(SPid, {add_child, Pid1}),
    gen_server:cast(SPid, {add_child, Pid2}),
    gen_server:cast(SPid, run),
    timer:sleep(2000),
    gen_server:cast(SPid, run),
    timer:sleep(2000),
    gen_server:cast(SPid, run),
    timer:sleep(2000),
    gen_server:cast(SPid, run),
    timer:sleep(2000),
    gen_server:cast(SPid, run).
