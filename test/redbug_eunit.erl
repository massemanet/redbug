-module('redbug_eunit').
-author('mats cronqvist').

-dialyzer(no_improper_lists).

-include_lib("eunit/include/eunit.hrl").

millisecond_test_() ->
    {"option `print_msec' produces HH:MM:SS.mmm timestamps",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort", [print_msec]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:26.663 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              Timestamp = get_line_seg(Content, 1, 2),
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual({match, [{0,12}, {0,0}, {12,0}]},
                             re:run(Timestamp, "(.*)[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}(.*)", [{capture, all}]))]
      end}}.

arity_test_() ->
    {"option `arity' prints function arity instead of arguments",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort", [arity]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:26 <0.445.0>({erlang,apply,2})
              %% % lists:sort/1
              [?_assertEqual(<<"lists:sort/1">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual(3,
                             length(re:split(get_line_seg(Content, 1, 2), "[:.]")))]
      end}}.

microsecond_test_() ->
    {"option `{print_time_unit, microsecond}' produces HH:MM:SS.mmmmmm timestamps",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort", [{print_time_unit, microsecond}]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:26.927090 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              Timestamp = get_line_seg(Content, 1, 2),
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual({match, [{0,15}, {0,0}, {15,0}]},
                             re:run(Timestamp, "(.*)[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{6}(.*)", [{capture, all}]))]
      end}}.

buffered_test_() ->
    {"option `buffered' collects call and return into a single message",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->return", [buffered]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:27 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              %%
              %% % 19:58:27 <0.445.0>({erlang,apply,2})
              %% % lists:sort/1 -> [1,2,3]
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual([<<"lists:sort/1">>, <<"->">>, <<"[1,2,3]">>],
                             get_line_seg(Content, 4, 2, 4))]
      end}}.

return_stack_test_() ->
    {"action `stack' prints the call stack",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->stack", []),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:27 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              %% %   redbug_eunit:'-return_stack_test_/0-fun-9-'/0
              %% %   eunit_test:enter_context/4
              %% %   eunit_proc:run_group/2
              %% %   eunit_proc:tests_inorder/3
              %% %   eunit_proc:with_timeout/3
              %% %   eunit_proc:run_group/2
              %% %   eunit_proc:tests_inorder/3
              %% %   eunit_proc:with_timeout/3
              %% %   eunit_proc:run_group/2
              %% %   eunit_proc:child_process/2
              Lines = lists:seq(3, lines(Content)-1),
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertNotEqual([], Lines),
               ?_assert(lists:all(fun (L) -> is_mfa(get_line_seg(Content, L, 2)) end, Lines))]
      end}}.

proc_send_test_() ->
    {"trace atom `send' captures messages sent by a process specified by option `procs'",
     {setup,
      fun () ->
              Pid = spawn(fun ding_dong_listener/0),
              redbug_start(?FUNCTION_NAME, send, [{procs, Pid}]),
              Pid ! self(),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:58:27 <0.657.0>(dead)
              %% % <0.445.0>({erlang,apply,2}) <<< ding
              [?_assertEqual(<<"ding">>,
                             get_line_seg(Content, 2, 4))]
      end}}.

proc_receive_test_() ->
    {"trace atom `receive' captures messages received by a process specified by option `procs'",
     {setup,
      fun () ->
              Pid = spawn(fun ding_dong_listener/0),
              redbug_start(?FUNCTION_NAME, 'receive', [{procs, Pid}]),
              Pid ! pling,
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 20:04:32 <0.665.0>({erlang,apply,2})
              %% % <<< pling
              [?_assertEqual(<<"pling">>,
                             get_line_seg(Content, 2, 3))]
      end}}.

ding_dong_listener()->
    receive
        P when is_pid(P) -> P ! ding;
        quit -> ok
    after
        1500 -> timeout
    end.

call_time_test_() ->
    {"action `time' reports the call duration",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->time", []),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 19:55:17 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              %% %      1 :      2 :    2.0 : lists:sort/1
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual(<<"lists:sort/1">>,
                             get_line_seg(Content, 3, 8))]
      end}}.

call_count_test_() ->
    {"action `count' reports the number of calls",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->count", []),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 20:04:32 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              %%
              %% %      1 : lists:sort/1
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual(<<"lists:sort/1">>,
                             get_line_seg(Content, 3, 4))]
      end}}.

blocking_test_() ->
    {"option `blocking' returns trace results synchronously",
     {setup,
      fun () ->
              Options = [blocking, arity, {time, 499}, debug],
              {timeout, Msgs} = redbug:start(["erlang:demonitor", "erlang:monitor"], Options),
              Msgs
      end,
      fun (_Msgs) ->
              ok
      end,
      fun (Msgs) ->
              %% Example output:
              %% [{call,{{erlang,demonitor,1},<<>>},
              %%        {<0.445.0>,{erlang,apply,2}},
              %%        {20,10,36,706682}},
              %%  {call,{{erlang,monitor,2},<<>>},
              %%        {<0.445.0>,{erlang,apply,2}},
              %%        {20,10,36,706695}}]
              [?_assertMatch([{call, {{erlang, demonitor, 1}, _}, _, _},
                              {call, {{erlang, monitor, 2}, _}, _, _}],
                             Msgs)]
      end}}.

trace_file_test_() ->
    {"option `file' writes trace files",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->return", [{file, "foo"}]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              {2, Msgs} = replay_trc:go("foo0.trc", fun(E, A) -> [E]++A end, []),
              Msgs
      end,
      fun (_Msgs) ->
              maybe_delete("foo0.trc")
      end,
      fun (Msgs) ->
              %% Example output:
              %% [{trace_ts,<0.445.0>,return_from,{lists,sort,1},[1,2,3],{1778,443837,230786}},
              %%  {trace_ts,<0.445.0>,call,{lists,sort,[[3,2,1]]},{1778,443837,230776}}]
              [?_assertMatch([{trace_ts, _, return_from, {lists, sort, 1}, [1, 2, 3], _},
                              {trace_ts, _, call, {lists, sort, [[3, 2, 1]]}, _}],
                             Msgs)]
      end}}.

no_return_test_() ->
    {"option `{print_return, false}' suppresses the return value",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "lists:sort->return", [{print_return, false}]),
              [1, 2, 3] = lists:sort([3, 2, 1]),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % lists:sort([3,2,1])
              %%
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % lists:sort/1 -> '...'
              [?_assertEqual(<<"lists:sort([3,2,1])">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual([<<"lists:sort/1">>, <<"->">>, <<"'...'">>],
                             get_line_seg(Content, 4, 2, 4))]
      end}}.

improper_list_test_() ->
    {"improper lists are properly output",
     {setup,
      fun () ->
              redbug_start(?FUNCTION_NAME, "redbug_eunit:ipl()->return", []),
              ipl(),
              redbug_normal_stop(),
              redbug_output(?FUNCTION_NAME)
      end,
      fun (_Content) ->
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun (Content) ->
              %% Example output:
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % redbug_eunit:ipl()
              %%
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % redbug_eunit:ipl/0 -> [a,b|c]
              [?_assertEqual(<<"redbug_eunit:ipl()">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual([<<"redbug_eunit:ipl/0">>, <<"->">>, <<"[a,b|c]">>],
                             get_line_seg(Content, 4, 2, 4))]
      end}}.

%% test printing of improper lists
ipl() -> [a, b|c].

loct_stripped_non_arity_return_test_() ->
    {"tracing a non-exported function with action `return' works when LocT is stripped",
     {setup,
      fun () ->
              M = load_stripped_module(),
              redbug_start(?FUNCTION_NAME, "stripped_mod:local_fun->return", []),
              M:exported_fun(5),
              redbug_normal_stop(),
              Content = redbug_output(?FUNCTION_NAME),
              {M, Content}
      end,
      fun ({M, _Content}) ->
              unload_module(M),
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun ({_M, Content}) ->
              %% Example output:
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun(5)
              %%
              %% % 20:04:33 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun/1 -> 10
              [?_assertEqual(<<"stripped_mod:local_fun(5)">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual([<<"stripped_mod:local_fun/1">>, <<"->">>, <<"10">>],
                             get_line_seg(Content, 4, 2, 4))]
      end}}.

loct_stripped_full_module_return_test_() ->
    {"tracing a full module with action `return' works when LocT is stripped",
     {setup,
      fun () ->
              M = load_stripped_module(),
              redbug_start(?FUNCTION_NAME, "stripped_mod->return", []),
              M:exported_fun(5),
              redbug_normal_stop(),
              Content = redbug_output(?FUNCTION_NAME),
              {M, Content}
      end,
      fun ({M, _Content}) ->
              unload_module(M),
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun ({_M, Content}) ->
              %% Example output:
              %% % 20:10:37 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:exported_fun(5)
              %%
              %% % 20:10:37 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun(5)
              %%
              %% % 20:10:37 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun/1 -> 10
              %%
              %% % 20:10:37 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:exported_fun/1 -> 10
              [?_assertEqual(<<"stripped_mod:local_fun(5)">>,
                             get_line_seg(Content, 4, 2)),
               ?_assertEqual([<<"stripped_mod:local_fun/1">>, <<"->">>, <<"10">>],
                             get_line_seg(Content, 6, 2, 4))]
      end}}.

loct_stripped_non_arity_count_test_() ->
    {"tracing a non-exported function with action count works when LocT is stripped",
     {setup,
      fun () ->
              M = load_stripped_module(),
              redbug_start(?FUNCTION_NAME, "stripped_mod:local_fun->count", []),
              M:exported_fun(5),
              redbug_normal_stop(),
              Content = redbug_output(?FUNCTION_NAME),
              {M, Content}
      end,
      fun ({M, _Content}) ->
              unload_module(M),
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun ({_M, Content}) ->
              %% Example output:
              %% % 20:10:37 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun(5)
              %%
              %% %      1 : stripped_mod:local_fun/1
              [?_assertEqual(<<"stripped_mod:local_fun(5)">>,
                             get_line_seg(Content, 2, 2)),
               ?_assertEqual(<<"stripped_mod:local_fun/1">>,
                             get_line_seg(Content, 3, 4))]
      end}}.

loct_stripped_full_module_count_test_() ->
    {"tracing a full module with action `count' works when LocT is stripped",
     {setup,
      fun () ->
              M = load_stripped_module(),
              redbug_start(?FUNCTION_NAME, "stripped_mod->count", []),
              M:exported_fun(5),
              redbug_normal_stop(),
              Content = redbug_output(?FUNCTION_NAME),
              {M, Content}
      end,
      fun ({M, _}) ->
              unload_module(M),
              Filename = output_filename(?FUNCTION_NAME),
              maybe_delete(Filename)
      end,
      fun ({_M, Content}) ->
              %% Example output:
              %% % 20:10:38 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:exported_fun(5)
              %%
              %% % 20:10:38 <0.445.0>({erlang,apply,2})
              %% % stripped_mod:local_fun(5)
              %%
              %% %      1 : stripped_mod:local_fun/1
              %%
              %% %      1 : stripped_mod:exported_fun/1
              [?_assertEqual(<<"stripped_mod:local_fun(5)">>,
                             get_line_seg(Content, 4, 2)),
               ?_assertEqual(<<"stripped_mod:local_fun/1">>,
                             get_line_seg(Content, 5, 4))]
      end}}.

load_stripped_module() ->
    TestCode =
        "-module(stripped_mod)."
        "-export([exported_fun/1])."
        "exported_fun(X) -> local_fun(X)."
        "local_fun(X) -> X*2.",
    Opts = [deterministic, no_line_info],

    {ok, stripped_mod, Bin} = compile_str(TestCode, Opts),
    {ok, {stripped_mod, StrippedBin}} = beam_lib:strip(Bin),
    write_beam(stripped_mod, StrippedBin),

    %% Verify beam_lib:strip does compressing
    %% 16#1f8b: gzip magic numbers
    %% 16#08: Compression method: deflate
    <<16#1f, 16#8b, 16#08, _/binary>> = StrippedBin,
    %% verify LocT is actually gone
    {error, beam_lib, _} = beam_lib:chunks(StrippedBin, [locals]),
    stripped_mod.

compile_str(Str, Opts) ->
    Lines = string:split(Str, ".", all),
    Forms = lists:reverse(forms_from_string(Lines, [])),
    compile:forms(Forms, Opts).

forms_from_string([], Acc) ->
    Acc;
forms_from_string([[]|Lines], Acc) ->
    forms_from_string(Lines, Acc);
forms_from_string([L|Lines], Acc) ->
    {ok, T, _} = erl_scan:string(L ++ [$.]),
    {ok, F} = erl_parse:parse_form(T),
    forms_from_string(Lines, [F|Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace file utilities

output_filename(Name) ->
    filename(Name, ".txt").

beam_filename(Name) ->
    filename(Name, ".beam").

filename(Name, Ext) ->
    atom_to_list(Name) ++ Ext.

redbug_start(TestName, TraceFun, TraceOpts) ->
    Filename = output_filename(TestName),
    Options = [{print_file, Filename}, debug|TraceOpts],
    {ProcessName, NoProcs, NoFuncs} = redbug:start(TraceFun, Options),
    true = is_process_alive(whereis(ProcessName)),
    true = (NoProcs + NoFuncs > 0).

redbug_normal_stop() ->
    %% collect all traces
    timer:sleep(100),
    redbug:stop().

write_beam(M, Bin) ->
    TmpDir = filename:dirname(code:which(redbug_eunit)),
    TmpFile = filename:join(TmpDir, beam_filename(M)),
    ok = file:write_file(TmpFile, Bin),
    {module, M} = code:load_abs(filename:rootname(TmpFile)).

unload_module(M) ->
    TmpDir = filename:dirname(code:which(redbug_eunit)),
    TmpFile = filename:join(TmpDir, beam_filename(M)),
    ok = file:delete(TmpFile),
    code:delete(M),
    code:purge(M).

redbug_output(Name) ->
    Filename = output_filename(Name),
    Content = read_file(Filename),
    maybe_show(Content),
    Content.

maybe_show(Content) ->
    [io:fwrite("~p~n", [Content]) || in_shell()].

lines(Content) ->
    length(Content).

get_line_seg(Content, Line, Seg) ->
    hd(get_line_seg(Content, Line, Seg, Seg)).

get_line_seg(Content, Line, SegF, SegL) when Line =< length(Content) ->
    [e(S, e(Line, Content)) || S <- lists:seq(SegF, SegL)];
get_line_seg(Content, Line, _SegF, _SegL) ->
    error({line_out_of_bounds, Line, Content}).

read_file(Filename) ->
    {ok, C} = file:read_file(Filename),
    [[S||S<-re:split(L, "\\s"), S=/=<<>>]||L<-re:split(C, "\n"), L=/=<<>>].

is_mfa(H) ->
    L = byte_size(H),
    {match, [{0, L}]} =:= re:run(H, "[a-zA-Z0-9\'/:_-]*/[0-9]+").

maybe_delete(Filename) ->
    [file:delete(Filename) || not in_shell()].

in_shell() ->
    lists:member("shell:eval_loop/3", stack()).

stack() ->
    stack(self()).

stack(P) ->
    [string:trim(e(2, (string:tokens(L, "(+)")))) || L<- bt(P), $0 =:= hd(L)].

bt(P) ->
    string:tokens(binary_to_list(e(2, (process_info(P, backtrace)))), "\n").

e(N, L) when is_list(L) -> lists:nth(N, L);
e(N, T) when is_tuple(T)-> element(N, T).
