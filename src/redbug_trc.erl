-module(redbug_trc).
-author('Mats Cronqvist').

-export(
   [fold/3]).

-record(state,
        {fn,
         acc,
         files = [],
         fd = null,
         msgs = 0,
         ptr = 0,
         chunksz = 1048576,
         chunk = <<>>,
         state = eof
        }).

fold(Fun, Acc, File) ->
    case files(File) of
        [] -> no_files_found;
        Files -> fold(#state{fn = Fun, acc = Acc, files = Files})
    end.

fold(#state{state = reading, ptr = Ptr, fn = Fun, acc = Acc} = Z) ->
    case Z#state.chunk of
        <<_:Ptr/binary, 0, S:32/integer, X:S/binary, _/binary>> -> 
            fold(Z#state{ptr = Ptr+5+S, acc = Fun(X, Acc), msgs = Z#state.msgs+1});
        _ ->
            fold(read(Z))
    end;
fold(#state{state = eof} = Z) ->
    fold(read(Z));
fold(#state{state = done} = Z) ->
    Z.

files(F) ->
    case {filelib:is_regular(F), filelib:is_dir(F)} of
        {true, false} -> [F];
        {false, true} -> filelib:wildcard("*.trc", F);
        {false, false} -> filelib:wildcard(F++"*.trc")                
    end.

read(#state{state = eof, files = []} = Z) ->
    Z#state{state = done};
read(#state{state = eof, files = [File|Files]} = Z) ->
    case file:open(File, [read, raw, binary, compressed]) of
        {ok, FD} -> read(Z#state{state = reading, fd = FD, files = Files});
        Err -> error({open, File, Err})
    end;
read(#state{state = reading, fd = FD, ptr = Ptr} = Z) ->
    <<_:Ptr/binary, R/binary>> = Z#state.chunk,
    case file:read(FD, Z#state.chunksz) of
        {ok, B} -> Z#state{ptr = 0, chunk = <<R/binary, B/binary>>};
        eof -> read(Z#state{state = eof, fd = file:close(FD), ptr = 0, chunk = R})
    end.
