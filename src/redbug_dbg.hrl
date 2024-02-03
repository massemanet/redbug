%% a debug macro, cause redbugging redbug is difficult.

-compile([{nowarn_unused_function, [{dbg, 1}]}]).
dbg(Z) -> io:fwrite(standard_error, "~n~p:~w ~s:~s: ~p~n", Z),
          case hd(Z) of error -> error(tl(Z)); _ -> Z end.
-define(DBG(Tag, X), (fun(_X) -> dbg([Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, _X]), _X end)(X)).
