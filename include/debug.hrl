
%% the LOG(T) is from mats.cronqvist(ericsson); shows function name and args
%% use, thusly, e.g. foo(A,B) -> ?LOG([{A,B}]) , blah blah blah .
%-define(debug,1). %% comment out -define(debug,1) to turn it off (*any* debug def turns it on)
% from Joe Armstrong's new book
% c(block, {d, debug}) to turn on TRACE
-ifdef(debug).
-define(TRACE(X), io:format("TRACE ~p:~w ~p~n" ,[?MODULE, ?LINE, X])).
-define(LOG(T),error_logger:info_report([process_info(self(),current_function)|T])).
-define(LOGy(T),error_logger:info_report([process_info(self(),current_function)|T])).
-define(SHOWLINENUM, io:fwrite("line ~p~n", [?LINE]), timer:sleep(100)).
-else.
-define(TRACE(X), void).
-define(LOG(T),"").
-define(LOGy(T),"").
-define(SHOWLINENUM, "").
-endif.
