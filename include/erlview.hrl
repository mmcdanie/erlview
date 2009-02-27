%% for erlview.erl
%%
-ifndef(STATE).
-define(STATE, 1).


-record(state, {fun_was="everywhere"}).

-record(response, 
	{
	  true = "true\"\n"
	  , success  = true
	  , failure  = {error, unknown}
	  , fun_insert = {ok, fun_inserted}
%
	  , bad_eval   = {error, fun_didnt_eval_try_another_fun}
	  , fun_exists = {error, fun_exists}
	  , ets_fail   = {error, ets_failure}
	 }
       ).

-else.
-endif.

%% end $Id: erlview.hrl,v 1.6 2009/02/27 00:49:26 mmcdanie Exp mmcdanie $
