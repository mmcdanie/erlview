%%%-------------------------------------------------------------------
%%% File    : erlview_test.erl
%%% Author  : mmcdanie <>
%%% Description : intended to be -include("erlview_test.erl") by
%%%               erlview.erl
%%%
%%% Currently needs to be run from within an interactive couchdb 
%%% session.  And session needs restarting after the tests because
%%% at the moment test funs are inserted into ets table but not
%%% removed.
%%%
%%% Created : 31 Jan 2009 by mmcdanie <>
%%%-------------------------------------------------------------------



-ifdef(test).

-include_lib("eunit/include/eunit.hrl").

-include("erlview.hrl").


% this must run before *_fun tests which depend on ets table being available
 init_test() ->
    case lists:member( couch_server, registered() ) of
	true  -> dont_run_init_again ;

	false -> exit(run_tests_from_interactive_couchdb)
    end
   , timer:sleep(333)
.



reset_test() -> 
    Data = [1,2,3] ,
    R = #response{} ,
    ?assert(  R#response.success == gen_server:call(erlview, {reset, Data}) )
%% handle_call({reset, Data}, self(), #state{}) )
   , timer:sleep(333)
.



ets_clean( BinFuns ) ->

    lists:map( fun(BinFun) ->
		       FunStr = binary_to_list( BinFun ) ,
		       {ok, Tokens, _} = erl_scan:string(FunStr) ,
		       Fcrypt = crypto:sha( term_to_binary(Tokens) ) ,
		       ets:match_delete(?FUNTABLE, {Fcrypt, '$1'} )
	       end , BinFuns )
.


prompt_test() ->
    R = #response{} ,
    Data  = [<<"add_fun">>, <<"fun(Doc) -> x end.">>] ,
    ?assert(  R#response.success == gen_server:call(erlview, {prompt, Data}) )
   , timer:sleep(333)
.



%% 
%% How can I start erlview so the Pid is returned from this call ?
%% I could eliminate the start_docs_map fun addition to couch_query_servers
%%
%% get_os_process(Lang) ->
%%     gen_server:call(couch_query_servers, {get_proc, Lang})
%%    , timer:sleep(333)
%% .




% expand this to enter funs from a file
% also check that fun was inserted into ets table, and then remove it
% from the table
add_fun_1_test() -> 
    Good_fun  = [<<"fun(Doc) -> x end.">>] ,
    R = #response{} ,
    ?assert( R#response.success == gen_server:call(erlview, {add_fun, Good_fun})) ,
    ets_clean( Good_fun ) ,
    timer:sleep(333)
.



add_fun_2_test() -> 
    Good_funk = [<<"fun(Doc) -> y end.">> ,<<"fun(Doc) -> z end.">>] ,
    R = #response{} ,
    ?assert( R#response.success == gen_server:call(erlview, {add_fun, Good_funk})) ,
    ets_clean( Good_funk ) ,
    timer:sleep(333)
.



add_fun_3_test() -> 
    Good_fun2 = [<<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"when\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>] ,
    R = #response{} ,
    ?assert(R#response.success == gen_server:call(erlview, {add_fun, Good_fun2})) ,
    ets_clean( Good_fun2 ) ,
    timer:sleep(333)
.



add_fun_4_test() -> 
    Good_fun3 = [<<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"when\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>,
                  <<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"who\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>] ,
    R = #response{} ,
    ?assert(R#response.success == gen_server:call(erlview, {add_fun, Good_fun3})) ,
    ets_clean( Good_fun3 ) ,
    timer:sleep(333)
.



add_fun_5_test() -> 
    Bad_fun  = [<<"fun(Doc) -> x end ">>] ,   % missing end dot
    R = #response{} ,
    ?assert( R#response.success /= gen_server:call(erlview,
						   {add_fun,
						    Bad_fun})) ,
    ?assert( R#response.bad_eval == gen_server:call(erlview,
						    {add_fun,
						     Bad_fun})) ,
    timer:sleep(333)
.



add_fun_6_test() -> 
    Bad_fun2 = [<<"function(doc) { emit(null, doc); }">>] , % javascript
    R = #response{} ,
    ?assert( R#response.success /= gen_server:call(erlview,
						   {add_fun,
						    Bad_fun2})) ,
    
    ?assert( R#response.bad_eval == gen_server:call(erlview,
						    {add_fun,
						     Bad_fun2})) ,
    timer:sleep(333)
.



% expand this to enter docs from command line or read a file
map_docs_test() -> 
    Doc = 
	{doc,<<"1e8fef9320ca4b8dc2e8265e28e11c2c">>,
	 [<<"1998958978">>],
	 {[{<<"who">>,<<"fuzzy bear">>},
	   {<<"what">>,<<"a new test">>},
	   {<<"when">>,<<"2009/01/17 23:30">>},
	   {<<"note">>,
	    <<"yadda yadda, SNL is on">>}]},
	 [],false,[]} ,
    
    Response = gen_server:call(erlview, {map_doc, Doc}) ,
    ?assert( is_list( Response ) ) ,
    timer:sleep(333)
.


                    

-else.
-endif.
%% end $Id: erlview_test.erl,v 1.6 2009/02/16 04:02:02 mmcdanie Exp mmcdanie $
