%%%-------------------------------------------------------------------
%%% File    : erlview_test.erl
%%% Author  : mmcdanie <>
%%% Description : intended to be -include("erlview_test.erl") by
%%%               erlview.erl
%%%
%%% Currently needs to be run from within an interactive couchdb 
%%% session.
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
   , timer:sleep(555)
.



reset_test() -> 
    Data = [1,2,3] ,
    R = #response{} ,
    ?assert(  R#response.success == gen_server:call(erlview, {reset, Data}) )
%% handle_call({reset, Data}, self(), #state{}) )
   , timer:sleep(555)
.



ets_clean( BinFuns ) ->
						% see erlview.erl for ets:insert
    lists:map( fun(BinFun) ->
		       FunStr = binary_to_list( BinFun ) ,
		       {ok, Tokens, _} = erl_scan:string(FunStr) ,
		       Fcrypt = crypto:sha( term_to_binary(Tokens) ) ,
		       ets:match_delete(?FUNTABLE, {'_', '_', Fcrypt, '_'} )
	       end , 
	       BinFuns )
.




% expand this to enter funs from a file
% also check that fun was inserted into ets table, and then remove it
% from the table
add_fun_1_test() -> 
    Good_fun  = [<<"add_fun">>,<<"fun(Doc) -> x end.">>] ,
    R = #response{} ,
    ?assert( R#response.success == gen_server:call(erlview, {prompt,  Good_fun})) ,
    ets_clean( Good_fun ) ,
    timer:sleep(555)
.



add_fun_2_test() -> 
    Good_funk = [<<"add_fun">>,
		 [<<"fun(Doc) -> Doc end.">>, 
		  <<"fun(Doc) -> 
                         erlview:entire_doc(Doc, {<<\"name\">>,all}) end.">>]] ,
    R = #response{} ,
    ?assert( R#response.success == gen_server:call(erlview, {prompt, Good_funk})) ,
    ets_clean( lists:nth(2,Good_funk) ) ,
    timer:sleep(555)
.



add_fun_3_test() -> 
    Good_fun2 = [<<"add_fun">>,
		 <<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"when\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>] ,
    R = #response{} ,
    ?assert(R#response.success == gen_server:call(erlview, {prompt, Good_fun2})) ,
    ets_clean( Good_fun2 ) ,
    timer:sleep(555)
.



add_fun_4_test() -> 
    Good_fun3 = [<<"add_fun">>,
		 [<<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"when\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>,
                  <<"fun( Doc ) -> \n  One = element(1,element(4,Doc)) ,\n  case lists:keysearch(<<\"who\">>, 1, One) of\n  {value, _Value} -> \n  {null, { lists:append(\n[ {<<\"_id\">>, element(2, Doc)},{<<\"_rev\">>,hd(element(3, Doc))}\n] , One) } } ;\n  _               -> []\n  end\nend.">>]] ,
    R = #response{} ,
    ?assert(R#response.success == gen_server:call(erlview, {prompt, Good_fun3})) ,
    ets_clean( lists:nth(2,Good_fun3) ) ,
    timer:sleep(555)
.



add_fun_5_test() -> 
    Bad_fun  = [<<"add_fun">>,<<"fun(Doc) -> x end ">>] ,   % missing end dot
    R = #response{} ,
    ?assert( R#response.success /= gen_server:call(erlview,
						   {prompt,
						    Bad_fun})) ,
    ?assert( R#response.bad_eval == gen_server:call(erlview,
						    {prompt,
						     Bad_fun})) ,
    timer:sleep(555)
.



add_fun_6_test() -> 
    Bad_fun2 = [<<"add_fun">>,<<"function(doc) { emit(null, doc); }">>] , % javascript
    R = #response{} ,
    ?assert( R#response.success /= gen_server:call(erlview,
						   {prompt,
						    Bad_fun2})) ,
    
    ?assert( R#response.bad_eval == gen_server:call(erlview,
						    {prompt,
						     Bad_fun2})) ,
    timer:sleep(555)
.



% expand this to enter docs from command line or read a file
map_doc_test() -> 
    Data = [ <<"map_doc">>, 
	{doc,<<"1e8fef9320ca4b8dc2e8265e28e11c2c">>,
	 [<<"1998958978">>],
	 {[{<<"who">>,<<"fuzzy bear">>},
	   {<<"what">>,<<"a new test">>},
	   {<<"when">>,<<"2009/01/17 23:30">>},
	   {<<"note">>,
	    <<"yadda yadda, SNL is on">>}]},
	 [],false,[]} ] ,
    
    Response = gen_server:call(erlview, {prompt, Data}) ,
    ?assert( is_list( Response ) ) ,
    timer:sleep(555)
.


                    

-else.
-endif.
%% end $Id: erlview_test.erl,v 1.6 2009/02/16 04:02:02 mmcdanie Exp mmcdanie $
