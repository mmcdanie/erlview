%%%-------------------------------------------------------------------
%%% File    : $Id: erlview.erl,v 1.36 2009/02/22 16:12:18 mmcdanie Exp mmcdanie $
%
%% @doc erlview, an Erlang View Server for CouchDB
%
% Copyright 2009 Michael McDaniel [http://autosys.us]
%
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
%
% You may obtain a copy of the License at
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, 
% software distributed under the License is distributed on an 
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
% either express or implied. 
%
% See the License for the specific language governing permissions
% and limitations under the License. 
%
% @author   : (originally) Michael McDaniel [http://autosys.us]
% @reference: used with CouchDB [http://couchdb.apache.org]
% @reference: Now at <a href="http://github.com/mmcdanie/erlview/tree/master">http://github.com.</a>
%%
%%
%% 
%%
%% @type key_pairs(). list( {field, content} )  where field::binary() 
%%  and content::binary() describe document field names or field contents.
%%
%% @type fields(). list( field )  where field::binary() describes a document
%%  field name.
%%
%% @end
%%
%%% Description : 
%%% Created : 24 Jan 2009 by mmcdanie <>
%%
%% $Log: erlview.erl,v $
%% Revision 1.36  2009/02/22 16:12:18  mmcdanie
%% removed queue table; just shouldn't be needed; still don't know
%% view name/content mixup
%% need to ween off of RCS and learn how to do everything I want
%% with git
%%
%% Revision 1.35  2009/02/17 06:38:52  mmcdanie
%% refactored entire_doc/2
%%
%% Revision 1.34  2009/02/17 06:08:22  mmcdanie
%% refactored find_all_content/2 ; cursory testing works same as before; NOTE that
%% multiple entire_doc/2 work fine with name/views (no mixup); add a single
%% find_all_content/2 map fun though, and the name/views get mixed up.
%% Need more testing around this but, on test server I saw mixups with
%% entire_doc/2 map funs also.
%%
%% Revision 1.33  2009/02/16 04:06:25  mmcdanie
%% *** empty log message ***
%%
%% Revision 1.32  2009/02/16 03:59:18  mmcdanie
%% *** empty log message ***
%%
%% Revision 1.31  2009/02/16 03:52:40  mmcdanie
%% *** empty log message ***
%%
%% Revision 1.30  2009/02/15 18:03:25  mmcdanie
%% just worked correctly with entire_doc( ... who/when/what/note ... ) 
%% and name/view matched fine.  
%% Using Fcrypt key in handle_call( {map_doc ...) and dropped ordered_set
%% option when creating both ets tables (cdb_table and erlview_queue)
%%
%% Revision 1.29  2009/02/15 17:27:45  mmcdanie
%% added a queue table for, I think, preventing multiple concurrent views
%% from running every map fun again; i.e. when a new handle_call( {map_fun ...)
%% is called, it checks if the queue table is empty and if so, populates it
%% with all map funs stored in the cdb_table and then removes each fun
%% (not quite implemented) as it runs it; contrary,
%% if a new handle_call( {map_fun ...) is called and the table is not empty,
%% it uses the map funs in the queue table
%% Currently, as long as each map fun is entered correct (in Futon) then everything
%% runs; still have name/view mixup (??!!??$#%!!!), and need to check for bad eval
%% and runtime errors.
%%
%% ... see 'rlog erlview.erl' for missing log notes
%%
%% Revision 1.26  2009/02/15 07:51:17  mmcdanie
%% works now and can correct bad map funs whether the problem is bad eval or runtime
%% error; HOWEVER, the name/view relationship is still mixed up as after adding
%% multiple names (at least, in Futon) when I call a view it is not the same as
%% the name of the view (i.e. the output is for a different name).
%% So, still don't know what's up with that but at least CDB doesn't need
%% restarting now just because a bad map fun was entered (runtime error map fun).
%%
%% Revision 1.25  2009/02/15 02:28:35  mmcdanie
%% seems to work, i.e. each implemented map fun helper.  Though the name/view still
%% gets mixed up and a bad view can spoil the bunch.  I think that if a map fun
%% has a runtime failure, that's what messes everything up.  Maybe figure out
%% how to catch the runtime failure and remove that view from ets.  Because,
%% it's stuck there when the view is resaved (because the key in ets is
%% a crypt of the fun).  Hmm, if I can figure out how to capture the name under
%% which the map fun is saved then this might not be a problem (i.e. overwrite
%% the runtime erroring fun).  That can't be that hard, can it? Finding out
%% the name under which a map fun is saved?
%%
%% Revision 1.23  2009/02/14 17:10:57  mmcdanie
%% each helper fun seems to work ok in a map fun, but only the first time
%% (unless I save a view); something is still messed up w/saving & recovering
%% funs
%%
%%
%% Revision 1.16  2009/02/11 02:56:03  mmcdanie
%% hurray!  I can now return a subset of documents.  Change was in xemit
%% (of course) and the "not found" return value is the entire document
%%  without the Key
%%
%% Revision 1.15  2009/02/10 06:24:12  mmcdanie
%% xemit/2 now working for simple pattern; can't yet find a field content, only
%% if the field exists
%%
%% Revision 1.13  2009/02/09 20:36:14  mmcdanie
%% fun(Body) -> erlview:xemit(Body) end.  will now work.
%% need to fully qualify xemit, and note that the name emit causes
%% failure (apparently
%%
%%
%% Revision 1.6  2009/02/01 21:50:03  mmcdanie
%% someday I'll remember to compile before saving !
%%
%% Revision 1.5  2009/02/01 21:48:37  mmcdanie
%% works with proper stored function (only tested with a single function
%% and three documents)
%%
%% ...
%%
%% Revision 1.1  2009/01/31 02:32:06  mmcdanie
%% Initial revision
%%
%% TODO:
%%
%%DONE 1) in handle_call( {map_doc ... ) figure out what consitutes null for a 
%%     fun(Body) view  (as in, don't show document N)
%%     [answer: leave off the Key value when returning the doc, see code, Luke]
%%DONE  2) make some helper funs for writing views ala jsscript emit(null, doc.name)
%%(though they're named 'funny')
%%  3) when I save two or more views in Futon, the wrong one runs for the name
%%     (i.e. they're swapped); s'up with that?  I'm guessing something about how
%%     the funs are saved and called to run
%%DONE  4) rewrite one_field to be find_all_fields
%%  5) Add magic word 'all' or 'doc' to each helper fun so that the entire document
%%     can be returned instead of enumerating each field.  If Out_fields = all
%%     return entire doc (i.e. if requisite keys are found).  That way the silly
%%     entire_doc/2 fun can be eliminated.
%%  6) Implement reduce.
%%  7) Implement log.
%%  8) Add search for any doc field (already finding body fields)
%%     #doc{id=Id,deleted=Del,body=Body,revs=Revs,meta=Meta}
%%  9) Change helper funs to accept atoms or lists 
%%     ( "for fields or content with spaces" ); that is, instead of requiring
%%     binaries in the map funs.
%% 10)
%% 11)
%%
%%
%% NOTES:
%% 1) In /usr/local/etc/couchdb/local.ini 
%%    I have the following (not as comments, of course)
%
%    erlang = erl -sname fubar -noshell -detached   (NEEDED on earlier CDB)
%    erlang = /dev/null  (THIS WORKS w/v 740870
%    [daemons]
%    erlview = {erlview, start_link, []}
%
%% The 'erlang = ...' line is apparently needed so that Futon (and maybe other
%% parts of cdb) recognizes erlang as a language ; 
%% I _think_ the following works as well and gives up the memory when it stops;
%% I need to test it in more situations but a simple case worked ok.
%
%   erlang = erl -eval 'timer:sleep(3000), init:stop().'
%
%%
%% 2) If a reset is needed this will work ... 
%%    curl  -X POST --header "Content-Type: application/erlang"  \
%%       http://192.168.1.7:5984/daptest/_slow_view/reset
%%
%% 3) I'm not sure if deleted or meta data should be included in the
%%    returned documents.
%%
%%%-------------------------------------------------------------------
-module(erlview).

-behaviour(gen_server).

-include("debug.hrl").
-include("erlview.hrl").
-include("couch_db.hrl").
%% #doc{id=Id,deleted=Del,body=Body,revs=Revs,meta=Meta}=Doc

-author('Michael McDaniel dba Automated Systems, http://autosys.us').
-copyright('2009 Michael McDaniel dba Automated Systems, http://autosys.us').
-license('Apache 2.0, see the documentation').

-define(SERVER, ?MODULE).
-define(FUNTABLE, cdb_table).
-define(QTABLE, erlview_queue).

-define(ERROR(T),error_logger:error_report([process_info(self(),current_function)|T])).
%% API
%% -compile(export_all).  %% TESTING ONLY

-export([start_link/0]).
%% map fun helper funs
-export([find_all_content/2, find_any_content/2]).
-export([find_all_fields/2, find_any_fields/2, entire_doc/2, entire_doc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-include("erlview_test.erl"). %for testing; has to be after all -export/1



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%====================================================================
%% Helper funs, used in map funs
%%====================================================================

%% @doc
%% Used in map funs, finds documents having any of the requested content, NOT IMPLEMENTED
%%
%%<pre>
%% returns docs containing a foo of "new", or myid of "123"
%% and returns fields myid,who,when
%% fun(Doc) -> erlview:find_any_content(Doc,
%%                          {[ {&lt;&lt;"foo"&gt;&gt;, &lt;&lt;"new"&gt;&gt;}, 
%%                             {&lt;&lt;"myid"&gt;&gt;, &lt;"123"&gt;&gt;} ] 
%%                           [ &lt;&lt;"myid"&gt;&gt;, 
%%                             &lt;&lt;"who"&gt;&gt;, 
%%                             &lt;&lt;"when"&gt;&gt; ] 
%%                          } )
%% end.
%%</pre>
%%
%% find_any_content( Doc::doc(), { Key_pairs::key_pairs() ,
%%                            Out_fields::fields() } ) -> doc()
%% @end
find_any_content( _Doc, {_Key_pairs, _Out_fields} ) -> 
    {error, not_implemented}
.



%% @doc
%% Used in map funs, finds documents having all of the requested content.
%%
%% The following map fun would return all documents which contain "pooh" in the 
%% "who" field and "carrots" in the "what" field, but would only return document
%% fields "who", "when", and "what" from those documents.  Returns first Key 
%% content as 'Key' (i.e. what you'll see in Futon's 'Key' column on left)
%%
%%<pre>
%% fun(Doc) ->
%%    erlview:find_all_content( Doc,
%%                         { [{&lt;&lt;"who"&gt;&gt;,
%%                             &lt;&lt;"pooh"&gt;&gt;},
%%                            {&lt;&lt;"what"&gt;&gt;
%%                             &lt;&lt;"carrots"&gt;&gt;}],
%%                           [ &lt;&lt;"myid"&gt;&gt;,
%%                             &lt;&lt;"who"&gt;&gt;,
%%                             &lt;&lt;"when"&gt;&gt; ]
%%                          } )
%% end.
%%</pre>
%%
%%
%% @spec find_all_content( Doc::doc(), 
%%                    { Key_pairs::key_pairs() ,
%%                      Out_fields::fields() } ) -> doc()
%%
%% @end
find_all_content( Doc, {Key_pairs, Out_fields} ) ->  % all must match
    #doc{id=Id,deleted=_Del,body=Body,revs=Revs,meta=_Meta} = Doc ,
?LOG( [{Body, {Key_pairs, Out_fields}}] ) ,

    Eb = element(1, Body) ,

    Truth_list = lists:map(fun(K) ->              % search doc content
				   lists:map( 
				     fun(Bt) -> K == Bt end, 
				     Eb )
			   end,
			   Key_pairs) ,

				 
    Out = 
  	case is_true(Truth_list)
 	    of true -> Vk = element(2, hd(Key_pairs)) ,
                       Out_Fields = 
			   lists:map( 
			     fun(K) -> 
				     case lists:keysearch(K, 1, Eb) of
					 {value, V} -> V ;
					 _          -> {<<"">>,<<"">>}
				     end
			     end,
			     Out_fields ) ,

                       [{ Vk, {[{<<"_id">>,Id}] 
			       ++ [{<<"_rev">>,hd(Revs)}] 
			       ++ Out_Fields } }] ;
				 
 	     false  -> []
	end ,

    Out
. % find_all_content/2 (formerly xemitx/2)





%% @doc
%% Used in map funs, finds documents having any of the requested fields, NOT IMPLEMENTED
%%
%%<pre>
%% fun(Doc) -> erlview:find_any_fields( Doc, { &lt;&lt;"who"&gt;&gt;, 
%%                                       [&lt;&lt;"who"&gt;&gt;,
%%                                        &lt;&lt;"what"&gt;&gt;,
%%                                        &lt;&lt;"when"&gt;&gt;] } )
%% end.
%%</pre>
%%
%% If any Keys are found in Doc, returns fields requested via Out_fields argument.
%%
%% @spec find_any_fields( Doc::doc(), { Keys::fields(), [Out_fields::fields()] } ) -> doc()
%% @end
find_any_fields( _Doc, {_Keys, _Out_fields} ) ->
  {error, not_implemented}
.





%% @doc
%% Used in map funs, finds documents containing all requested fields.
%%
%%<pre>
%% fun(Doc) -> 
%%     erlview:find_all_fields( Doc, { [&lt;&lt;"who"&gt;&gt;, 
%%                                        &lt;&lt;"what"&gt;&gt;],
%%                                       [&lt;&lt;"who"&gt;&gt;,
%%                                        &lt;&lt;"what"&gt;&gt;,
%%                                        &lt;&lt;"when"&gt;&gt;] } )
%% end.
%%</pre>
%%
%% If all Keys are found in Doc, returns Out_fields.  Returns first Key 
%% 'Key' (i.e. what you'll see in Futon's 'Key' column on left)
%%
%% @spec find_all_fields( Doc::doc(), { [Keys::fields()], [Out_fields::fields()] } ) -> doc()
%% @end
%% find_all_fields( Doc, {Keys, all} ) ->
find_all_fields( Doc, {Keys, Out_fields} ) ->
    #doc{id=Id,deleted=_Del,body=Body,revs=Revs,meta=_Meta} = Doc ,
?LOG( [{Body, {Keys, Out_fields}}] ) ,

    Eb = element(1, Body) ,
    Vk = hd(Keys) ,
    Fields = lists:map( 
	       fun(K) -> 
		       case lists:keysearch(K, 1, Eb) of
			   {value, V} -> V ;
			   _          -> {<<"">>,<<"">>}
		       end
	       end,
	       Out_fields ) ,

						% do all Keys match some Body
						% field ?
    Truth_list = lists:map(fun(K) -> 
				   lists:map( 
				     fun(Bt) -> K == element(1,Bt) end, 
				     Eb )
			   end,
			   Keys) ,

     Out = case is_true(Truth_list) and (lists:flatlength(Fields) > 0)
	       of true ->
		   [{ Vk, {[{<<"_id">>,Id}] 
			   ++ [{<<"_rev">>,hd(Revs)}] 
			   ++ Fields } }] ;
	       _       -> []
	   end , 

    Out
. % find_all_fields/2



% [ [true,false,false], [false,false,false,true] ]
is_true(Truth_list) ->
 case  lists:all( fun(T) -> 
			  lists:member(true,T) 
		  end,
		  Truth_list )
     of true -> true ;
     false   -> false
 end
.    


% @doc
% Used in map funs, finds documents containing requested field, returns entire doc.
%
% used thusly from map funs ...
%
%<pre>
% fun(Doc) -> 
%    erlview:entire_doc( Doc, 
%                        { &lt;&lt;"who"&gt;&gt;, all } ) 
% end.
%</pre>
% If Key is found in Doc, returns entire doc.
%
% @spec entire_doc( Doc::doc(), {Key::key(), all} ) -> doc()
%
% @end
entire_doc( Doc, {Key, all} ) ->
    #doc{id=Id,deleted=_Del,body=Body,revs=Revs,meta=_Meta} = Doc ,
    Eb = element(1, Body) ,

    Out = case lists:keysearch(Key, 1, Eb) 
	      of false          -> [] ;

	      {value, {Key,Vk}} -> 
                                   [{ Vk, {[{<<"_id">>,Id}] 
					   ++ [{<<"_rev">>,hd(Revs)}] 
					   ++ Eb } }]
	      end , 

    Out
. % entire_doc/2



% @doc
% Used in map funs, finds documents NOT containing requested field, returns entire doc.
%
% used thusly from map funs ...
%%
%%<pre>
%% fun(Doc) -> erlview:entire_doc( Doc,
%%                                 without,
%%                                 { &lt;&lt;"who"&gt;&gt;, all } ) 
%% end.
%%</pre>
%%
% If Key is NOT found in Doc, returns entire doc.
%
% @spec entire_doc( Doc::doc(), without, {Key::key(), all} ) -> doc()
%
% @end
entire_doc( Doc, without, {Key, all} ) ->
    #doc{id=Id,deleted=_Del,body=Body,revs=Revs,meta=_Meta} = Doc ,
    Eb = element(1, Body) ,

    Not_key = "no " ++ binary_to_list(Key) ++ " field",
    Key_not = list_to_binary(Not_key) ,

    Out = case lists:keysearch(Key, 1, Eb) 
	      of {value, {_Key,_Vk}} -> 
                                   [{          {[{<<"_id">>,Id}] 
						++ [{<<"_rev">>,hd(Revs)}] 
						++ Eb } }] ;

	            false          -> []
	      end , 

    Out
. % entire_doc/3



%% when add_fun is called, then map_doc gets called to run a view




%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
init([]) ->
    ets:new(?FUNTABLE, [public, named_table, ordered_set]) ,

    {ok, #state{fun_was="init"}}
. % init/1



%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}
%% ;


%% [ H | T ] = Data in  couch_os_process:prompt(Pid, Data)
%%                       (called from couch_query_servers)
%% 
%% [ H | T ] = [<<"add_fun">>,<<"function(doc) {\n  emit(null, doc);\n}">>]
%% H = <<"add_fun">> ,
%% T = <<"function(doc) {\n  emit(null, doc);\n}">> ,
%%@doc
%%@private
%%@end
handle_call({prompt, [ H | T ] = _Data}, _From, State) ->
    ?LOG([{?MODULE, prompt, _Data}, _From, State]) ,

    case H of
	<<"add_fun">> -> handle_call( {add_fun, T}, _From, State  )  ;
	<<"map_doc">> -> handle_call( {map_doc, hd(T)}, _From, State ) ;
	<<"reset">>   -> handle_call( {reset, x}, _From, State )
%%                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
%% need to call handle_call/3 directly 
%%
%% cannot "layer" gen_server:calls as below,
%% caller of initiating gen_server:call(erlview, {prompt, Data})
%% receives a timeout when doing the below
%%
%%   	<<"add_fun">> -> gen_server:call(?MODULE, {add_fun, T} ) ;
%%   	<<"map_doc">> -> gen_server:call(?MODULE, {map_docs, T} )
     end
;
handle_call({reset, _Data}, _From, _State) ->
    ?LOG([{reset, _Data}, _From, _State]) ,
    erlang:garbage_collect() ,
    ets:match_delete(?FUNTABLE, '$1') ,
    R = #response{} ,
    {reply, R#response.success, #state{fun_was="reset"}}
;
handle_call( {add_fun, BinFunctions}, _From, _State ) ->
    ?LOG([{?MODULE, add_fun, BinFunctions}, _From, _State]) ,
%
%% thanks to:
%% http://erlang.org/pipermail/erlang-questions/2003-November/010544.html
%% for the scan/parse/eval
%%
%% NOTE that a map fun can evaluate as valid but may have a runtime error.
%%      Worthwhile to put some good feedback in the
%%      funs so when they fail I can figure out why.

    R = #response{} ,
    
    Reply =
	lists:foldl(
	  fun(BinFunction, Acc) ->
		  Acc ,
		  try
		      FunStr = binary_to_list( BinFunction ) ,
		      {ok, Tokens, _} = erl_scan:string(FunStr) ,
		      {ok, [Form]} = erl_parse:parse_exprs(Tokens) ,
		      Bindings = erl_eval:new_bindings() ,
		      {value, Fun, _} = erl_eval:expr(Form, Bindings) ,

						% ets overwrites identical records
						% if table is set or ordered_set
%%      		      Fcrypt = crypto:sha( term_to_binary(Tokens) ) ,    
%%      		      ets:insert(?FUNTABLE,{Fcrypt,term_to_binary(Fun)}) 
     		      Key = calendar:datetime_to_gregorian_seconds({date(),time()}),
     		      ets:insert(?FUNTABLE,{Key,term_to_binary(Fun)}) 

		  of true        -> R#response.success ;

                  {error, R}     -> R#response.ets_fail
	           
		  catch _:_      ->
			  ?ERROR([ R#response.bad_eval ]) ,
			  R#response.bad_eval

		  end 
	  end, 
	  "", 
	  BinFunctions ) ,

    {reply, Reply, #state{fun_was="add_fun"}}

%handle_call/3  add_fun
;
handle_call({map_doc, Doc} , _From , _State) ->
    Fun_list = ets:tab2list(?FUNTABLE) ,
    L = lists:map( fun(Fa) -> G = binary_to_term( element(2, Fa)) , 
			      try  (catch G(Doc))
			      of {'EXIT', _}  -> exit(runtime_error_map_fun) ;
			      Gx              -> Gx
			      catch _:_       -> exit(runtime_error_map_fun) 
			      end
		   end ,
 		   Fun_list ) ,

    {reply, L, #state{fun_was="map_doc"}}
.%handle_call map_doc




%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%%@doc
%%@private
%%@end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



%% end $Id: erlview.erl,v 1.36 2009/02/22 16:12:18 mmcdanie Exp mmcdanie $

