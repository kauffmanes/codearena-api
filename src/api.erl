%% @author kauff
%% @doc @todo Add description to api.


-module(api).
-include("http.hrl").

-define(TIMEOUT, 5000).
-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start/1]).

start() -> start(8080).
start(Port) ->
		inets:start(),
		
		{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
		spawn_link(fun() -> accept(LSocket) end),
		
		io:format("Listening on ~p~n", [Port]).

accept(LSocket) ->
	{ok, ClientSocket} = gen_tcp:accept(LSocket),
	Pid = spawn(fun() ->
			io:format("Connection accepted~n"),
			loop(ClientSocket) end),
	gen_tcp:controlling_process(ClientSocket, Pid),
	accept(LSocket).

loop(ClientSocket) ->
		
	Request_LINE = read_request_line(ClientSocket),
	
	%%read headers
  Request_HEADERS = read_headers(ClientSocket, Request_LINE),
	%io:format("Request headers: ~p~n", [Request_HEADERS]),
	
	%%read body
  Request_BODY = read_body(ClientSocket, Request_HEADERS),
	io:format("Request body: ~p~n", [Request_BODY]),
	
	URI = atom_to_list(Request_BODY#http_request.uri),
	
	%% make db requests
	db_req(ClientSocket,
				 comdetect(Request_BODY#http_request.verb, URI, Request_BODY#http_request.body)),
	
	loop(ClientSocket).
		
%%handling the responses including error
db_req(ClientSocket, {ok, {{_HTTP, _RN, _RT}, L, D}}) ->
		io:format("Data: ~p~n", [D]),
		io:format("Headers: ~p~n", [L]),
		NewHeaders = [{"access-control-allow-origin", "*"}|L],
		io:format("New Headers: ~p~n", [NewHeaders]),
		http_write_response(?HTTP_200, ClientSocket,
                       header_to_string(NewHeaders), [D]);

db_req(ClientSocket, {error, {connect_failed, _}})  ->
    http_write_response(?HTTP_404, ClientSocket, "\r\n", "connect_failed");

db_req(ClientSocket, {error, {send_failed, _}})  ->
    http_write_response(?HTTP_400, ClientSocket, "\r\n", "send_failed");

db_req(ClientSocket, {error, _})  ->
    http_write_response(?HTTP_400, ClientSocket, "\r\n", "").


%%---------------------------------------------------
%% RESTful URI's to the CouchDB Database
%%---------------------------------------------------

%%CREATE/UPDATE DATA
comdetect('PUT', "api/arena/"++ArenaId, Data) ->
		httpc:request(put, {"http://127.0.0.1:5984/arenas/"++ArenaId, [{"Access-Control-Allow-Origin", "*"}], [], Data}, [], []);

%%READ DATA
comdetect('GET', "api/arena", _) ->
		httpc:request(get, {"http://127.0.0.1:5984/arenas", [{"Access-Control-Allow-Origin", "*"}]}, [], []);
comdetect('GET', "api/arena/", _) ->
		httpc:request(get, {"http://127.0.0.1:5984/arenas", [{"Access-Control-Allow-Origin", "*"}]}, [], []);
comdetect('GET', "api/arena/"++ArenaId, _) ->
		httpc:request(get, {"http://127.0.0.1:5984/arenas/"++ArenaId, [{"Access-Control-Allow-Origin", "*"}]}, [], []);
		
%% DELETE (todo)
comdetect('DELETE', "api/arena/"++ArenaId, _) ->
		httpc:request(delete, {"http://127.0.0.1:5984/arenas/"++ArenaId, []}, [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================
read_request_line (ClientSocket)  ->
		
	%% set client socket to read once
	inet:setopts(ClientSocket, [{active,once}, {packet, http}]),
  
	%% process data
	receive
		
		%%deal with only http requests
		{http, ClientSocket, {http_request, Method, OPath, Version}}  ->
				
		    %% get path in usable format
		    Path = get_path(OPath),
		
		    %% Split path into URI & Arguments
				io:format("path: ~p~n", [Path]),
		    {URI, ArgumentString} = split_path(Path),
				io:format("uri: ~p~n", [URI]),
		  
				Arguments = create_argument_tuples(ArgumentString),
		  
				%%put the data we already have into a request record
		    Request = #http_request{
	          senderPID = self(),
	        
	          uri = uri_to_atom(URI),
	          arguments = Arguments,
	        
	          verb = Method,
	          path = Path,
	          version = Version
	        },
		 		Request;
	  
		_Else  ->
			
			%% ERROR -> KILL CONNECTION
			gen_tcp:close(ClientSocket),
			exit(normal)
	
	end.


%%---------------------
%% read_request_headers
%%---------------------
read_headers(ClientSocket, Request) ->
      
    %% set socket options to one more run
    inet:setopts(ClientSocket, [{active,once}]),
  
    %% wait for messages
  
    receive
      
        %% get header
        {http, ClientSocket, {http_header, _, HttpField, _, Value}} ->
            read_headers(ClientSocket, Request#http_request{ headers = [{HttpField, Value}| Request#http_request.headers]});
      
        %% Simple Error -> ignore
        {http, ClientSocket, {http_error, "\r\n"}} ->
            read_headers(ClientSocket,Request);
      
        %% Simple Error -> ignore
        {http, ClientSocket, {http_error, "\n"}} ->
            read_headers(ClientSocket,Request);
      
        %% no more headers -> go to process body
        {http, ClientSocket, http_eoh} ->
            Request;
      
        %% Serious Client Error -> close socket and terminate process
        _Error ->
            %% ERROR -> KILL CONNECTION
            gen_tcp:close(ClientSocket),
            exit(normal)
    end.

read_body(ClientSocket, Request) ->
  
    %% get content Length
    #http_request{headers = Headers} = Request,
  
    case get_header('Content-Length', Headers) of
      
        %% no value
        {'Content-Length', undefined} -> Request;
      
        %% a value
        {'Content-Length',Value} ->
          
            %%ContentLength = list_to_integer(atom_to_list(Value)),
            ContentLength = list_to_integer(Value),
          
            if
              
                ContentLength == 0 -> Request;
              
                ContentLength > 0 ->
                  
                    inet:setopts(ClientSocket, [{packet, raw}]),
                  
                    case gen_tcp:recv(ClientSocket, ContentLength, ?TIMEOUT) of
              
                {ok, Bin} ->
                    Request#http_request{body = Bin};

                _Other ->
                    gen_tcp:close(ClientSocket),
                    exit(normal)
          
                    end
            end      
    end.





%% ----------------------------------------------------------------------------------------
%%
%% GET_PATH/1
%%
%% RawPath: The unprocessed path from the original http request
%%
%% ->        The path string
get_path(RawPath) ->

    %% deal with the most common formats for the path
    case RawPath of
        {abs_path, Path} ->
            Path;
          
        {absoluteURI, http, _Host, _, Path} ->
            Path;
          
        _ELSE  ->
            ""
    end.



%% ----------------------------------------------------------------------------------------
%%
%% SPLIT_PATH/1
%%
%% Look for ? and split in before (URI) and after (argument string)
%%
%% Request: The parsed data of the request
%%
%% ->        close | keep_alive
split_path([47|Path]) ->
    split_path(Path,[]);

split_path(Path) ->
    split_path(Path,[]).

split_path([$?|Arguments], Path) ->
    {lists:reverse(Path), Arguments};

split_path([H|T], Path) ->
    split_path(T, [H|Path]);

split_path([], Path) ->
    {lists:reverse(Path), []}.

%% ----------------------------------------------------------------------------------------
%% uri_to_atom
%% ----------------------------------------------------------------------------------------
uri_to_atom([]) ->
    {error};

uri_to_atom([47 | _]) ->
  
    {error};

uri_to_atom(URI) ->
  
    list_to_atom(URI).


%% ----------------------------------------------------------------------------------------
%%
%% CREATE_ARGUMENT_TUPLES/1
%%
%% Request: The parsed data of the request
%%
%% ->        close | keep_alive

create_argument_tuples(Arguments) ->
  
    Tokens = string:tokens(Arguments, "&"),
    create_argument_tuples(Tokens,[]).

create_argument_tuples([],Tuples) -> Tuples;

create_argument_tuples([H|T],Tuples) ->   
  
    case string:tokens(H, "=") of
      
        [Argument, Value] ->
            create_argument_tuples(T,[{Argument, Value} | Tuples]);
      
        _Else ->
            create_argument_tuples(T,Tuples)
    end.

%% ----------------------------------------------------------------------------------------
%%
%% HTTP_WRITE_RESPONSE/4
%%
%% Code:
%% Client_Socket
%% Headers
%% Data:    The data blob that is sent to the client_socket/client
%%
%% ->        nothing

http_write_response(Code, Client_Socket, Headers, Data) ->
  
    %Response = "HTTP/1.1 200 OK\r\n" ++ Headers ++ "Content-Length: " ++ erlang:integer_to_list(Length ,10) ++ "\r\n\r\n" ++ Data,
    Response = "HTTP/1.1 " ++ Code++ "\r\n" ++ Headers ++  Data,
    gen_tcp:send(Client_Socket, Response).

get_header(Name,[]) ->
    {Name,undefined};

get_header(Name, [{Name, Value} | _]) ->
    {Name,Value};

get_header(Name, [_|List]) ->
    get_header(Name, List).

header_to_string(Headers) -> header_to_string(Headers, []).
header_to_string([],Headers) -> Headers++"\r\n";
header_to_string([{Name, Value}|Headers], String) -> header_to_string(Headers, String++Name++": "++Value++"\r\n").
