%% ============================================================================
%%
%% HTTP VERBS
%%

-define(HTTP_HEAD, 			'HEAD').
-define(HTTP_GET,			'GET').
-define(HTTP_POST,			'POST').
-define(HTTP_PUT,			'PUT').
-define(HTTP_DELETE,		'DELETE').
-define(HTTP_TRACE,			'TRACE').
-define(HTTP_OPTIONS,		'OPTIONS').
-define(HTTP_CONNECT,		'CONNECT').
-define(HTTP_PATCH,			'PATCH').

%% ============================================================================
%%
%%  WEBDAV VERBS
%%

-define(WEBDAV_PROPFIND,	'PROPFIND').
-define(WEBDAV_PROPPATCH,	'PROPPATCH').
-define(WEBDAV_MKCOL,		'MKCOL').
-define(WEBDAV_COPY,		'COPY').
-define(WEBDAV_MOVE,		'MOVE').
-define(WEBDAV_LOCK,		'LOCK').
-define(WEBDAV_UNLOCK,		'UNLOCK').

%% ============================================================================
%%
%%  ENCRYPTION KEYS
%%
%% AES----------
-define(Key_AES,	<<"abcdefghabcdefgh">>). %%  Encryption key
-define(IV_AES,	<<"12345678abcdefgh">>).     %%  Initialization Vector

%% DES----------
-define(Key_DES,	<<"abcdefgh">>).         %% Encryption Key
-define(IV_DES,	<<"12345678">>).             %% Initialization Vector



%% ============================================================================
%%
%% HTTP SERVER RESPONSE CODES
%%

-define(HTTP_100,"100 Continue").
-define(HTTP_101,"101 Switching Protocols"). 
-define(HTTP_102,"102 Processing"). 

-define(HTTP_200,"200 OK"). 
-define(HTTP_201,"201 Created"). 
-define(HTTP_202,"202 Accepted"). 
-define(HTTP_203,"203 Non-Authoritative Information"). 
-define(HTTP_204,"204 No Content"). 
-define(HTTP_205,"205 Reset Content"). 
-define(HTTP_206,"206 Partial Content"). 
-define(HTTP_207,"207 Multi-Status"). 

-define(HTTP_300,"300 Multiple Choices"). 
-define(HTTP_301,"301 Moved Permanently"). 
-define(HTTP_302,"302 Found"). 
-define(HTTP_303,"303 See Other"). 
-define(HTTP_304,"304 Not Modified"). 
-define(HTTP_305,"305 Use Proxy"). 
-define(HTTP_306,"306 Switch Proxy"). 
-define(HTTP_307,"307 Temporary Redirect"). 

-define(HTTP_400,"400 Bad Request"). 
-define(HTTP_401,"401 Unauthorized"). 
-define(HTTP_402,"402 Payment Required"). 
-define(HTTP_403,"403 Forbidden"). 
-define(HTTP_404,"404 Not Found"). 
-define(HTTP_405,"405 Method Not Allowed"). 
-define(HTTP_406,"406 Not Acceptable"). 
-define(HTTP_407,"407 Proxy Authentication Required"). 
-define(HTTP_408,"408 Request Timeout"). 
-define(HTTP_409,"409 Conflict"). 
-define(HTTP_410,"410 Gone"). 
-define(HTTP_411,"411 Length Required"). 
-define(HTTP_412,"412 Precondition Failed"). 
-define(HTTP_413,"413 Request Entity Too Large"). 
-define(HTTP_414,"414 Request-URI Too Long"). 
-define(HTTP_415,"415 Unsupported Media Type"). 
-define(HTTP_416,"416 Requested Range Not Satisfiable"). 
-define(HTTP_417,"417 Expectation Failed"). 
-define(HTTP_418,"418 I'm a teapot"). 
-define(HTTP_422,"422 Unprocessable Entity"). 
-define(HTTP_423,"423 Locked"). 
-define(HTTP_424,"424 Failed Dependency"). 
-define(HTTP_425,"425 Unordered Collection"). 
-define(HTTP_426,"426 Upgrade Required"). 
-define(HTTP_449,"449 Retry With"). 
-define(HTTP_450,"450 Blocked by Windows Parental Controls"). 

-define(HTTP_500,"500 Internal Server Error"). 
-define(HTTP_501,"501 Not Implemented"). 
-define(HTTP_502,"502 Bad Gateway"). 
-define(HTTP_503,"503 Service Unavailable"). 
-define(HTTP_504,"504 Gateway Timeout"). 
-define(HTTP_505,"505 HTTP Version Not Supported"). 
-define(HTTP_506,"506 Variant Also Negotiates"). 
-define(HTTP_507,"507 Insufficient Storage"). 
-define(HTTP_509,"509 Bandwidth Limit Exceeded"). 
-define(HTTP_510,"510 Not Extended").


%% ============================================================================
%%
%% HTTP REQUEST
%%

-record(http_request, {
					   
					   %% PID of process that sends the request
					   senderPID,
				  
					   %% HTTP Verb
					   verb,
				  
					   %% The original Path
					   path = "",
					   
					   %% Parsed URI
					   uri = "",
				  
					   %% Parsed Arguments
					   arguments = [],
					   
					   %% HTTP Version
					   version = {1,1},
				  
					   %% List of Headers
					   headers = [],
					   
					   %% Body of the Request
					   body = <<>>
				 
				  }
	   ).


%% ============================================================================
%%
%% HTTP RESPONSE
%%

-record(http_response, {
						
						%% PID of process that sends the response
						senderPID,
						
						%% Server Response Code
						responseCode,
						
						%% List of Headers
						headers 		= [],
						
						%% Body
						body 		= <<>>
				   
				   }).

