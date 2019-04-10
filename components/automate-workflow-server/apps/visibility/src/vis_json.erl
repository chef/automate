-module(vis_json).

-export([
         init_schemas/0,
         validate_and_return_json_or_400_with_proper_message/3
        ]).

-include_lib("chef_common/include/chef_common.hrl").

-spec init_schemas() -> ok.
init_schemas() ->
    chef_json:init_schemas(?MODULE).

-spec validate_and_return_json_or_400_with_proper_message(cowboy_req:req(),
                                                          any(),
                                                          atom() | json()) -> {halt, cowboy_req:req(), any()}
                                                                                  | {json(), cowboy_req:req(), any()}.
validate_and_return_json_or_400_with_proper_message(Req, State, JsonSchema) ->
    case deliv_web_utils:parse_json_req(Req, JsonSchema) of
        {{error,  {[{data_invalid, _, _, _, _}], _Ejson}}, Req1} ->
            %% TODO: once we are self-hosting the API docs, let's link directly to the
            %% documentation for each endpoint :)
            Message = <<"The request JSON you sent was valid JSON but was invalid for this request. Please see the docs for more info.">>,
            deliv_web_utils:error_response(400, bad_request, Message, Req1, State);
        {{error, _}, Req1} ->
            Message = <<"The server could not parse the request JSON you sent.">>,
            deliv_web_utils:error_response(400, bad_request, Message, Req1, State);
        {ReqJson, Req1} ->
            {ReqJson, Req1, State}
    end.
