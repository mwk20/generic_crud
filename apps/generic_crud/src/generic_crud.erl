-module(generic_crud).

%% ------------------------------------------------------------------
%% webmachine resource function exports
%% ------------------------------------------------------------------

-export([init/1,
         resource_exists/2,
         content_types_provided/2,
         generate_xml/2]).

%% ------------------------------------------------------------------
%% webmachine header import
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
%% webmachine resource context
%% ------------------------------------------------------------------

-record(context, {resource, is_collection, db_result}).

%% ------------------------------------------------------------------
%% webmachine resource function definitions
%% ------------------------------------------------------------------

%% initialise request context
init([Resource, CollectionSingular]) ->
    %% is this a list/search of the collection?
    IsCollection = case CollectionSingular of
                       collection -> true;
                       singular -> false
                   end,
    %% construct context
    Ctx = #context{resource=Resource,
                   is_collection=IsCollection},
    {ok, Ctx}.

%% lookup resource in database -- collections always exist
resource_exists(ReqData, Ctx=#context{is_collection=true}) ->
    DbResult = db_things:rest_list_things(),
    {true, ReqData, Ctx#context{db_result=DbResult}};
%% lookup resource in database -- singular items might not exist
resource_exists(ReqData, Ctx) ->
    Id = wrq:path_info(id, ReqData),
    DbResult = db_things:rest_read_thing(Id),
    case DbResult of
        undefined -> {false, ReqData, Ctx};
        _ -> {true, ReqData, Ctx#context{db_result=DbResult}}
    end.

content_types_provided(ReqData, Ctx) ->
    {[{"application/xml", generate_xml}], ReqData, Ctx}.

%% ------------------------------------------------------------------
%% internal function definitions
%% ------------------------------------------------------------------

%% return list of spaces to provide requested indentation
%% @todo this is just a temporary hack for xml generation
indent(Indent) when Indent >= 0 ->
    [32 || _I <- lists:seq(1, Indent)].

%% generate body for collections of things
%% @todo this is just a temporary hack for xml generation
generate_xml_body(ReqData, Ctx, true, Things, 0) when is_list(Things) ->
    [<<"<things>\n">>,
     [generate_xml_body(ReqData, Ctx, false, Thing, 2) || Thing <- Things],
     <<"</things>\n">>];

%% generate body for individual things
%% @todo this is just a temporary hack for xml generation
generate_xml_body(_ReqData, _Ctx, false, Thing, Indent) ->
    [indent(Indent), <<"<thing>\n">>,
     [begin
          Spaces = indent(Indent+2),
          TagStr = atom_to_list(Tag),
          ValueStr = if
                         is_integer(Value) -> io_lib:format("~b", [Value]);
                         is_list(Value) -> Value;
                         is_binary(Value) -> Value
                     end,
          [Spaces, "<", TagStr, ">", ValueStr, "</", TagStr, ">\n"]
      end || {Tag, Value} <- Thing],
     indent(Indent), <<"</thing>\n">>].

%% generate content
%% @todo this is just a temporary hack for xml generation
generate_xml(ReqData, Ctx) ->
    XmlDeclaration = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
    Body = generate_xml_body(ReqData, Ctx, Ctx#context.is_collection, Ctx#context.db_result, 0),
    {[XmlDeclaration, Body], ReqData, Ctx}.
