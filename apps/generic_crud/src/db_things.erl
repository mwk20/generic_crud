-module(db_things).

-export([rest_list_things/0,
         rest_create_thing/1,
         rest_read_thing/1,
         rest_update_thing/2,
         rest_delete_thing/1]).

thingA() ->
    [{name, "feepstick"},
     {address, "1/2/3"},
     {count, 6},
     {other, <<"thingA">>}].

thingB() ->
    [{name, "piffle"},
     {address, "4/5/6"},
     {count, 15},
     {other, <<"thingB">>}].

rest_list_things() ->
    [thingA(), thingB()].

rest_create_thing(_Props) ->
    thingA().

rest_read_thing("a") ->
    thingA();
rest_read_thing("b") ->
    thingB();
rest_read_thing(_) ->
    undefined.

rest_update_thing(_Id, _Props) ->
    thingA().

rest_delete_thing(_Id) ->
    ok.
