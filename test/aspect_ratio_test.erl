-module(aspect_ratio_test).

-include_lib("eunit/include/eunit.hrl").

-define(RECTANGULAR, [
    {<<"rectangular viewBox">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 200 100\" width=\"100\" height=\"100\"></svg>">>},
    {<<"rectangular viewBox and wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 200 100\" width=\"200\" height=\"100\"></svg>">>},
    {<<"rectangular viewBox but not wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 200 100\"></svg>">>},
    {<<"rectangular wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200\" height=\"100\"></svg>">>}
]).

-define(SQUARE, [
    {<<"square viewBox">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\"></svg>">>},
    {<<"square viewBox and wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\" width=\"100\" height=\"100\"></svg>">>},
    {<<"square viewBox but not wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\" width=\"200\" height=\"100\"></svg>">>},
    {<<"square wh">>, <<"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\"></svg>">>}
]).

rectangular_test() ->
    lists:foreach(fun({TestName, Svg}) ->
        {ok, State} = erlsvgtps_check:from_buffer(Svg),
        ?assertEqual({TestName, false}, {TestName, erlsvgtps_check:is_square(State)})
    end, ?RECTANGULAR).

square_test() ->
    lists:foreach(fun({TestName, Svg}) ->
        {ok, State} = erlsvgtps_check:from_buffer(Svg),
        ?assertEqual({TestName, true}, {TestName, erlsvgtps_check:is_square(State)})
    end, ?SQUARE).

malformed_view_box_test() ->
    Svg = <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
        <svg viewBox=\"0 0 400 300\" version=\"1.1\"
             xmlns=\"http://www.w3.org/2000/svg\"
             xmlns:xlink=\"http://www.w3.org/1999/xlink\"
             preserveAspectRatio=\"xMidYMid\">
        </svg>">>,
    ?assertMatch({ok, _}, erlsvgtps_check:from_buffer(Svg)).


