-module(logos_test).

-include_lib("eunit/include/eunit.hrl").

-define(LOGOS, [
    {<<"twitter">>, [
        {<<"logo">>, <<"twitter.svg">>},
        {<<"issues_excepted">>, [
            <<"Missing <title> element">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"SVG is not square">>,
            <<"Element <svg> has a disallowed attribute: style">>
        ]}
    ]},
    {<<"corrupt svg">>, [
        {<<"logo">>, <<"corruptsvg.svg">>},
        {<<"issues_excepted">>, [
            <<"Missing <title> element">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"Element <script> is not allowed">>,
            <<"SVG is not square">>
        ]}
    ]},
    {<<"waffle image">>, [
        {<<"logo">>, <<"waffle-icon.svg">>},
        {<<"issues_excepted">>, [
            <<"Logo is larger than 32KB">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"Element <image> is not allowed">>,
            <<"Element <stop> is not allowed">>,
            <<"Element <pattern> is not allowed">>,
            <<"Element <use> has a disallowed attribute: xlink:href">>,
            <<"Element <mask> is not allowed">>,
            <<"Element <g> has a disallowed attribute: mask">>,
            <<"SVG is not square">>
        ]}
    ]},
    {<<"microsoft edge">>, [
        {<<"logo">>, <<"microsoft-edge.svg">>},
        {<<"issues_excepted">>, [
            <<"Missing <title> element">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"Element <linearGradient> has a disallowed attribute: gradientTransform">>,
            <<"Element <stop> is not allowed">>,
            <<"Element <radialGradient> has a disallowed attribute: gradientTransform">>,
            <<"Element <path> has a disallowed attribute: opacity">>,
            <<"Element <path> has a disallowed attribute: enable-background">>
        ]}
    ]},
    {<<"php">>, [
        {<<"logo">>, <<"php-alt.svg">>},
        {<<"issues_excepted">>, [
            <<"The <title> element is not the first child of the <svg> element">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"SVG is not square">>
        ]}
    ]},
    {<<"svg">>, [
        {<<"logo">>, <<"svg.svg">>},
        {<<"issues_excepted">>, [
            <<"Logo is larger than 32KB">>,
            <<"Missing <title> element">>,
            <<"Incorrect version or baseProfile attributes">>,
            <<"Element <svg> has a disallowed attribute: id">>,
            <<"Element <svg> has a disallowed attribute: inkscape:version">>,
            <<"Element <svg> has a disallowed attribute: sodipodi:docname">>,
            <<"Element <svg> has a disallowed attribute: inkscape:export-filename">>,
            <<"Element <svg> has a disallowed attribute: inkscape:export-xdpi">>,
            <<"Element <svg> has a disallowed attribute: inkscape:export-ydpi">>,
            <<"Element <metadata> is not allowed">>,
            <<"Element <RDF> is not allowed">>,
            <<"Element <Work> is not allowed">>,
            <<"Element <format> is not allowed">>,
            <<"Element <type> is not allowed">>,
            <<"Element <perspective> is not allowed">>,
            <<"Element <namedview> is not allowed">>,
            <<"Element <path> has a disallowed attribute: style">>,
            <<"Element <path> has a disallowed attribute: sodipodi:nodetypes">>,
            <<"Element <path> has a disallowed attribute: sodipodi:type">>,
            <<"Element <path> has a disallowed attribute: sodipodi:cx">>,
            <<"Element <path> has a disallowed attribute: sodipodi:cy">>,
            <<"Element <path> has a disallowed attribute: sodipodi:rx">>,
            <<"Element <path> has a disallowed attribute: sodipodi:ry">>,
            <<"Element <text> has a disallowed attribute: style">>,
            <<"Element <tspan> is not allowed">>,
            <<"Element <text> has a disallowed attribute: sodipodi:linespacing">>
        ]}
    ]}
]).

logos_test() ->
    lists:foreach(fun({TestName, Args}) ->
        Image = erlsvgtps_utils:lookup(<<"logo">>, Args),
        ExpectedErrors = lists:sort(erlsvgtps_utils:lookup(<<"issues_excepted">>, Args)),

        {ok, State} = erlsvgtps_check:from_file(<<"test/assets/", Image/binary>>),
        Issues = lists:sort(erlsvgtps_check:issues(State)),
        ?assertEqual({TestName, ExpectedErrors}, {TestName, Issues})
    end, ?LOGOS).

converter_test() ->
    NotFixableIssues = [
        <<"Logo is larger than 32KB">>,
        <<"Element <image> is not allowed">>,
        <<"SVG is not square">>
    ],

    lists:foreach(fun({TestName, Args}) ->
        Image = erlsvgtps_utils:lookup(<<"logo">>, Args),
        SvgFile = <<"test/assets/", Image/binary>>,

        {ok, NewSvgContent} = erlsvgtps_converter:from_file(SvgFile),
        {ok, StateAfter} = erlsvgtps_check:from_buffer(NewSvgContent),
        IssuesAfter0 = erlsvgtps_check:issues(StateAfter),
        IssuesAfter = lists:foldl(fun(X, Acc) -> sets:del_element(X, Acc) end, sets:from_list(IssuesAfter0), NotFixableIssues),
        ?assertEqual({TestName, []}, {TestName, sets:to_list(IssuesAfter)})

    end, ?LOGOS).
