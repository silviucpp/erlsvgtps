-module(erlsvgtps).

-include_lib("erlxml/include/erlxml.hrl").
-include("erlsvgtps.hrl").

-export([
    from_file/1,
    from_buffer/1,

    metadata/1,
    title/1,
    version/1,
    base_profile/1,
    size/1,
    width/1,
    height/1,
    is_square/1,
    issues/1
]).

-record(state, {
    issues = [],
    title = null,
    version = null,
    base_profile = null,
    size = 0,
    width = null,
    height = null,
    is_square = null
}).

from_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            from_buffer(Binary);
        Error ->
            Error
    end.

from_buffer(Content) ->
    case erlxml:parse(Content) of
        {ok, #xmlel{name = <<"svg">>} = Svg} ->
            analyze_svg(Svg, #state{size = byte_size(Content)});
        _ ->
            {error, <<"Invalid SVG">>}
    end.

metadata(#state{title = Title, version = Version, base_profile = BaseProfile, size = Size, width = Width, height = Height, is_square = IsSquare}) -> [
    {<<"title">>, Title},
    {<<"version">>, Version},
    {<<"base_profile">>, BaseProfile},
    {<<"size">>, Size},
    {<<"width">>, Width},
    {<<"height">>, Height},
    {<<"is_square">>, IsSquare}
].

title(#state{title = V}) ->
    V.

version(#state{version = V}) ->
    V.

base_profile(#state{base_profile = V}) ->
    V.

size(#state{size = V}) ->
    V.

width(#state{width = V}) ->
    V.

height(#state{height = V}) ->
    V.

is_square(#state{is_square = V}) ->
    V.

issues(#state{issues = V}) ->
    V.

% internals

analyze_svg(#xmlel{attrs = SvgAttributes} = SvgElm, #state{size = Size} = State) ->
    Issues1 = check_size(Size, sets:new()),
    {Title, Issues2} = check_title(SvgElm, Issues1),
    {Version, BaseProfile, Issues3} = check_version_and_baseprofile(SvgAttributes, Issues2),
    Issues4 = check_svg_descendant_issues(SvgElm, Issues3),
    {Width, Height, IsSquare, Issues5} = check_square(SvgElm, Issues4),
    {ok, State#state{
        title = Title,
        version = Version,
        base_profile = BaseProfile,
        width = Width,
        height = Height,
        is_square = IsSquare,
        issues = sets:to_list(Issues5)
    }}.

check_size(Size, Issues) ->
    case Size > 32 * 1024 of
        true ->
            sets:add_element(<<"Logo is larger than 32KB">>, Issues);
        _ ->
            Issues
    end.

check_title(SvgEl, Issues) ->
    Title = case erlxml_utils:subel(SvgEl, <<"title">>) of
        #xmlel{children = [#xmlcdata{content = T}]} ->
            T;
        _ ->
            null
    end,
    case Title of
        null ->
            {null,sets:add_element(<<"Missing <title> element">>, Issues)};
        _ ->
            case SvgEl#xmlel.children of
                [#xmlel{name = <<"title">>}|_] ->
                    {Title, Issues};
                _->
                    {Title, sets:add_element(<<"The <title> element is not the first child of the <svg> element">>, Issues)}
          end
    end.

check_version_and_baseprofile(SvgAttributes, Issues) ->
    InfoVersion = erlsvgtps_utils:lookup(<<"version">>, SvgAttributes),
    InfoBaseProfile = erlsvgtps_utils:lookup(<<"baseProfile">>, SvgAttributes),
    case {InfoVersion, InfoBaseProfile} of
        {<<"1.2">>, <<"tiny-ps">>} ->
            {InfoVersion, InfoBaseProfile, Issues};
        _ ->
            {InfoVersion, InfoBaseProfile, sets:add_element(<<"Incorrect version or baseProfile attributes">>, Issues)}
    end.

check_square(SvgElm, Issues) ->
    {IsSquare, Width, Height} = is_svg_square(SvgElm),
    case IsSquare of
        false ->
            {Width, Height, IsSquare, sets:add_element(<<"SVG is not square">>, Issues)};
        _ ->
            {Width, Height, IsSquare, Issues}
    end.

check_svg_descendant_issues(#xmlel{children = Children} = Element, Issues) ->
    identify_children_issues(Children, check_element(Element, Issues));
check_svg_descendant_issues(#xmlcdata{}, Issues) ->
    Issues.

check_element(#xmlel{name = Name0, attrs = Attributes}, Issues) ->
    LocalName = erlsvgtps_utils:local_name(Name0),

    Issues1 = case lists:member(LocalName, ?ALLOWED_ELEMENTS_NAME) of
        true ->
            Issues;
        false ->
            sets:add_element(<<"Element <", LocalName/binary, "> is not allowed">>, Issues)
    end,
    case maps:get(LocalName, ?ALLOWED_ATTRIBUTES, null) of
        null ->
            Issues1;
        AllowedAttrs ->
            check_attributes(LocalName, Attributes, AllowedAttrs, Issues1)
    end.

check_attributes(LocalName, [{AttrName, _} | Rest], AllowedAttrs, Issues) ->
    case (string:prefix(AttrName, <<"xmlns">>) =/= nomatch orelse lists:member(AttrName, AllowedAttrs)) of
        true ->
            check_attributes(LocalName, Rest, AllowedAttrs, Issues);
        _ ->
            check_attributes(LocalName, Rest, AllowedAttrs, sets:add_element(<<"Element <", LocalName/binary, "> has a disallowed attribute: ", AttrName/binary>>, Issues))
    end;
check_attributes(_, [], _, Issues) ->
    Issues.

identify_children_issues([Child | Rest], Issues) ->
    identify_children_issues(Rest, check_svg_descendant_issues(Child, Issues));
identify_children_issues([], Issues) ->
    Issues.

is_svg_square(Element) ->
    case calculate_svg_aspect_ratio(Element) of
        {ok, Width, Height, AspectRatio} ->
            {abs(AspectRatio - 1.0) < 1.0e-9, Width, Height};
        _ ->
            {false, null, null}
    end.

calculate_svg_aspect_ratio(#xmlel{attrs = Attributes}) ->
    case erlsvgtps_utils:lookup(<<"viewBox">>, Attributes) of
        null ->
            calculate_width_height_aspect_ratio(Attributes);
        ViewBox ->
            case parse_viewbox(ViewBox) of
                {ok, _MinX, _MinY, VbWidth, VbHeight} when VbWidth > 0, VbHeight > 0 ->
                    {ok, VbWidth, VbHeight, VbWidth / VbHeight};
                _ ->
                    calculate_width_height_aspect_ratio(Attributes)
            end
    end.

calculate_width_height_aspect_ratio(Attributes) ->
    Width = erlsvgtps_utils:lookup_float(<<"width">>, Attributes, 0),
    Height = erlsvgtps_utils:lookup_float(<<"height">>, Attributes, 0),

    case {Width > 0, Height > 0} of
        {true, true} ->
            {ok, Width, Height, Width / Height};
        _ ->
            null
    end.

parse_viewbox(ViewBox) ->
    case [erlang:abs(erlsvgtps_utils:bin2number(X, 0.0)) || X <- binary:split(ViewBox, <<" ">>, [global])] of
        [MinX, MinY, VbWidth, VbHeight] ->
            {ok, MinX, MinY, VbWidth, VbHeight};
        _ ->
            null
    end.
