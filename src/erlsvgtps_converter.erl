-module(erlsvgtps_converter).

-include_lib("erlxml/include/erlxml.hrl").
-include("erlsvgtps.hrl").

-export([
    from_buffer/1,
    from_file/1
]).

from_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            sanitize_svg(Binary, null);
        Error ->
            Error
    end.

from_buffer(Content) ->
    sanitize_svg(Content, null).

% internals

sanitize_svg(Content, NewTitleBin) ->
    case erlxml:parse(Content) of
        {ok, #xmlel{name = <<"svg">>} = SvgEl} ->
            SvgEl1 = sanitize_title_element(SvgEl, NewTitleBin),
            {_, SvgEl2} = remove_namedview(SvgEl1, false),
            SvgEl3 = SvgEl2#xmlel{attrs = sanitize_svg_attributes(SvgEl2#xmlel.attrs)},
            SvgEl4 = sanitize_svg_descendants(SvgEl3),
            {ok, erlxml:to_binary(SvgEl4)};
        _ ->
            {error, <<"Invalid SVG">>}
    end.

remove_namedview(#xmlel{name = <<"namedview">>}, _Found) ->
    {true, undefined};
remove_namedview(#xmlel{children = Children} = El, Found) when not Found ->
    case remove_namedview_from_children(Children, false) of
        {true, NewChildren} ->
            {true, El#xmlel{children = NewChildren}};
        {false, _} ->
            {false, El}
    end;
remove_namedview(Other, Found) ->
    {Found, Other}.

remove_namedview_from_children([H | T], Found) ->
    case remove_namedview(H, Found) of
        {true, undefined} ->
            {true, T};
        {true, NewH} ->
            {true, [NewH | T]};
        {false, NewH} ->
            {FNext, NewT} = remove_namedview_from_children(T, false),
            {FNext, [NewH | NewT]}
    end;
remove_namedview_from_children([], Found) ->
    {Found, []}.

sanitize_title_element(#xmlel{children = Children} = SvgEl, NewTitle) ->
    case Children of
        [#xmlel{name = <<"title">>} = H |RestChildren] ->
            % title already first element. check if we want to change the title or not.
            case NewTitle of
                null ->
                    SvgEl;
                _ ->
                    SvgEl#xmlel{children = [H#xmlel{children = #xmlcdata{content = NewTitle}}|RestChildren]}
            end;
        _ ->
            % title is not the first element. check if we have any valid title element and move
            % it first or add a new one

            case take_element(SvgEl, <<"title">>) of
                {ok, #xmlel{children = C0} = TitleEl0, NewSvgEl1} ->
                    ExistingTitle = case C0 of
                        [#xmlcdata{content = ExistingTitleBin}] ->
                            ExistingTitleBin;
                        _ ->
                            <<"Company Name">>
                    end,
                    TitleEl = TitleEl0#xmlel{children = [#xmlcdata{content = apply_new_title(ExistingTitle, NewTitle)}]},
                    NewSvgEl1#xmlel{children = [TitleEl|NewSvgEl1#xmlel.children]};
                _ ->
                    SvgEl#xmlel{children = [#xmlel{name = <<"title">>, children = [#xmlcdata{content = apply_new_title(<<"Company Name">>, NewTitle)}]}|Children]}
            end
    end.

sanitize_svg_attributes(Attr) ->
    Attr1 = erlsvgtps_utils:add_or_replace(<<"version">>, <<"1.2">>, Attr),
    Attr2 = erlsvgtps_utils:add_or_replace(<<"baseProfile">>, <<"tiny-ps">>, Attr1),
    Attr3 = erlsvgtps_utils:delete(<<"y">>, erlsvgtps_utils:delete(<<"x">>, Attr2)),

    case erlsvgtps_utils:lookup(<<"style">>, Attr3) of
        null ->
            remove_disallowed_attributes(Attr3, ?SVG_ALLOWED_ATTRIBUTES, []);
        Style ->
            StyleAttributes = lists:filter(fun({K, _}) -> lists:member(K, ?SVG_ALLOWED_ATTRIBUTES) end, parse_style(Style)),
            remove_disallowed_attributes(Attr3, ?SVG_ALLOWED_ATTRIBUTES, StyleAttributes)
    end.

remove_disallowed_attributes([{AttrName, _V}= H|T], AllowedAttr, Acc) ->
    case (string:prefix(AttrName, <<"xmlns">>) =/= nomatch orelse lists:member(AttrName, AllowedAttr)) of
        true ->
            remove_disallowed_attributes(T, AllowedAttr, [H|Acc]);
        _ ->
            remove_disallowed_attributes(T, AllowedAttr, Acc)
    end;
remove_disallowed_attributes([], _AllowedAttr, Acc) ->
    Acc.

sanitize_svg_descendants(#xmlel{name = Name, attrs = Attr, children = Children}= RootEl) ->
    AllowedAttributes = maps:get(erlsvgtps_utils:local_name(Name), ?ALLOWED_ATTRIBUTES, null),
    NewAttr = case erlsvgtps_utils:lookup(<<"style">>, Attr) of
        null ->
            case AllowedAttributes of
                null ->
                    Attr;
                _ ->
                    remove_disallowed_attributes(Attr, AllowedAttributes, [])
            end;
        Style ->
            ParseStyleAttributes = lists:filter(fun({K, _}) -> lists:member(K, ?SVG_ALLOWED_ATTRIBUTES) end, parse_style(Style)),
            case AllowedAttributes of
                null ->
                    ParseStyleAttributes++Attr;
                _ ->
                    remove_disallowed_attributes(Attr, AllowedAttributes, ParseStyleAttributes)
            end
    end,

    % filtermap children

    NewChildren = lists:filtermap(fun(El) ->
        case El of
            #xmlel{name = ChName} ->
                ChLocalName = erlsvgtps_utils:local_name(ChName),
                case ChLocalName == <<"image">> orelse lists:member(ChLocalName, ?ALLOWED_ELEMENTS_NAME) of
                    true ->
                        {true, sanitize_svg_descendants(El)};
                    _ ->
                        false
                end;
            _ ->
                {true, El}
        end
    end, Children),

    RootEl#xmlel{attrs = NewAttr, children = NewChildren};
sanitize_svg_descendants(RootEl) ->
    RootEl.

parse_style(Bin) ->
    Styles = binary:split(Bin, <<";">>, [global]),
    lists:foldl(fun(S, Acc) ->
        case binary:split(S, <<":">>, [global]) of
            [K, V] ->
                [{string:trim(K), string:trim(V)}|Acc];
            _ ->
                Acc
        end
    end, [], Styles).

take_element(#xmlel{children = Children} = El, Name) ->
    case lists:keytake(Name, 2, Children) of
        {value, #xmlel{} = X, NewChildren} ->
            {ok, X, El#xmlel{children = NewChildren}};
        _ ->
            null
    end.

apply_new_title(ExistingTitle, null) ->
    ExistingTitle;
apply_new_title(_ExistingTitle, NewTitle) ->
    NewTitle.
