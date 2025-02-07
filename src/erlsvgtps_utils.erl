-module(erlsvgtps_utils).

-export([
    lookup/2,
    lookup/3,
    lookup_float/2,
    lookup_float/3,
    bin2number/2,
    local_name/1
]).

lookup(Key, List) ->
    lookup(Key, List, null).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} when Result =/= null ->
            Result;
        _ ->
            Default
    end.

lookup_float(Key, List) ->
    lookup_float(Key, List, null).

lookup_float(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, V}  ->
            case catch binary_to_float(V) of
                {'EXIT',_} ->
                    case catch binary_to_integer(V) of
                        {'EXIT',_} ->
                            Default;
                        IntVal ->
                            float(IntVal)
                    end;
                FloatValue ->
                    FloatValue
            end;
        _ ->
            Default
    end.


bin2number(null, Default) ->
    Default;
bin2number(V, Default) ->
    case catch binary_to_float(V) of
        N1 when is_float(N1) ->
            N1;
        _ ->
            case catch binary_to_integer(V) of
                N2 when is_integer(N2) ->
                    N2;
                _ ->
                    Default
            end
    end.

local_name(FullName) ->
case binary:split(FullName, <<":">>, [global]) of
    [_Namespace, LocalName] ->
        LocalName;
    [LocalName] ->
        LocalName
end.
