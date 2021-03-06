-module(eqoi_tools).

-export([
         compare/2,
         compare_full/2,
         collect_stats/1
        ]).

%% Compare two QOI encodings. Stop at first differing chunk, and
%% return the byte offset and remaining bytes in each stream.
compare(Stream1, Stream2) ->
    compare(Stream1, Stream2, 0).

compare(<<>>, <<>>, _) ->
    same;
compare(<<>>, Stream2, Offset) ->
    {Offset, <<>>, Stream2};
compare(Stream1, <<>>, Offset) ->
    {Offset, Stream1, <<>>};
compare(<<Same, Stream1/binary>>, <<Same, Stream2/binary>>, Offset) ->
    compare(Stream1, Stream2, Offset+1);
compare(Stream1, Stream2, Offset) ->
    {Offset, Stream1, Stream2}.

%% Attempt to compare the full stream at once, by accumulating single
%% chunk differences and continuing afterward. Return value is a list
%% of skipped chunks, and any tail that had more than one chunk
%% differing at its start.
%%
%% Each element in the list is of the format:
%%
%% { {ByteOffsetInStream1, ChunkAtOffsetInStream1}
%%   {ByteOffsetInStream2, ChunkAtOffsetInStream2} }
%%
%% With the tail element containg the rest of the stream instead of
%% just one chunk.
compare_full(Stream1, Stream2) ->
    compare_full(Stream1, Stream2, 0, 0, []).

compare_full(Stream1, Stream2, Offset1, Offset2, Diffs) ->
    case compare(Stream1, Stream2) of
        same ->
            Diffs;
        {Offset, RestStream1, RestStream2} ->
            case {consume_chunk(RestStream1),
                  consume_chunk(RestStream2)} of
                {{ok, Chunk1, <<Same,ContinueStream1/binary>>},
                 {ok, Chunk2, <<Same,ContinueStream2/binary>>}} ->
                    %% one chunk difference - continue
                    compare_full(ContinueStream1,
                                 ContinueStream2,
                                 Offset1+Offset+size(Chunk1),
                                 Offset2+Offset+size(Chunk2),
                                 [{{Offset1+Offset, Chunk1},
                                   {Offset2+Offset, Chunk2}}|Diffs]);
                _ ->
                    %% any other difference - stop
                    [{{Offset1+Offset, RestStream1},
                      {Offset2+Offset, RestStream2}}
                     |Diffs]
            end
    end.

%% Trying not to completely reimplement eqoi:decode_next_chunk
%% here. We just want the chunk, without any interpretation.
consume_chunk(<<N:2, _:6, _/binary>>=Stream) when N >= 0, N =< 2 ->
    <<Chunk, Rest/binary>> = Stream,
    {ok, Chunk, Rest};
consume_chunk(<<N:8, _/binary>>=Stream) when N == 254; N == 255 ->
    case N of
        254 ->
            <<Chunk:4/binary, Rest/binary>> = Stream;
        255 ->
            <<Chunk:5/binary, Rest/binary>> = Stream
    end,
    {ok, Chunk, Rest};
consume_chunk(<<Chunk, Rest/binary>>) ->
    {ok, Chunk, Rest}.

collect_stats(Stream) ->
    EmptyStats = [
                  {reference, 0, []},
                  {run, 0, []},
                  {mod_small, 0, []},
                  {mod_medium, 0, []},
                  {substitute_no_alpha, 0, []},
                  {substitute_alpha, 0, []}
                 ],
    collect_stats(Stream, EmptyStats).

collect_stats(<<0:2, _:6, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(reference, Stats));
collect_stats(<<1:2, _:6, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(mod_small, Stats));
collect_stats(<<2:2, _:14, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(mod_medium, Stats));
collect_stats(<<254:8, _:3/binary, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(substitute_no_alpha, Stats));
collect_stats(<<255:8, _:13, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(substitute_alpha, Stats));
collect_stats(<<3:2, _:6, Rest/binary>>, Stats) ->
    collect_stats(Rest, increment_stat(run, Stats));
collect_stats(<<>>, Stats) ->
    Stats.

increment_stat(Type, Stats) ->
    {value, {Type, Count, Props}, Rest} = lists:keytake(Type, 1, Stats),
    [{Type, Count+1, Props}|Rest].
