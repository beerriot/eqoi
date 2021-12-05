-module(eqoi).

-export([
         encode/2,
         decode/2,
         read/1,
         write/2,
         write/4,
         verify/2
        ]).

%% RGB or RGBA
-type pixel() :: <<_:24>> | <<_:32>>.
-type channels() :: 3..4.

%% 64-element tuple
-type index() :: {pixel() | undef}.
-define(INDEX_SIZE, 64).

%% The hash of a pixel
-type hash() :: 0..?INDEX_SIZE.

%% Encoder/decoder state
-record(eqoi_state,
        {
         %% Most recently encoded/decoded pixel value
         previous :: pixel(),

         %% Number of pixels matching `previous` since the first time
         %% that value was seen (not including the first time)
         run :: integer(),

         %% Pixel value index.
         index :: index()
        }).

%% QOI format final file bytes
-define(FILE_TAIL, <<0,0,0,0>>).

%%% SETUP

%% Create the starting state for the encoder or decoder.
-spec state_initial(pixel()) -> #eqoi_state{}.
state_initial(InitPixel) ->
    #eqoi_state{
       previous = InitPixel,
       run = 0,
       index = index_initial(InitPixel)
      }.

%% Create an opaque, black pixel (the default "previous" pixel for the
%% encoder/decoder start state), with the correct number of channels.
-spec pixel_initial(channels()) -> pixel().
pixel_initial(3) ->
    <<0, 0, 0>>;
pixel_initial(4) ->
    <<0, 0, 0, 255>>.

%% Add an opaque alpha to an RGB pixel. Convenience for pattern
%% matching and diff computation.
-spec full_pixel(pixel()) -> <<_:32>>.
full_pixel(RGBA = <<_:32>>) -> RGBA;
full_pixel(<<RGB:24>>) -> <<RGB:24, 255>>.

%%% INDEX MAINTENANCE

%% Create an index that has only mapped the given initial pixel.
-spec index_initial(pixel()) -> index().
index_initial(InitPixel) ->
    index_add(InitPixel,
              list_to_tuple(lists:duplicate(?INDEX_SIZE, undef))).

%% Lookup a pixel in the index, by hash value.
-spec index_find(integer(), index()) -> pixel() | undef.
index_find(Hash, Index) ->
    element(Hash+1, Index).

%% Map a pixel in the index. This will replace any pixel mapped at the
%% same hash.
%%
%% Use this instead of index_update if you don't care whether the
%% pixel was already mapped (i.e. in decoding).
-spec index_add(pixel(), index()) -> index().
index_add(Pixel, Index) ->
    setelement(pixel_hash(Pixel)+1, Index, Pixel).

%% Check if a pixel is already mapped in the index, and map it if it
%% isn't. This will replace any pixel mapped at the same hash.
%%
%% If the pixel is already mapped, `{match, Hash}` is returned (to cue
%% the encoder to produce a chunk referencing it). If the pixel is not
%% already mapped, `{nomatch, UpdatedIndex}` is returned with the
%% mapping made (to cue the encoder to produce a chunk describing the
%% pixel another way, while saving the value for potential later
%% reference).
-spec index_update(pixel(), index()) ->
          {match, integer()} |
          {nomatch, index()}.
index_update(Pixel, Index) ->
    Hash = pixel_hash(Pixel),
    case index_find(Hash, Index) of
        Pixel ->
            {match, Hash};
        _ ->
            {nomatch, setelement(Hash+1, Index, Pixel)}
    end.

%% Compute the hash of a pixel.
-spec pixel_hash(pixel()) -> hash().
pixel_hash(<<R/integer, G/integer, B/integer, A/integer>>) ->
    (R bxor G bxor B bxor A) rem ?INDEX_SIZE;
pixel_hash(<<R/integer, G/integer, B/integer>>) ->
    %% bnot would flip bits 256+, producing a negative number
    (R bxor G bxor B bxor 255) rem ?INDEX_SIZE.

%%% ENCODING

%% Encode the Image, treating the data as pixels with Channels number
%% of bytes. 3-byte (RGB) and 4-byte (RGB + Alpha) are supported.
-spec encode(channels(), binary()) -> binary().
encode(Channels, Image) ->
    encode_image(Channels, Image, state_initial(pixel_initial(Channels)), []).

%% Internal helper to drive the encoder by feeding it a pixel at a
%% time. This function is recursive, accumulating chunks in Acc until
%% Image is exhausted.
-spec encode_image(channels(), binary(), #eqoi_state{}, iodata()) -> binary().
encode_image(Channels, Image, State, Acc)->
    case Image of
        <<Pixel:Channels/binary, Rest/binary>> ->
            {NewData, NewState} = encode_pixel(Pixel, State),
            encode_image(Channels, Rest, NewState,
                         case NewData of
                             [] -> Acc;
                             _ -> [NewData | Acc]
                         end);
        <<>> ->
            iolist_to_binary(
              lists:reverse(
                maybe_add_run(State#eqoi_state.run, Acc)))
    end.

%% Apply a pixel to the encoder state. Zero, one, or two chunks may be
%% produced, and the encoder state may be updated. If a chunk is
%% produced, the encoder state's run length will be reset to 0.
%%
%% Zero chunks will be produced if the pixel has the same component
%% values as the previous pixel applied to the encoder state AND the
%% run length has NOT exceeded the maximum encodable run length.
%%
%% Two chunks will be produced if the pixel does not have the same
%% component values as the previous pixel AND the run length is
%% non-zero.
%%
%% One chunk will be produced in the cases not covered above (run
%% length exceeded, or run length zero).
-spec encode_pixel(pixel(), #eqoi_state{}) -> {iodata(), #eqoi_state{}}.
encode_pixel(Pixel, State=#eqoi_state{previous=Pixel, run=Run}) ->
    %% previous pixel matches this pixel
    case Run < 8223 of
        true ->
            %% no new chunk to write; just lengthen the run
            {[], State#eqoi_state{run = 1 + Run}};
        false ->
            %% max run size; write a run chunk and reset the counter
            {encode_run(Run+1), State#eqoi_state{run=0}}
    end;
encode_pixel(Pixel, State=#eqoi_state{run=Run, index=Index}) ->
    %% not a match for previous pixel
    case index_update(Pixel, Index) of
        {match, Hash} ->
            %% reference a pixel value already in the index
            OutBin = <<0:2, Hash:6>>,
            NewIndex = Index;
        {nomatch, NewIndex} ->
            %% describe the new pixel value
            case component_diffs(Pixel, State#eqoi_state.previous) of
                {R, G, B, A} when R >= -2, R =< 1,
                                  G >= -2, G =< 1,
                                  B >= -2, B =< 1,
                                  A == 0 ->
                    %% small modification
                    OutBin = <<2:2, (R+2):2, (G+2):2, (B+2):2>>;
                {R, G, B, A} when R >= -16, R =< 15,
                                  G >= -8, G =< 7,
                                  B >= -8, B =< 7,
                                  A == 0 ->
                    %% medium modification
                    OutBin = <<6:3, (R+16):5, (G+8):4, (B+8):4>>;
                {R, G, B, A} when R >= -16, R =< 15,
                                  G >= -16, G =< 15,
                                  B >= -16, B =< 15,
                                  A >= -16, A =< 15 ->
                    %% large modification
                    OutBin = <<14:4, (R+16):5, (G+16):5, (B+16):5, (A+16):5>>;
                {R, G, B, A} ->
                    %% component substitution
                    <<Pr, Pg, Pb, Pa>> = full_pixel(Pixel),
                    OutBin = [<<15:4,
                                (case R of 0 -> 0; _ -> 1 end):1,
                                (case G of 0 -> 0; _ -> 1 end):1,
                                (case B of 0 -> 0; _ -> 1 end):1,
                                (case A of 0 -> 0; _ -> 1 end):1>>,
                              case R of 0 -> <<>>; _ -> Pr end,
                              case G of 0 -> <<>>; _ -> Pg end,
                              case B of 0 -> <<>>; _ -> Pb end,
                              case A of 0 -> <<>>; _ -> Pa end]
            end
    end,
    {maybe_add_run(Run, OutBin),
     State#eqoi_state{previous=Pixel, run=0, index=NewIndex}}.


%% If the state's run length counter is non-zero, add a run-length
%% chunk before any other chunks produced.
-spec maybe_add_run(integer(), iodata()) -> iodata().
maybe_add_run(0, IoData) ->
    IoData;
maybe_add_run(Length, IoData) ->
    [encode_run(Length) | IoData].

%% Encode a run-length chunk.
-spec encode_run(integer) -> iodata().
encode_run(Length) when Length =< 32 ->
    <<2:3, (Length-1):5>>;
encode_run(Length) ->
    <<3:3, (Length-33):13>>.

%%% DECODING

%% Decode a chunk byte stream into a pixel byte stream, where each
%% pixel is Channels bytes long.
-spec decode(channels(), binary()) ->
          {ok, binary()} | {error, proplists:proplist()}.
decode(Channels, Chunks) ->
    %% This is a neat trick. Since all of the encodings are based on
    %% modifiying the previous pixel, if we start with a pixel having
    %% the correct number of channels, then all pixel modifications
    %% create pixels with the correct number of channels.
    decode_loop(Chunks, state_initial(pixel_initial(Channels)), [], 0).

%% Decode one chunk at a time, accumulating the decoded pixels in Acc.
%%
%% The final argument, Offset, is only used for debugging. It keeps
%% track of how many bytes have been processed from the Chunks binary,
%% so that information can be included in any potential error return.
-spec decode_loop(binary(), #eqoi_state{}, iodata(), integer()) ->
          {ok, binary()} | {error, proplists:proplist()}.
decode_loop(<<>>, _, Acc, _) ->
    %% accept a chunk stream with no file tail, for easier testing
    {ok, iolist_to_binary(lists:reverse(Acc))};
decode_loop(?FILE_TAIL, _, Acc, _) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
decode_loop(Chunks, State, Acc, Offset) ->
    case decode_next_chunk(Chunks, State) of
        {Pixels, RestChunks, NewState} ->
            decode_loop(RestChunks, NewState, [Pixels|Acc],
                        Offset+(size(Chunks)-size(RestChunks)));
        {error, Reason} ->
            {error, [{reason, Reason},
                     {decoder_state, State},
                     {chunk_bytes_processed, Offset},
                     {chunk_bytes_remaining, Chunks},
                     {decoded_pixels, iolist_to_binary(lists:reverse(Acc))}]}
    end.

%% Decode the next chunk of the Chunks byte stream.
%%
%% This is a different style than the encoder, because while we knew
%% how many bytes were in a pixel without looking at its value, we
%% don't know how many are in a chunk until we examine the first few
%% bits. This could be rewritten to take one byte at a time, and track
%% decoding state until a full chunk is read, but the pattern matching
%% works out well the way it's written here.
-spec decode_next_chunk(binary(), #eqoi_state{}) ->
          {iodata(), binary(), #eqoi_state{}} | {error, term()}.
decode_next_chunk(<<0:2, Hash:6, Rest/binary>>,
                  State=#eqoi_state{index=Index}) ->
    %% indexed pixel
    case index_find(Hash, Index) of
        Pixel when is_binary(Pixel) ->
            {Pixel, Rest, State#eqoi_state{previous=Pixel}};
        _ ->
            {error, {bad_pixel_index, Index}}
    end;
decode_next_chunk(<<2:3, Length:5, Rest/binary>>,
                  State=#eqoi_state{previous=Pixel}) ->
    %% short run
    {lists:duplicate(Length + 1, Pixel), Rest, State};
decode_next_chunk(<<3:3, Length:13, Rest/binary>>,
                  State=#eqoi_state{previous=Pixel}) ->
    %% long run
    {lists:duplicate(Length + 33, Pixel), Rest, State};
decode_next_chunk(<<2:2, Rd:2, Gd:2, Bd:2,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, index=Index}) ->
    %% small modification
    NewPixel = mod_pixel(Pixel, Rd-2, Gd-2, Bd-2, 0),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        index=index_add(NewPixel, Index)}};
decode_next_chunk(<<6:3, Rd:5, Gd:4, Bd:4,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, index=Index}) ->
    %% medium modification
    NewPixel = mod_pixel(Pixel, Rd-16, Gd-8, Bd-8, 0),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        index=index_add(NewPixel, Index)}};
decode_next_chunk(<<14:4, Rd:5, Gd:5, Bd:5, Ad:5,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, index=Index}) ->
    %% large modification
    NewPixel = mod_pixel(Pixel, Rd-16, Gd-16, Bd-16, Ad-16),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        index=index_add(NewPixel, Index)}};
decode_next_chunk(<<15:4, Rp:1, Gp:1, Bp:1, Ap:1, ModRest/binary>>,
                  State=#eqoi_state{previous=Pixel, index=Index}) ->
    %% component substitution
    {NewPixel, Rest} = sub_pixel(Pixel, {Rp, Gp, Bp, Ap}, ModRest),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        index=index_add(NewPixel, Index)}}.

%% PIXEL VALUE MANIPULATION

%% Compute the component-wise difference of two pixels.
-spec component_diffs(pixel(), pixel()) ->
          {integer(), integer(), integer(), integer()}.
component_diffs(<<R, G, B, A>>, <<Pr, Pg, Pb, Pa>>) ->
    {wrap_diff(R, Pr), wrap_diff(G, Pg), wrap_diff(B, Pb), wrap_diff(A, Pa)};
component_diffs(<<R, G, B>>, <<Pr, Pg, Pb>>) ->
    {wrap_diff(R, Pr), wrap_diff(G, Pg), wrap_diff(B, Pb), 0}.

%% Compute the difference (X-Y) in two pixel components. This uses the
%% wrap-around math described by the QOI spec. That is the difference
%% between 0 and 255 is either 1 or -1, depending on which way you're
%% wrapping.
%%
%% The encoder isn't going to use a diff unless it's in the range
%% -16..15, so returning -128 instead of 128 (or 56 instead of -200,
%% etc.) doesn't matter.
-spec wrap_diff(integer(), integer()) -> integer().
wrap_diff(X, Y) ->
    case X - Y of
        D when D > 15 ->
            X - (256 + Y);
        D when D < -16 ->
            (256 + X) - Y;
        D ->
            D
    end.

%% Compute the sum of two pixel components, wrapping the value around
%% 0<->255. (That is, 255+1=0, 1-2=255, etc.)
%%
%% Yes, the way the ranges in the spec are written are arbitrary
%% (either X or Y could be the diff, and the other the pixel), but it
%% describes the function bounds correctly.
-spec wrap_sum(-16..15, 0..255) -> 0..255.
wrap_sum(X, Y) ->
    case X + Y of
        D when D > 255 ->
            D rem 256;
        D when D < 0 ->
            D + 256;
        D ->
            D
    end.

%% Apply differences to pixel components.
-spec mod_pixel(pixel(), integer(), integer(), integer(), integer()) ->
          pixel().
mod_pixel(<<Ro:8, Go:8, Bo:8, Ao:8>>, Rd, Gd, Bd, Ad) ->
    <<(wrap_sum(Rd, Ro)):8,
      (wrap_sum(Gd, Go)):8,
      (wrap_sum(Bd, Bo)):8,
      (wrap_sum(Ad, Ao)):8>>;
mod_pixel(<<Ro:8, Go:8, Bo:8>>, Rd, Gd, Bd, 0) ->
    %% Alpha diff will always be zero for 3-Channel images. And,
    %% importantly, the decoding process depends on this function
    %% producing pixels with the same number of channels as previous
    %% pixels.
    <<(wrap_sum(Rd, Ro)):8,
      (wrap_sum(Gd, Go)):8,
      (wrap_sum(Bd, Bo)):8>>.

%% Substitute pixel components. Each element in the second-argument
%% tuple maps to the same-position component of the first-argument
%% pixel. For each 1 in the tuple, read the next byte from Data, and
%% use that byte for that component of the pixel. Return the modified
%% pixel value, and the unconsumed data.
%%
%% e.g. sub_pixel(<<1, 2, 3, 4>>, {0, 1, 1, 0}, [5, 6, 7, 8])
%%         -> {<<1, 5, 6, 4>>, [7, 8]}.
-spec sub_pixel(pixel(), {integer()}, binary()) -> {pixel(), binary()}.
sub_pixel(<<Ro:8, Go:8, Bo:8, Ao:8>>, {Rm, Gm, Bm, Am}, Data) ->
    {R, Rrest} = maybe_sub(Rm, Ro, Data),
    {G, Grest} = maybe_sub(Gm, Go, Rrest),
    {B, Brest} = maybe_sub(Bm, Bo, Grest),
    {A, Arest} = maybe_sub(Am, Ao, Brest),
    {<<R:8, G:8, B:8, A:8>>, Arest};
sub_pixel(<<Ro:8, Go:8, Bo:8>>, {Rm, Gm, Bm, 0}, Data) ->
    %% Alpha will never be subsituted in 3-Channel images. And,
    %% importantly, the decoding process depends on this function
    %% producing pixels with the same number of channels as previous
    %% pixels.
    {R, Rrest} = maybe_sub(Rm, Ro, Data),
    {G, Grest} = maybe_sub(Gm, Go, Rrest),
    {B, Brest} = maybe_sub(Bm, Bo, Grest),
    {<<R:8, G:8, B:8>>, Brest}.

%% Helper function for sub_pixel/3. If the first argument is 0, return
%% Original and Data unchanged. If 1, return the first byte of Data
%% instead of Original, and the Data remaining after that byte.
-spec maybe_sub(0|1, integer(), binary()) -> {integer(), binary()}.
maybe_sub(0, Original, Data) ->
    {Original, Data};
maybe_sub(1, _, <<New:8, Rest/binary>>) ->
    {New, Rest}.

%% READING AND WRITING FILES

%% Read a QOI-format file. The proplist in the return value contains
%% elements for `width`, `height`, and `channels`, as well as `pixels`
%% which will be a binary containing Channels bytes per pixel in the
%% image.
-spec read(string()) -> {ok|error, proplists:proplist()}.
read(Filename) ->
    {ok, <<"qoif",
           Width:32/unsigned, Height:32/unsigned,
           Channels:8/unsigned,
           _ColorSpace:8/unsigned, %% ignored right now
           Chunks/binary>>} = file:read_file(Filename),
    Props = [{width, Width}, {height, Height}, {channels, Channels}],
    case decode(Channels, Chunks) of
        {ok, Pixels} ->
            {ok, [{pixels, Pixels} | Props]};
        {error, Proplist} ->
            {error, Props ++ Proplist}
    end.

%% Given a proplist with pixels, width, height, and channels elements,
%% in the format specified by a successful return from read/1, write a
%% QOI-encoded file to Filename.
-spec write(proplists:proplist(), string()) -> ok | {error | term()}.
write(Props, Filename) ->
    {width, Width} = proplists:lookup(width, Props),
    {height, Height} = proplists:lookup(height, Props),
    {channels, Channels} = proplists:lookup(channels, Props),
    {pixels, Pixels} = proplists:lookup(pixels, Props),
    write(Channels, Pixels, {Width, Height}, Filename).

%% Encode Pixels using QOI, and write an image file at Filename. Size
%% is a 2-tuple of {Width, Height}. Pixels should contain Channels
%% bytes per pixel <<Red, Green, Blue [, Alpha]>> (i.e. Channels *
%% Width * Height).
-spec write(channels(), binary(), {integer(), integer()}, string()) ->
          ok | {error, term()}.
write(Channels, Pixels, Size, Filename) ->
    Chunks = encode(Channels, Pixels),
    file:write_file(Filename,
                    [qoif_header(Size, Channels),
                     Chunks,
                     ?FILE_TAIL]).

%% Create a QOI-format file header.
-spec qoif_header({integer(), integer()}, channels()) -> binary().
qoif_header({Width, Height}, Channels) ->
    <<"qoif",
      Width:32/unsigned, Height:32/unsigned,
      Channels:8/unsigned,
      0:8/unsigned %% Color space - unused right now
      >>.

%% TESTING

%% Verify that decode(Channels, encode(Channels, Pixels)) reproduces
%% Pixels exactly. The two values in the successful `{ok, _, _}`
%% return are the encoder and decoder states, respectively. They
%% should be equivalent, but that is not part of this verification. If
%% an error is returned, the proplist contains information about what
%% didn't match (reason), where in the image it was (pixels_consumed),
%% and the state of the encoder and decoder.
-spec verify(channels(), binary()) ->
          {ok, #eqoi_state{}, #eqoi_state{}} |
          {error, proplists:proplist()}.
verify(Channels, Pixels) ->
    EncodeState = state_initial(pixel_initial(Channels)),
    DecodeState = EncodeState,
    verify(Channels, EncodeState, DecodeState, Pixels, [], 0).

%% Consume Pixels (fourth argument) one at a time, passing them to the
%% encoder, and accumulating them in Acc (fifth argument). When the
%% encoder returns a new chunk (or chunks), pass them to the decoder
%% and verify that the bytes produced match the bytes
%% accumulated. Essential verify that Pixels ==
%% decode(encode(Pixels)), but step-by-step, stopping with hopefully
%% helpful information when the decoding doesn't match the original.
-spec verify(channels(),
             #eqoi_state{}, #eqoi_state{},
             binary(), iodata(),
             integer()) ->
          {ok, #eqoi_state{}, #eqoi_state{}} |
          {error, proplists:proplist()}.
verify(Channels, ES, DS, <<>>, Acc, Consumed) ->
    case Acc of
        [] ->
            case ES#eqoi_state.run of
                0 ->
                    {ok, ES, DS};
                N ->
                    {error, [{reason,
                              "End run non-zero, but no accumulated pixels"},
                             {end_run_length, N},
                             {pixels_consumed, Consumed},
                             {encoder_state, ES},
                             {decoder_state, DS}]}
            end;
        _ ->
            case ES#eqoi_state.run of
                0 ->
                    {error, [{reason, "Accumulated pixels remaining"},
                             {pixels_remaining, length(Acc)},
                             {pixels_consumed, Consumed},
                             {encoder_state, ES},
                             {decoder_state, DS}]};
                N ->
                    Chunks = encode_run(N),
                    NewES = ES#eqoi_state{run=0},
                    Expect = list_to_binary(lists:reverse(Acc)),
                    case verify_match(Channels, Chunks, Expect, DS) of
                        {ok, NewDS} ->
                            {ok, NewES, NewDS};
                        {error, Reason, NewDS} ->
                            {error, [{reason, Reason},
                                     {pixels_consumed, Consumed},
                                     {encoder_state, NewES},
                                     {decoder_state, NewDS}]}
                    end
            end
    end;
verify(Channels, ES, DS, Pixels, Acc, Consumed) ->
    <<Next:Channels/binary, Rest/binary>> = Pixels,
    case encode_pixel(Next, ES) of
        {[], NewES} ->
            verify(Channels, NewES, DS, Rest, [Next | Acc], Consumed + 1);
        {EncodedList, NewES} ->
            Chunks = list_to_binary([EncodedList]),
            Expect = list_to_binary(lists:reverse([Next | Acc])),
            case verify_match(Channels, Chunks, Expect, DS) of
                {ok, NewDS} ->
                    verify(Channels, NewES, NewDS, Rest, [], Consumed + 1);
                {error, Reason, NewDS} ->
                    {error, [{reason, Reason},
                             {chunks, Chunks},
                             {expect, Expect},
                             {pixels_consumed, Consumed+1},
                             {encoder_state, NewES},
                             {decoder_state, NewDS}]}
            end
    end.

%% Veryify that Chunks (second argument) decode to the bytes of Expect
%% (third argument). The state in either return value is the updated
%% decoder state.
-spec verify_match(channels(), binary(), binary(), #eqoi_state{}) ->
          {ok, #eqoi_state{}} | {error, term(), #eqoi_state{}}.
verify_match(_, <<>>, <<>>, DS) ->
    {ok, DS};
verify_match(_, <<>>, Expect, DS) ->
    {error, {leftover_expect, Expect}, DS};
verify_match(Channels, Chunks, Expect, DS) ->
    case decode_next_chunk(Chunks, DS) of
        {PixelList, Rest, NewDS} ->
            case match_pixels(Channels, Expect, iolist_to_binary(PixelList)) of
                {ok, Remaining} ->
                    verify_match(Channels, Rest, Remaining, NewDS);
                {error, Reason} ->
                    {error, Reason, NewDS}
            end;
        {error, Reason} ->
            {error, Reason, DS}
    end.

%% Verify that Pixels (third argument) is an exact prefix of Expect
%% (second argument). An `{ok, Remaining}` return means it is. if
%% Remaining is the empty binary, Expect and Pixels were the same
%% size. The binaries returned by `{error, {mismatch, _, _}}` might be
%% either one pixel each, or unconsumed tails of each input.
-spec match_pixels(channels(), binary(), binary()) ->
          {ok, binary()} | {error, {mismatch, binary(), binary()}}.
match_pixels(_, Remaining, <<>>) ->
    %% Encoding can produce multiple chunks at once. Remaining bytes
    %% here likely mean that verify_match has another chunk to decode.
    {ok, Remaining};
match_pixels(Channels, Expect, Pixels) ->
    case {Expect, Pixels} of
        {<<E:Channels/binary, RestExpect/binary>>,
         <<P:Channels/binary, RestPixels/binary>>} ->
            case E == P of
                true ->
                    match_pixels(Channels, RestExpect, RestPixels);
                false ->
                    {error, {mismatch, E, P}}
            end;
        _ ->
            %% Pixels isn't empty, but either it has too few bytes for
            %% a full pixel, or Expect is empty.
            {error, {mismatch, Expect, Pixels}}
    end.
