%% Simple "QOI - Quite Okay Image" format.
%%
%% Based on https://github.com/phoboslab/qoi.
%%
%% Terminology:
%%
%%  * Pixel: 3 or 4 8-bit bytes, stored as a `binary()`, either on its
%%           own or as part of a larger binary.
%%
%%  * Channels: Number of bytes per pixel. Three (Red, Green, Blue),
%%              and four (Red, Green, Blue, Alpha) are supported.
%%
%%  * Chunk: One QOI code. These may be from one to five bytes in
%%           length.
%%
%% The encode/2 function converts from a binary() of Pixels to a
%% binary() of Chunks. The decode/2 function converts from a binary()
%% of Chunks to a binary() of Pixels.
%%
%% The read/1 and write/2,4 functions will load or store files
%% containing QOI chunks, prefixed with an informational header,
%% on-disk.
%%
%% Chunk Encoding:
%%
%%   0 0 Reference:6
%%   0 1 R:2 G:2 B:2
%%   1 0 G:6         R:4 B:4
%%
%%   1 1 1 1 1 1 1 0 R:8 G:8 B:8
%%   1 1 1 1 1 1 1 1 R:8 G:8 B:8 A:8
%%
%%   1 1 Run:6

-module(eqoi).

-export([
         encode/2,
         decode/2,
         read/1,
         write/2,
         write/4,
         verify/2
        ]).

-compile({inline, [{index_add, 2},
                   {wrap_diff, 2},
                   {encode_run, 1}]}).

%% RGB or RGBA
-type pixel() :: <<_:24>> | <<_:32>>.
-type channels() :: 3..4.

%% The hash of a pixel
-define(INDEX_SIZE, 64).
-type hash() :: 0..(?INDEX_SIZE-1).

%% map from hash to pixel value
-type index() :: #{hash() := pixel()}.

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
-define(FILE_TAIL, <<0,0,0,0,0,0,0,1>>).

%%% SETUP

%% Create the starting state for the encoder or decoder.
-spec state_initial(pixel()) -> #eqoi_state{}.
state_initial(InitPixel) ->
    #eqoi_state{
       previous = InitPixel,
       run = 0,
       index = index_add(InitPixel, #{})
      }.

%% Create an opaque, black pixel (the default "previous" pixel for the
%% encoder/decoder start state), with the correct number of channels.
-spec pixel_initial(channels()) -> pixel().
pixel_initial(3) ->
    <<0, 0, 0>>;
pixel_initial(4) ->
    <<0, 0, 0, 255>>.

%%% INDEX MAINTENANCE

%% Map a pixel in the index. This will replace any pixel mapped at the
%% same hash.
-spec index_add(pixel(), index()) -> index().
index_add(Pixel, Index) ->
    Index#{pixel_hash(Pixel) => Pixel}.

%% Compute the hash of a pixel.
-spec pixel_hash(pixel()) -> hash().
pixel_hash(<<R/integer, G/integer, B/integer, A/integer>>) ->
    ((R * 3) + (G * 5) + (B * 7) + (A * 11)) rem ?INDEX_SIZE;
pixel_hash(<<R/integer, G/integer, B/integer>>) ->
    ((R * 3) + (G * 5) + (B * 7) + (255 * 11)) rem ?INDEX_SIZE.

%%% ENCODING

%% Encode the Image, treating the data as pixels with Channels number
%% of bytes. 3-byte (RGB) and 4-byte (RGB + Alpha) are supported.
-spec encode(channels(), binary()) -> binary().
encode(Channels, Image) ->
    {Encoded, _} = encode_loop(Channels, Image,
                               state_initial(pixel_initial(Channels)), <<>>),
    Encoded.

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
-define(ENCODE_LOOPI(Channels),
        encode_loop(Channels,
                    <<Pixel:Channels/binary, Rest/binary>>,
                    State=#eqoi_state{previous=Pixel, run=Run},
                    Acc) ->
               %% previous pixel matches this pixel
               case Run < 61 of
                   true ->
                       %% no new chunk to write; just lengthen the run
                       encode_loop(Channels, Rest,
                                   State#eqoi_state{run = 1 + Run}, Acc);
                   false ->
                       %% max run size; write a run chunk and reset
                       %% the counter
                       encode_loop(Channels, Rest,
                                   State#eqoi_state{run=0},
                                   <<Acc/binary, (encode_run(Run+1))/binary>>)
               end).
-define(ENCODE_LOOPM(Channels),
        encode_loop(Channels,
                    <<Pixel:Channels/binary, Rest/binary>>,
                    State=#eqoi_state{run=Run, index=Index},
                    Acc) ->
               %% not a match for previous pixel
               Hash = pixel_hash(Pixel),
               case Index of
                   #{Hash := Pixel} ->
                       %% reference a pixel value already in the index
                       NewAcc = <<(maybe_add_run(Run, Acc))/binary,
                                  0:2, Hash:6>>,
                       NewIndex = Index;
                   _ ->
                       %% describe the new pixel value
                       NewIndex = Index#{Hash => Pixel},
                       case component_diffs(Pixel, State#eqoi_state.previous) of
                           {R, G, B, 0} when R >= -2, R =< 1,
                                             G >= -2, G =< 1,
                                             B >= -2, B =< 1 ->
                               %% small modification
                               %% +2 = diffs are shifted up to be
                               %% encoded unsigned
                               NewAcc = <<(maybe_add_run(Run, Acc))/binary,
                                          1:2, (R+2):2, (G+2):2, (B+2):2>>;
                           {R, G, B, 0} when G >= -32, G =< 31,
                                             (R-G) >= -8, (R-G) =< 7,
                                             (B-G) >= -8, (B-G) =< 7 ->
                               %% medium modification
                               %% +32,+8 = diffs are shifted up to be
                               %% encoded unsigned
                               NewAcc = <<(maybe_add_run(Run, Acc))/binary,
                                          2:2, (G+32):6, (R-G+8):4, (B-G+8):4>>;
                           {_, _, _, 0} ->
                               %% component substitution, no alpha change
                               NewAcc = <<(maybe_add_run(Run, Acc))/binary,
                                          254, Pixel:3/binary>>;
                           _ when size(Pixel) == 4 ->
                               %% component substitution, alpha change

                               %% 'when' clause is not necessary - diff won't
                               %% show an alpha change if the pixel is only 3
                               %% bytes wide, but the guard is kept here to
                               %% ensure that

                               NewAcc = <<(maybe_add_run(Run, Acc))/binary,
                                          255, Pixel/binary>>
                       end
               end,
               encode_loop(Channels, Rest,
                           State#eqoi_state{previous=Pixel, run=0,
                                            index=NewIndex},
                           NewAcc)).

-spec encode_loop(channels(), binary(), #eqoi_state{}, binary()) ->
          {binary(), #eqoi_state{}}.
encode_loop(_, <<>>, State, Acc) ->
    {Acc, State};
?ENCODE_LOOPI(3);
?ENCODE_LOOPM(3);
?ENCODE_LOOPI(4);
?ENCODE_LOOPM(4).

%% If the state's run length counter is non-zero, add a run-length
%% chunk at the end of the accumulated binary.
-spec maybe_add_run(integer(), binary()) -> binary().
maybe_add_run(0, Acc) ->
    Acc;
maybe_add_run(Length, Acc) ->
    <<Acc/binary, (encode_run(Length))/binary>>.

%% Encode a run-length chunk.
-spec encode_run(integer) -> binary().
encode_run(Length) when Length =< 62 ->
    %% -1 = no need to encode a run of length 0, so encoded 0 = length 1
    <<3:2, (Length-1):6>>.

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
    case decode_loop(Chunks, state_initial(pixel_initial(Channels)),
                     <<>>, 0) of
        {ok, Pixels, _} ->
            {ok, Pixels};
        Error={error,_} ->
            Error
    end.

%% Decode one chunk at a time, accumulating the decoded pixels in Acc.
%%
%% The final argument, Offset, is only used for debugging. It keeps
%% track of how many bytes have been processed from the Chunks binary,
%% so that information can be included in any potential error return.
%%
%% This is a different style than the encoder, because while we knew
%% how many bytes were in a pixel without looking at its value, we
%% don't know how many are in a chunk until we examine the first few
%% bits. This could be rewritten to take one byte at a time, and track
%% decoding state until a full chunk is read, but the pattern matching
%% works out well the way it's written here.
-spec decode_loop(binary(), #eqoi_state{}, binary(), integer()) ->
          {ok, binary(), #eqoi_state{}} | {error, proplists:proplist()}.
decode_loop(<<>>, State, Acc, _) ->
    %% accept a chunk stream with no file tail, for easier testing
    {ok, Acc, State};
decode_loop(?FILE_TAIL, State, Acc, _) ->
    {ok, Acc, State};
decode_loop(<<0:2, Hash:6, Rest/binary>>,
            State=#eqoi_state{index=Index},
            Acc, Offset) ->
    %% indexed pixel
    case Index of
        #{Hash := Pixel} ->
            decode_loop(Rest, State#eqoi_state{previous=Pixel},
                        <<Acc/binary, Pixel/binary>>, Offset+1);
        _ ->
            {error, [{reason, {bad_pixel_index, Index}},
                     {decoder_state, State},
                     {chunk_bytes_processed, Offset},
                     {chunk_bytes_remaining, Rest},
                     {decoded_pixels, Acc}]}
    end;
decode_loop(<<1:2, Rd:2, Gd:2, Bd:2,Rest/binary>>,
            State=#eqoi_state{previous=Pixel, index=Index},
            Acc, Offset) ->
    %% small modification
    %% -2 = diffs are shifted up to be encoded unsigned
    NewPixel = mod_pixel(Pixel, Rd-2, Gd-2, Bd-2, 0),
    decode_loop(Rest,
                State#eqoi_state{previous=NewPixel,
                                 index=index_add(NewPixel, Index)},
                <<Acc/binary, NewPixel/binary>>, Offset+1);
decode_loop(<<2:2, Gd:6, Rd:4, Bd:4, Rest/binary>>,
            State=#eqoi_state{previous=Pixel, index=Index},
            Acc, Offset) ->
    %% medium modification
    %% -32 = green shift to unsigned encoding
    %% -40 = red and blue shift to unsigned encoding, plus green shift
    NewPixel = mod_pixel(Pixel, Gd+Rd-40, Gd-32, Gd+Bd-40, 0),
    decode_loop(Rest,
                State#eqoi_state{previous=NewPixel,
                                 index=index_add(NewPixel, Index)},
                <<Acc/binary, NewPixel/binary>>, Offset+2);
decode_loop(<<254, RGB:3/binary, Rest/binary>>,
            State=#eqoi_state{previous=Pixel, index=Index},
            Acc, Offset) ->
    case Pixel of
        <<_,_,_>> -> NewPixel = RGB;
        <<_,_,_,A>> -> NewPixel = <<RGB:3/binary,A>>
    end,
    decode_loop(Rest,
                State#eqoi_state{previous=NewPixel,
                                 index=index_add(NewPixel, Index)},
                <<Acc/binary, NewPixel/binary>>, Offset+4);
decode_loop(<<255, NewPixel:4/binary, Rest/binary>>,
            State=#eqoi_state{index=Index},
            Acc, Offset) ->
    decode_loop(Rest,
                State#eqoi_state{previous=NewPixel,
                                 index=index_add(NewPixel, Index)},
                <<Acc/binary, NewPixel/binary>>, Offset+5);
decode_loop(<<3:2, Length:6, Rest/binary>>,
            State=#eqoi_state{previous=Pixel},
            Acc, Offset) ->
    %% short run
    %% (see encode_run/1 for "+1" explanation)
    decode_loop(Rest, State,
                <<Acc/binary, (binary:copy(Pixel, Length+1))/binary>>,
                Offset+1).

%% PIXEL VALUE MANIPULATION

%% Compute the difference (X-Y) in two pixel components. This uses the
%% wrap-around math described by the QOI spec. That is the difference
%% between 0 and 255 is either 1 or -1, depending on which way you're
%% wrapping.
-spec wrap_diff(integer(), integer()) -> integer().
wrap_diff(X, Y) ->
    case X - Y of
        D when D > 127 ->
            D - 256;
        D when D < -127 ->
            D + 256;
        D ->
            D
    end.

%% Compute the component-wise difference of two pixels.
-spec component_diffs(pixel(), pixel()) ->
          {integer(), integer(), integer(), integer()}.
component_diffs(<<R, G, B, A>>, <<Pr, Pg, Pb, Pa>>) ->
    {wrap_diff(R, Pr), wrap_diff(G, Pg), wrap_diff(B, Pb), wrap_diff(A, Pa)};
component_diffs(<<R, G, B>>, <<Pr, Pg, Pb>>) ->
    {wrap_diff(R, Pr), wrap_diff(G, Pg), wrap_diff(B, Pb), 0}.

%% Apply differences to pixel components.
%%
%% We don't have to do any checking around <0 or >255, because putting
%% the integer in the binary is going to limit it to the lowest eight
%% bits, which does what we would have done with arithmetic.
-spec mod_pixel(pixel(), integer(), integer(), integer(), integer()) ->
          pixel().
mod_pixel(<<Ro, Go, Bo, Ao>>, Rd, Gd, Bd, Ad) ->
    <<(Rd+Ro), (Gd+Go), (Bd+Bo), (Ad+Ao)>>;
mod_pixel(<<Ro, Go, Bo>>, Rd, Gd, Bd, 0) ->
    %% Alpha diff will always be zero for 3-Channel images. And,
    %% importantly, the decoding process depends on this function
    %% producing pixels with the same number of channels as previous
    %% pixels.
    <<(Rd+Ro), (Gd+Go), (Bd+Bo)>>.

%% READING AND WRITING FILES

%% Read a QOI-format file. The proplist in the return value contains
%% elements for `width`, `height`, and `channels`, as well as `pixels`
%% which will be a binary containing Channels bytes per pixel in the
%% image.
-spec read(string()) -> {ok|error, proplists:proplist()}.
read(Filename) ->
    {ok, <<"qoif",
           Width:32/unsigned, Height:32/unsigned,
           Channels,
           _ColorSpace, %% ignored right now
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
      Channels,
      0 %% Color space - unused right now
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
    verify(Channels, EncodeState, DecodeState, Pixels, <<>>, 0).

%% Consume Pixels (fourth argument) one at a time, passing them to the
%% encoder, and accumulating them in Acc (fifth argument). When the
%% encoder returns a new chunk (or chunks), pass them to the decoder
%% and verify that the bytes produced match the bytes
%% accumulated. Essential verify that Pixels ==
%% decode(encode(Pixels)), but step-by-step, stopping with hopefully
%% helpful information when the decoding doesn't match the original.
-spec verify(channels(),
             #eqoi_state{}, #eqoi_state{},
             binary(), binary(),
             integer()) ->
          {ok, #eqoi_state{}, #eqoi_state{}} |
          {error, proplists:proplist()}.
verify(Channels, ES, DS, <<>>, Acc, Consumed) ->
    case Acc of
        <<>> ->
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
                             {pixels_remaining, size(Acc)},
                             {pixels_consumed, Consumed},
                             {encoder_state, ES},
                             {decoder_state, DS}]};
                N ->
                    Chunks = encode_run(N),
                    NewES = ES#eqoi_state{run=0},
                    case verify_match(Channels, Chunks, Acc, DS) of
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
    case encode_loop(Channels, Pixels, ES, <<>>) of
        {<<>>, NewES} ->
            verify(Channels, NewES, DS, <<>>, Pixels,
                   Consumed + 1);
        {Chunks, NewES} ->
            Expect = Pixels,
            case verify_match(Channels, Chunks, Expect, DS) of
                {ok, NewDS} ->
                    verify(Channels, NewES, NewDS, <<>>, <<>>, Consumed + 1);
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
verify_match(Channels, Chunks, Expect, DS) ->
    case decode_loop(Chunks, DS, <<>>, 0) of
        {ok, PixelList, NewDS} ->
            case match_pixels(Channels, Expect, PixelList) of
                {ok, <<>>} ->
                    {ok, NewDS};
                {ok, Remaining} ->
                    {error, {leftover_expect, Remaining}, NewDS};
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
