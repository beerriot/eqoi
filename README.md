eqoi: A simple Erlang implentation of the "Quite Okay Image" format
=====

This is a simple implementation of the
[QOI](https://github.com/phoboslab/qoi) image compression format. It
has been lightly tested to ensure compatibility with [phoboslab/qoi @
a27f8ed](https://github.com/phoboslab/qoi/commit/a27f8ed4593ffad272b70030bf1ef3010d0803e1).

There is some scaffolding for making this an Erlang Application here
(`src/{eqoi.app.src, eqoi_app.erl, eqoi_sup.erl`). Ignore it for
now. Everything is in `src/eqoi.erl`.

Build
-----

    $ rebar3 compile

Play
----

    $ erl -pa `rebar3 path`

Load a PNG using wxErlang:

```erlang
wxImage:new().
WI = wxImage:new().
WI = wxImage:loadFile(WI, "my-cool-image.png").
```

Write the loaded image in QOI format:

```erlang
eqoi:write(3, % RGB only, no alpha
           wxImage:getData(WI),
           {wxImage:getWidth(WI), wxImage:getHeight(WI)},
           "my-cool-image.qoi").
```

Read an image in QOI format:

```erlang
{ok, Props} = eqoi:read("my-even-cooler-image.qoi").
```

Write the image as a PNG using wxErlang (only works if the image has 3
channels, without alpha):

```erlang
wx:new(). %% only necessary if you haven't already done this
WI2 = wxImage:new().
wxImage:setData(WI2,
                proplists:get_value(pixels, Props),
                proplists:get_value(width, Props),
wxImage:saveFile(WI2, "my-even-cooler-image.png").
```
