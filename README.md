# Imperial
Library for rendering [mustache](http://http://mustache.github.io) templates in Erlang. The library support most functionality except partials. It also supports dotted keys.
The library started out as a fork of [mustache.erl](https://github.com/Textalk/mustache.erl) but ended up being totally rewritten.

## API
### Types
``` erlang
context()    :: #{binary() => term()}.
renderer()   :: fun((context()) -> binary()).
template()   :: binary() | string().
delimiters() :: {binary(), binary()}.
```

### Render
``` erlang
render(template() | renderer(), context())               -> binary().
render(template() | renderer(), delimiters(), context()) -> binary().
```

### Compile
``` erlang
compile(template())               -> renderer().
compile(template(), delimiters()) -> renderer().
```
