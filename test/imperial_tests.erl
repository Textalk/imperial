%% The MIT License
%%
%% Copyright (c) 2015 Emil Falk <emph@riseup.net>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
-module(imperial_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    test_helper(<<"Hello {{name}}!">>, <<"Hello world!">>, #{<<"name">> => <<"world">>}).

integer_values_too_test() ->
    test_helper(
      <<"Hello {{name}}~nYou have just won ${{value}}!">>,
      <<"Hello Chris~nYou have just won $10000!">>,
      #{<<"name">> => <<"Chris">>, <<"value">> => 10000}
    ).

specials_test() ->
    test_helper(
        <<"\'Hello\n\"{{name}}\"~nYou \"have\" ju\0st\\ won\b\r\"${{value}}!\"\t">>,
        <<"\'Hello\n\"Chris\"~nYou \"have\" ju\0st\\ won\b\r\"$10000!\"\t">>,
        #{<<"name">> => <<"Chris">>, <<"value">> => 10000}
    ).

binary_test() ->
    test_helper(
      <<"Hello {{name}}~nYou have just won ${{value}}!">>,
      <<"Hello Chris~nYou have just won $10000!">>,
      #{<<"name">> => <<"Chris">>, <<"value">> => 10000}
    ).

simple_dot_test() ->
    test_helper(
      <<"Hello {{name.first}} {{name.last}}!">>,
      <<"Hello Emil Falk!">>,
      #{<<"name">> => #{<<"first">> => <<"Emil">>, <<"last">> => <<"Falk">>}}
    ).

simple_section_dot_test() ->
    test_helper(<<"{{#test}}{{.}}{{/test}}">>, <<"123">>, #{<<"test">> => lists:seq(1,3)}).

complex_section_dot_test() ->
    test_helper(
        <<"{{#test1.test2}}{{.}}{{/test1.test2}}">>,
        <<"123">>,
        #{<<"test1">> => #{<<"test2">> => lists:seq(1, 3)}}
    ).

object_section_test() ->
    test_helper(<<"{{#test}}{{name}}{{/test}}">>, <<"Emil">>, #{<<"test">> => #{<<"name">> => <<"Emil">>}}).

fun_test() ->
    test_helper(
      <<"{{#test1}}xxx{{/test1}}">>,
      <<"yyy">>,
      #{<<"test1">> => fun (Text) -> re:replace(Text, <<"xxx">>, <<"{{test2}}">>, [{return, binary}]) end,
        <<"test2">> => <<"yyy">>}
    ).

set_delimiter_test() ->
    test_helper(
        <<"{{=[[ ]]=}}[[{test1}]] [[={{ }}=]]{{{test2}}}">>,
        <<"TEST1 TEST2">>,
        #{<<"test1">> => <<"TEST1">>, <<"test2">> => <<"TEST2">>}
    ).

set_delimiter_boolean_section_test() ->
    test_helper(
        <<"{{=[[ ]]=}}[[#test1.test2]]TEST[[/test1.test2]] {{::test}}">>,
        <<"TEST {{::test}}">>,
        #{<<"test1">> => #{<<"test2">> => true}}
    ).

deep_section_test() ->
    test_helper(
        <<"{{=[[ ]]=}}[[#a]][[#b]][[{test}]][[/b]][[/a]] [[#c]][[#d]][[{test}]][[/d]][[/c]]">>,
        <<"apa bepa">>,
        #{
            <<"a">> => #{<<"b">> => #{<<"test">> => <<"apa">>}},
            <<"c">> => #{<<"d">> => #{<<"test">> => <<"bepa">>}}
         }
     ).

tag_type_variable_empty_test() ->
    test_helper(<<"{{name}}">>, <<>>, #{}).

tag_type_variable_string_test() ->
    test_helper(<<"{{name}}">>, <<"NAME">>, #{<<"name">> => <<"NAME">>}).

tag_type_variable_integer_test() ->
    test_helper(<<"{{name}}">>, <<"1">>, #{<<"name">> => 1}).

tag_type_variable_boolean_test() ->
    test_helper(<<"{{name}}">>, <<"true">>, #{<<"name">> => true}).

tag_type_varibale_escaped_test() ->
    test_helper(<<"{{name}}">>, <<"&gt;&amp;do&lt;it&gt;">>, #{<<"name">> => <<">&do<it>">>}).

tag_type_variabel_unescaped_test() ->
    test_helper(<<"{{{name}}}">>, <<">dont&do<it>">>, #{<<"name">> => <<">dont&do<it>">>}).

tag_type_variable_unescaped_with_ampersand_test() ->
    test_helper(<<"{{&name}}">>, <<">dont&do<it>">>, #{<<"name">> => <<">dont&do<it>">>}).

tag_type_section_empty_test() ->
    test_helper(<<"{{#name}}section{{/name}}">>, <<>>, #{}).

tag_type_section_false_test() ->
    test_helper(<<"{{#name}}section{{/name}}">>, <<>>, #{<<"name">> => false}).

tag_type_section_true_test() ->
    test_helper(<<"{{#name}}section{{/name}}">>, <<"section">>, #{<<"name">> => true}).

tag_type_section_empty_list_test() ->
    test_helper(<<"{{#name}}section{{/name}}">>, <<>>, #{<<"name">> => []}).

tag_type_section_nonempty_list_test() ->
    CtxList = #{<<"name">> => [ #{} || _ <- lists:seq(1,3) ]},
    test_helper(<<"{{#name}}section{{/name}}">>, <<"sectionsectionsection">>, CtxList).

tag_type_inverted_section_empty_test() ->
    test_helper(<<"{{^name}}section{{/name}}">>, <<"section">>, #{}).

tag_type_inverted_section_false_test() ->
    test_helper(<<"{{^name}}section{{/name}}">>, <<"section">>, #{<<"name">> => false}).

tag_type_inverted_section_true_test() ->
    test_helper(<<"{{^name}}section{{/name}}">>, <<>>, #{<<"name">> => true}).

tag_type_inverted_section_empty_list_test() ->
    test_helper(<<"{{^name}}section{{/name}}">>, <<"section">>, #{<<"name">> => []}).

tag_type_inverted_section_nonempty_list_test() ->
    CtxList = #{<<"name">> => [ #{} || _ <- lists:seq(1,3)]},
    test_helper(<<"{{^name}}section{{/name}}">>, <<>>, CtxList).


tag_type_comment_test() ->
    test_helper(<<"{{!comment}}">>, <<>>, #{}).

tag_type_comment_empty_test() ->
    test_helper(<<"{{! }}">>, <<>>, #{}).

tag_type_comment_multiline_test() ->
    test_helper(<<"{{!\ncomment\ncomment\ncomment\n\n}}">>, <<>>, #{}).

test_helper(Template, Expected, Ctx) ->
    Result = imperial:render(Template, Ctx),
    ?assertEqual(Expected, Result).
