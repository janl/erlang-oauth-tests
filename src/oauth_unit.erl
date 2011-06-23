-module(oauth_unit).

-compile(export_all).

tests() ->
  tests("data").

tests(Path) ->
  signature_base_string_tests(Path),
  plaintext_tests(Path),
  hmac_sha1_tests(Path),
  rsa_sha1_tests(Path).

signature_base_string_tests(Dirname) ->
  foreach(Dirname, "base_string_test_*", fun (Path) ->
    [Method, URL, Params, BaseString] = path_get_values([method, url, params, base_string], Path),
    test(Path, oauth, signature_base_string, [Method, URL, Params], BaseString)
  end).

plaintext_tests(Dirname) ->
  foreach(Dirname, "plaintext_test_*", fun (Path) ->
    [Consumer, TokenSecret, Signature] = path_get_values([consumer, token_secret, signature], Path),
    test(Path, oauth, plaintext_signature, [Consumer, TokenSecret], Signature),
    test(Path, oauth, plaintext_verify, [Signature, Consumer, TokenSecret], true)
  end).

hmac_sha1_tests(Dirname) ->
  foreach(Dirname, "hmac_sha1_test_*", fun (Path) ->
    {ok, Data} = file:consult(Path),
    BaseString = proplists:get_value(base_string, Data),
    Consumer = proplists:get_value(consumer, Data),
    TokenSecret = proplists:get_value(token_secret, Data),
    Signature = proplists:get_value(signature, Data),
    test(Path, oauth, hmac_sha1_signature, [BaseString, Consumer, TokenSecret], Signature),
    test(Path, oauth, hmac_sha1_verify, [Signature, BaseString, Consumer, TokenSecret], true)
  end).

rsa_sha1_tests(Dirname) ->
  Path = filename:join(Dirname, "rsa_sha1_test"),
  Pkey = filename:join(Dirname, "rsa_sha1_private_key.pem"),
  Cert = filename:join(Dirname, "rsa_sha1_certificate.pem"),
  [BaseString, Signature] = path_get_values([base_string, signature], Path),
  test("rsa_sha1_test", oauth, rsa_sha1_signature, [BaseString, {"", Pkey, rsa_sha1}], Signature),
  test("rsa_sha1_test", oauth, rsa_sha1_verify, [Signature, BaseString, {"", Cert, rsa_sha1}], true).

test(Path, M, F, A, Expected) ->
  case apply(M, F, A) of
    Expected ->
      io:format("ok - ~p:~p (~s)~n", [M, F, Path]);
    Actual ->
      io:format("not ok - ~p:~p (~s)~n", [M, F, Path]),
      io:format(comment(iolist_to_binary(io_lib:format("~p~n", [Actual]))))
  end.

path_get_values(Keys, Path) ->
  {ok, Proplist} = file:consult(Path),
  proplist_get_values(Keys, Proplist).

proplist_get_values(Keys, Proplist) ->
  [proplists:get_value(K, Proplist) || K <- Keys].

foreach(Dirname, Basename, Fun) ->
  lists:foreach(Fun, filelib:wildcard(filename:join(Dirname, Basename))).

comment(String) ->
  re:replace(String, "^", "# ", [global, multiline]).
