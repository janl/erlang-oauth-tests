-module(oauth_unit).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

wildcard(Pattern) ->
  filelib:wildcard("data/" ++ Pattern).

read(Keys, Path) ->
  {ok, Proplist} = file:consult(Path),
  [proplists:get_value(K, Proplist) || K <- Keys].

signature_base_string_test_() ->
  lists:map(fun (Path) -> signature_base_string_test_fun(Path) end, wildcard("base_string_test_*")).

signature_base_string_test_fun(Path) ->
  [Method, URL, Params, BaseString] = read([method, url, params, base_string], Path),
  ?_assertEqual(BaseString, oauth:signature_base_string(Method, URL, Params)).

plaintext_signature_test_() ->
  plaintext_signature_test_funs(wildcard("plaintext_test_*"), []).

plaintext_signature_test_funs([], Tests) ->
  Tests;
plaintext_signature_test_funs([Path | Paths], Tests) ->
  [Consumer, TokenSecret, Signature] = read([consumer, token_secret, signature], Path),
  SignatureTest = ?_assertEqual(Signature, oauth:plaintext_signature(Consumer, TokenSecret)),
  VerifyTest = ?_assertEqual(true, oauth:plaintext_verify(Signature, Consumer, TokenSecret)),
  plaintext_signature_test_funs(Paths, [SignatureTest, VerifyTest | Tests]).

hmac_sha1_signature_test_() ->
  hmac_sha1_signature_test_funs(wildcard("hmac_sha1_test_*"), []).

hmac_sha1_signature_test_funs([], Tests) ->
  Tests;
hmac_sha1_signature_test_funs([Path | Paths], Tests) ->
  [BaseString, Consumer, TokenSecret, Signature] = read([base_string, consumer, token_secret, signature], Path),
  SignatureTest = ?_assertEqual(Signature, oauth:hmac_sha1_signature(BaseString, Consumer, TokenSecret)),
  VerifyTest = ?_assertEqual(true, oauth:hmac_sha1_verify(Signature, BaseString, Consumer, TokenSecret)),
  hmac_sha1_signature_test_funs(Paths, [SignatureTest, VerifyTest | Tests]).

rsa_sha1_test_() ->
  Pkey = "data/rsa_sha1_private_key.pem",
  Cert = "data/rsa_sha1_certificate.pem",
  [BaseString, Signature] = read([base_string, signature], wildcard("rsa_sha1_test*")),
  SignatureTest = ?_assertEqual(Signature, oauth:rsa_sha1_signature(BaseString, {"", Pkey, rsa_sha1})),
  VerifyTest = ?_assertEqual(true, oauth:rsa_sha1_verify(Signature, BaseString, {"", Cert, rsa_sha1})),
  [SignatureTest, VerifyTest].
