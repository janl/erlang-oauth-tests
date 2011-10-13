Test code for erlang-oauth (http://github.com/tim/erlang-oauth).

If you have erlang-oauth sitting next to erlang-oauth-tests then you
can just run "make" to compile and run the tests (don't forget to compile
erlang-oauth beforehand).

You can compile and run the tests as follows:

  $ make compile
  ...
  $ erl -pa ebin -pa ../path/to/erlang-oauth/ebin -s crypto -noshell -s oauth_unit tests -s init stop
  ...
