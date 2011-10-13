all: clean compile test

compile:
	@test -d ebin || mkdir ebin
	@erl -make

test:
	@erl -noshell -pa ebin -pa ../erlang-oauth/ebin -s crypto -s oauth_unit test -s init stop

clean:
	@rm -rf ebin/*.beam
