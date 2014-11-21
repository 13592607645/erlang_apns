ERL=erl
REBAR=./rebar

all:
	${REBAR} get-deps && ${REBAR} compile

clean:
	${REBAR} clean

run: all
	${ERL} -pa ebin -s ssl -s apns
erl: all
	${ERL} -pa ebin -s ssl

xref: all
	${REBAR} xref

eunit: all
	mkdir -p .eunit
	[ ! -e .eunit/priv ] && cp -fr priv .eunit || true
	${REBAR} eunit

test: eunit

