#/bin/sh -x

erl -pa $PWD/ebin \
    -sname erl-dev \
    -boot start_sasl -config dev \
    -eval 'code:load_abs("ebin/user_default").'
