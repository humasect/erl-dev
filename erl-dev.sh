#/bin/sh -x

erl -pa $PWD/ebin $PWD/*/ebin \
    -sname erl-dev \
    -boot start_sasl -config erl.config \
    -eval 'code:load_abs("ebin/user_default").'
