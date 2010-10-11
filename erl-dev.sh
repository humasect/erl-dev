#/bin/sh -x

erl -pa $PWD/ebin $PWD/*/ebin \
    -name erl-dev \
    -boot start_sasl -config erl.config \
    -eval 'code:load_abs("ebin/user_default").'
