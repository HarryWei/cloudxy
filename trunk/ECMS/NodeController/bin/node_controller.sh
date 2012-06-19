#!/bin/sh
./erts-5.8.4/bin/erl \
-sname cache \
¨Cboot ./releases/0.1.0/start \
¨Cconfig ./releases/0.1.0/sys \
¨Cdetached