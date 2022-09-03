#!/bin/bash

erl -config config/sys.config -args_file config/vm_args -pa deps/*/ebin -pa ebin
