#!/usr/bin/bash

if [[ $(hostname) != "suntab" || $(hostname) != "machine" ]]; then
    slock
fi
