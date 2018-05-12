#!/bin/sh -u

./build $@

if [ $? -eq 0 ]; then
    echo "Compilation success!"
    echo "success" > recompile_status
else
    echo "Compilation failure."
    # Force user to kill this.
    echo "failure" > recompile_status
    sleep infinity
fi
