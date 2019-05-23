#!/bin/bash

diff -u --color=always -- <(cat nestest.log | cut -c1-19,45-73) <(cargo run | cut -c1-19,45-73) | less -r
