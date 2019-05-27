#!/bin/bash

diff -u --color=always -- <(cat nestest.log | cut -c1-15,17-19,48-73) <(cargo run | cut -c1-15,17-19,48-73) | less -r
