#!/bin/bash

diff -u --color=always -- <(cat nestest.log | cut -c1-15,17-19,48-95) <(cargo run | cut -c1-15,17-19,48-95) | less -r
