#!/usr/bin/env sh
cat << eos | WSPATH=$PWD:/opt/mdyalog/17.1/64/unicode/ws  mapl +s
]dbuild $1
)save
)off
eos
