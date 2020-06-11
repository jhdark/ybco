#!/bin/bash

STUB="magfld"

rm -f $STUB.o
$FLUPRO/flutil/fff $STUB.f
$FLUPRO/flutil/lfluka -o flukamg $STUB.o

