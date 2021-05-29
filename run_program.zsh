#!/bin/zsh

SRC="src/"
MOD="modules/"
DATA_DIR="data/"

FLAGS=(-Og -Wall -fimplicit-none -fcheck=all -fbacktrace)
MODS=(constants.f95 io.f95 shell_prompts.f95)
MAIN=main.f95
EXE=main.out

[ ! -d "$MOD" ] && mkdir -p "$MOD"
[ ! -d "$DATA_DIR" ] && mkdir -p "$DATA_DIR"

gfortran "${FLAGS[@]}" -J$MOD "${MODS[@]}" $MAIN -o $EXE &&
time ./$EXE &&
gnuplot plotMR.gnu &&
gnuplot plotSeg.gnu