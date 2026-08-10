#!/usr/bin/env sh
# Benchmark the native backend against the bytecode VM (stage 10).
#
# For every benchmarks/*.qa: build a native binary, then time it against
# `glados compiler FILE` (the VM).  Verifies the two produce identical
# output before reporting the speedup, so a regression cannot masquerade
# as a fast result.
#
# Usage: benchmarks/run.sh [path-to-glados]
set -eu

GLADOS="${1:-./glados}"
HERE="$(dirname "$0")"
OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT

# Portable millisecond wall-clock around a command.
now_ms() { date +%s%3N; }

printf '%-14s %10s %10s %9s\n' "benchmark" "vm(ms)" "native(ms)" "speedup"
printf '%-14s %10s %10s %9s\n' "---------" "------" "----------" "-------"

for src in "$HERE"/*.qa; do
    name="$(basename "$src" .qa)"
    bin="$OUT/$name"

    "$GLADOS" compiler "$src" --native "$bin" >/dev/null

    vm_out="$("$GLADOS" compiler "$src")"
    nat_out="$("$bin")"
    if [ "$vm_out" != "$nat_out" ]; then
        printf '%-14s  MISMATCH -- vm and native output differ, skipping\n' "$name"
        continue
    fi

    t0="$(now_ms)"; "$GLADOS" compiler "$src" >/dev/null; t1="$(now_ms)"
    t2="$(now_ms)"; "$bin" >/dev/null; t3="$(now_ms)"

    vm_ms=$((t1 - t0))
    nat_ms=$((t3 - t2))
    if [ "$nat_ms" -eq 0 ]; then nat_ms=1; fi
    speedup="$(awk "BEGIN { printf \"%.1fx\", $vm_ms / $nat_ms }")"

    printf '%-14s %10s %10s %9s\n' "$name" "$vm_ms" "$nat_ms" "$speedup"
done
