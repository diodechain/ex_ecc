#!/bin/bash
set -e
export MIX_ENV=test
echo "mix compile"
mix compile --warnings-as-errors
#TEST=test/ex_ecc/bls/ciphersuites/g2_basic_test.exs
#TEST=test/bls/g2_primitives_test.exs:19
# TEST=test/bls/g2_core_test.exs
# TEST=test/bls/point_compression_test.exs
TEST=test/bls/bls12_381_test.exs
echo "mix test $TEST --seed=0 --max-failures=1 2>&1"
mix test $TEST --seed=0 --max-failures=1 2>&1
