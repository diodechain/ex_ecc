#!/bin/bash
echo "mix test test/ex_ecc/bls/ciphersuites/g2_basic_test.exs --max-failures=1 2>&1 | head -n 50"
mix test test/ex_ecc/bls/ciphersuites/g2_basic_test.exs --max-failures=1 2>&1 | head -n 50
