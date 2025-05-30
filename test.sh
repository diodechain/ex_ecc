#!/bin/bash
mix test test/ex_ecc/bn128_and_bls12_381_test.exs --max-failures=1 2>&1 | head -n 50
