This directory contains an unfinished port of py_ecc from python to elixir now named ex_ecc. Compare the existing .py and existing .ex files to discover any modules missing from the port. Then create the missing files by converting the python code into elixir module code. Don't re-create modules that already exist. When that is done run `mix compile` and fix all errors and warnings from this until compilation is clean.

This directory contains an unfinished port of py_ecc from python to elixir now named ex_ecc. Let's focus on the tests. There are two test directories test/ for the elixir tests and /tests with the original python tests. Please port the python tests over to elixir code and add test libraries as needed. Then run the tests using mix test.

This directory contains an unfinished port of py_ecc from python to elixir now named ex_ecc. We want to fix bls12_381 first. Run `./test.sh` and then fix the occuring warnings one by one until no more warnings occurs and there are no more failures. Fix the errors one by one, each time running `./test.sh` again after a change. In order to fix them look boot at the test case and implementations. For reference consult the corresponding original python files and ensure that the implementations and tests are behaving as intended in the original. Keep iterating on this until `./test.sh` does not show any errors anymore and don't ask me but keep going.


Redo this elixir module port in the same way as the port in @bn128_curve.ex . Don't port type enforcement.

Run `./test.sh` and then compare python and elixir implementations. Use @FILES.md to find the right files. Fix the bug in the port and run `./test.sh` again. When fixing the bug do this by correcting elixir port behaviour, but do not add any additional logic to the elixir port that is not present in the python reference. The elixir and python code should get more aligned rather than less. E.g. the bug exists because there is a mismatch. Reducing mismatches and making the code more similiar will be the way to correct the bevahiour. Also where applicable ensure variable names are matching between the port and the reference and the port is not diverging onto different names (apart from the usage of lower casing and underscores).

When comparing python and elixir files please follow the following guidelines:

1/5 Specific Comparison Goals:
"Please compare the implementations in the files accross all functions in the filesboth Python and Elixir, focusing on:
- Input validation logic
- Message handling
- Error conditions
- Return value conditions
- Any differences in the mathematical operations"

2/5 Systematic Analysis:

 "Please analyze the following aspects of both implementations:
   1. Function signatures and parameter handling
   2. Preconditions and validation checks
   3. Core algorithm steps
   4. Error handling
   5. Return value conditions
   For each aspect, highlight any differences between the implementations."

3/5 Security-Critical Differences:

   "Please identify any differences in the implementations that could affect:
   - Security properties
   - Message handling
   - Key validation
   - Signature verification
   - Aggregation behavior"

4/5 Cross-Reference:

   "Please compare how each function in the Elixir implementation relates to its Python counterpart, including:
   - Direct equivalents
   - Functions that have been split or combined
   - Functions that have been moved to different modules
   - Any missing or additional functionality"

5/5 Verification Steps:

   "Please verify that each security-critical operation in the Python implementation has an equivalent in the Elixir implementation, including:
   - Input validation
   - Message preprocessing
   - Key validation
   - Signature verification
   - Aggregation checks"   


