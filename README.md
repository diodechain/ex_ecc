# ex_ecc

This is an Elixir language port of the python library py_ecc from the Ethereum project.

It's a file-to-file port so to make it easier to compare the code and understand the differences as well as to incorporate new changes.

Same goes for the tests.

# Port notes

- Original python files are in py_ecc/ and tests/ the new ported files are in lib/ and test/ correspondingly.

- Elixir does not have class inheritance but py_ecc field maths is heavily based on that. So to keep the port mostly source similiar there is an Elixir specific FieldMath module that handles inheritance.

- Initializers and constructors are created using a single `def new(fp \\ %__MODULE{}, arg1, arg2)`using a default argument to be both used as init and new.

- Unused module attributes are covered with getters such as `def b(), do: @b` to avoid warnings.
