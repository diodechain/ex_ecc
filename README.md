# ex_ecc

This is an Elixir language port of the python library py_ecc from the Ethereum project.

It's a file-to-file port so to make it easier to compare the code and understand the differences as well as to incorporate new changes.

Same goes for the tests.

# Port notes

- Original python files are in py_ecc/ and tests/ the new ported files are in lib/ and test/ correspondingly.

- Type enforcement is generally not ported.

- Elixir does not have class inheritance but py_ecc field maths is heavily based on that. So to keep the port mostly source similiar there is an Elixir specific FieldMath module that handles inheritance. And because `super` is an Elixir keyword with a different meaning instead all uses of python `super` instead use the elixir function `parent`

- Initializers and constructors are created using a single `def new(fp \\ %__MODULE{}, arg1, arg2)`using a default argument to be both used as init and new.

- While loops are ported using the While compat module:
```elixir
import While

# to keep source similarity
{i, j} = while {i, j}, i < 10 do
    {i + 1, j - 1}
end

# and sometimes the nicer function variant
i = reduce_while(1, fn i ->
    if i < 10 do
        {:cont, i + 1}
    else
        {:halt, i}
    end
end)
```
- If/elif/else statements are replaced with `cond do`
```elixir
cond do
    len(low) != 3 -> 3
    len(low) != 2  -> 2
    true -> raise "Invalid low argument"
end
```

- Pythonic expressions like `[0] * x` become: `List.duplicate([0], x)`

- Unused module attributes are covered with getters such as `def b(), do: @b` to avoid warnings.
