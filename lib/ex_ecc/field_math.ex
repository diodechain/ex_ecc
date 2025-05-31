defmodule ExEcc.FieldMath do
  def add(a, b), do: call(:add, a, b)
  def mul(a, b), do: call(:mul, a, b)
  def sub(a, b), do: call(:sub, a, b)
  def div(a, b), do: call(:div, a, b)
  def pow(a, b), do: call(:pow, a, b)
  def inv(a), do: call(:inv, a)
  def conjugate(a), do: call(:conjugate, a)
  def frobenius(a, b), do: call(:frobenius, a, b)
  def equal?(a, b), do: call(:equal?, a, b)
  def not_equal?(a, b), do: call(:not_equal?, a, b)
  def less_than?(a, b), do: call(:less_than?, a, b)

  def field_modulus(a), do: get(:field_modulus, a)
  def coeffs(a), do: get(:coeffs, a)
  def modulus_coeffs(a), do: get(:modulus_coeffs, a)
  def degree(a), do: get(:degree, a)
  def parent(a), do: get(:parent, a)
  def corresponding_fq_class(a), do: get(:corresponding_fq_class, a)

  defp call(op, a, b) do
    apply(resolve(a, op, 2), op, [a, b])
  end

  defp call(op, a) do
    apply(resolve(a, op, 1), op, [a])
  end

  @doc """
  Corresponds to the `type` function in Python.

  Creates a struct with the given name, base class, and properties.
  """
  def type(name, base_class, props \\ []) do
    Enum.reduce(
      props,
      %{
        __struct__: String.to_atom(name),
        parent: base_class
      },
      fn {key, value}, acc -> Map.put(acc, key, value) end
    )
  end

  def type(a) do
    %type{} = a
    type
  end

  def isinstance(a, :int), do: is_integer(a)

  def isinstance(a, other_type) when is_atom(other_type) do
    type(a) == other_type || isinstance(parent(a), other_type)
  end

  def isinstance(a, [other_type | rest]) do
    isinstance(a, other_type) || isinstance(a, rest)
  end

  def isinstance(_a, []), do: false

  def isinstance(a, :int_types_or_FQ) do
    isinstance(a, [:int, FQ])
  end

  defp get(atom, a) do
    %type{} = a

    cond do
      Map.has_key?(a, atom) -> Map.get(a, atom)
      function_exported?(type, atom, 0) -> type.atom()
      true -> apply(a.parent(), atom, [])
    end
  end

  defp resolve(%type{}, op, params), do: resolve(type, op, params)

  defp resolve(type, op, params) do
    if function_exported?(type, op, length(params)) do
      type
    else
      resolve(type.parent(), op, params)
    end
  end
end
