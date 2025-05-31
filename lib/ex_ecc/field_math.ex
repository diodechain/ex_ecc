defmodule ExEcc.FieldMath do
  defmacro __using__(opts) do
    parent = opts[:parent]

    quote do
      alias ExEcc.FieldMath

      defstruct []
      def parent(), do: unquote(parent)

      def new(fq \\ %__MODULE__{}, val) do
        parent = FieldMath.resolve(parent(), :new, 2)
        parent.new(fq, val)
      end
    end
  end

  def add(a, b), do: call(:add, a, b)
  def mul(a, b), do: call(:mul, a, b)
  def sub(a, b), do: call(:sub, a, b)
  def div(a, b), do: call(:div, a, b)
  def pow(a, b), do: call(:pow, a, b)
  def inv(a), do: call(:inv, a)
  def eq(a, b), do: call(:eq, a, b)
  def neq(a, b), do: call(:neq, a, b)
  def lt(a, b), do: call(:lt, a, b)

  def fq2_modulus_coeffs(a), do: get(:fq2_modulus_coeffs, a)
  def fq12_modulus_coeffs(a), do: get(:fq12_modulus_coeffs, a)
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

  def type(%type{}), do: type(type)
  def type(type) when is_atom(type), do: type

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
    type = type(a)

    cond do
      is_map(a) and Map.has_key?(a, atom) ->
        Map.get(a, atom)

      function_exported?(type, atom, 0) ->
        apply(type, atom, [])

      true ->
        Enum.find_value(List.wrap(a.parent()), fn p ->
          function_exported?(p, atom, 0) && apply(p, atom, [])
        end)
    end
  end

  def resolve([type | rest], op, params) do
    resolve(type, op, params) || resolve(rest, op, params)
  end

  def resolve(type, op, params) when is_atom(type) do
    if function_exported?(type(type), op, params) do
      type
    else
      if parent = get(:parent, type) do
        resolve(parent, op, params)
      end
    end
  end
end
