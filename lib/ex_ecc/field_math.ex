defmodule ExEcc.FieldMath do
  defmacro __using__(opts) do
    parent = opts[:parent]

    quote do
      alias ExEcc.FieldMath

      defstruct n: nil,
                field_modulus: nil,
                fq12_modulus_coeffs: nil,
                fq2_modulus_coeffs: nil,
                coeffs: nil,
                modulus_coeffs: nil,
                degree: nil,
                mc_tuples: nil,
                corresponding_fq_class: nil

      def parent(), do: unquote(parent)
      def zero(), do: FieldMath.resolve(parent(), :zero, 1).zero(__MODULE__)
      def one(), do: FieldMath.resolve(parent(), :one, 1).one(__MODULE__)

      def new(fq \\ %__MODULE__{}, val) do
        FieldMath.new(fq, val)
      end
    end
  end

  def new(fq, val) do
    parent = resolve(parent(fq), :new, 2)

    try do
      parent.new(fq, val)
    rescue
      error ->
        IO.inspect(error, label: "error")
        reraise error, __STACKTRACE__
    end
  end

  def add(a, b), do: call(:add, a, b)
  def mul(a, b), do: call(:mul, a, b)
  def sub(a, b), do: call(:sub, a, b)
  def div(a, b), do: call(:div, a, b)
  def pow(a, b), do: call(:pow, a, b)
  def inv(a), do: call(:inv, a)
  def eq(a, b), do: call(:eq, a, b)
  def neg(a), do: call(:neg, a)
  def lt(a, b), do: call(:lt, a, b)

  def fq2_modulus_coeffs(a), do: get(:fq2_modulus_coeffs, a)
  def fq12_modulus_coeffs(a), do: get(:fq12_modulus_coeffs, a)
  def field_modulus(a), do: get(:field_modulus, a)
  def coeffs(a), do: get(:coeffs, a)
  def coeffs_list(a), do: Tuple.to_list(get(:coeffs, a))
  def coeffs(a, index), do: elem(get(:coeffs, a), index)
  def modulus_coeffs(a), do: get(:modulus_coeffs, a)
  def modulus_coeffs_list(a), do: Tuple.to_list(get(:modulus_coeffs, a))
  def modulus_coeffs(a, index), do: elem(get(:modulus_coeffs, a), index)
  def degree(a), do: get(:degree, a)
  def parent(a), do: get(:parent, a)
  def corresponding_fq_class(a), do: get(:corresponding_fq_class, a)
  def mc_tuples(a), do: get(:mc_tuples, a)

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
        parent: base_class,
        n: nil,
        field_modulus: nil,
        fq12_modulus_coeffs: nil,
        fq2_modulus_coeffs: nil,
        coeffs: nil,
        modulus_coeffs: nil,
        degree: nil
      },
      fn {key, value}, acc -> Map.put(acc, key, value) end
    )
  end

  def type(%type{}), do: type(type)
  def type(type) when is_atom(type), do: type
  def type(int) when is_integer(int), do: :int

  def isinstance(nil, _), do: false

  def isinstance(a, :int_types_or_FQ) do
    Enum.any?([:int, FQ], fn t -> isinstance(a, t) end)
  end

  def isinstance(a, :int), do: is_integer(a)

  def isinstance(a, other_type) when is_atom(other_type) do
    type(a) == other_type ||
      Enum.any?(List.wrap(parent(a)), fn p -> isinstance(p, other_type) end)
  end

  defp get(atom, nil) do
    raise "No value found for #{atom}"
  end

  defp get(atom, a) do
    # IO.inspect(a, label: "get(#{atom} in #{inspect(a)})")
    type = type(a)

    cond do
      function?(type, atom, 0) ->
        apply(type, atom, [])

      is_map(a) and Map.has_key?(a, atom) ->
        Map.get(a, atom)

      function?(type, :parent, 0) ->
        Enum.find_value(List.wrap(type.parent()), fn p ->
          function?(p, atom, 0) && apply(p, atom, [])
        end)

      is_map(a) and Map.has_key?(a, :parent) ->
        Enum.find_value(List.wrap(a.parent), fn p ->
          function?(p, atom, 0) && apply(p, atom, [])
        end)

      true ->
        nil
    end
  end

  def resolve([type | rest], op, param_count) do
    resolve(type, op, param_count) || resolve(rest, op, param_count)
  end

  def resolve(type, op, param_count) do
    if function?(type, op, param_count) do
      type
    else
      parent = get(:parent, type)

      if parent != nil do
        resolve(parent, op, param_count)
      end
    end
  end

  defp function?(type, op, param_count) do
    type = type(type)
    Code.ensure_compiled?(type) and function_exported?(type, op, param_count)
  end
end
