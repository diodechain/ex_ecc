defmodule ExEcc.FieldMath do
  alias ExEcc.FieldMath

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
                corresponding_fq_class: nil,
                sgn0: nil

      def parent(), do: unquote(parent)
      def zero(), do: FieldMath.resolve(parent(), :zero, 1).zero(__MODULE__)
      def one(), do: FieldMath.resolve(parent(), :one, 1).one(__MODULE__)

      def new(fq \\ %__MODULE__{}, val) when is_integer(val) or is_tuple(val) do
        FieldMath.new(fq, val)
      end
    end
  end

  def new(fq, val) do
    parent = resolve(parent(fq), :new, 2)
    parent.new(fq, val)
  end

  def add(a, b) when is_integer(a) and not is_integer(b), do: call(:add, b, a)
  def add(a, b), do: call(:add, a, b)
  def mul(a, b) when is_integer(a) and not is_integer(b), do: call(:mul, b, a)
  def mul(a, b), do: call(:mul, a, b)
  def sub(a, b), do: call(:sub, a, b)

  def div(n, fqp) when is_integer(n) and not is_integer(fqp) do
    n = mod_int(ExEcc.Utils.prime_field_inv(fqp.n, field_modulus(fqp)) * n, field_modulus(fqp))
    FieldMath.new(fqp, n)
  end

  def div(a, b), do: call(:div, a, b)

  def pow(a, b), do: call(:pow, a, b)
  def inv(a), do: call(:inv, a)
  def eq(a, b), do: call(:eq, a, b)
  def neg(a), do: call(:neg, a)
  def lt(a, b), do: call(:lt, a, b)

  def sgn0(a), do: get(:sgn0, a)
  def fq2_modulus_coeffs(a), do: get(:fq2_modulus_coeffs, a)
  def fq12_modulus_coeffs(a), do: get(:fq12_modulus_coeffs, a)
  def field_modulus(a), do: get(:field_modulus, a)

  def coeffs(a) do
    coeffs = get(:coeffs, a)

    if not is_tuple(coeffs) do
      raise "coeffs is not a tuple: #{inspect(coeffs)}"
    end

    coeffs
  end

  def coeffs_list(a), do: Tuple.to_list(get(:coeffs, a))
  def coeffs(a, index), do: elem(get(:coeffs, a), index)
  def modulus_coeffs(a), do: get(:modulus_coeffs, a)
  def modulus_coeffs_list(a), do: Tuple.to_list(get(:modulus_coeffs, a))
  def modulus_coeffs(a, index), do: elem(get(:modulus_coeffs, a), index)
  def degree(a), do: get(:degree, a)
  def parent(a), do: get(:parent, a)
  def corresponding_fq_class(a), do: get(:corresponding_fq_class, a)
  def mc_tuples(a), do: get(:mc_tuples, a)

  def mod_int(a, b) when is_integer(b) do
    case a do
      %{n: n} -> rem(n, b)
      n when is_integer(n) -> rem(n, b)
      _ -> raise "Only int and T_FQ types are accepted: got {type(x)}"
    end
  end

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
        degree: nil,
        sgn0: nil
      },
      fn {key, value}, acc -> Map.put(acc, key, value) end
    )
  end

  def type(%type{}), do: type(type)
  def type(type) when is_atom(type), do: type
  def type(int) when is_integer(int), do: ExEcc.IntegerMath

  def isinstance(nil, _), do: false

  def isinstance(a, :int_types_or_FQ) do
    Enum.any?([ExEcc.IntegerMath, ExEcc.Fields.FQ], fn t -> isinstance(a, t) end)
  end

  def isinstance(a, ExEcc.IntegerMath), do: is_integer(a)

  def isinstance(a, other_type) when is_atom(other_type) do
    type(a) == other_type ||
      Enum.any?(List.wrap(parent(a)), fn p -> isinstance(p, other_type) end)
  end

  defp get(atom, nil) do
    raise "No value found for #{atom}"
  end

  defp get(atom, a) do
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

  def resolve(n, _op, _param_count) when is_integer(n), do: ExEcc.IntegerMath

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
    ensure_compiled?(type) && function_exported?(type, op, param_count)
  end

  defp ensure_compiled?(type) do
    if String.starts_with?(to_string(type), "Elixir.") do
      case Code.ensure_compiled(type) do
        {:module, _} -> true
        _ -> false
      end
    else
      false
    end
  end
end

defmodule ExEcc.IntegerMath do
  def add(a, b), do: a + b
  def mul(a, b), do: a * b
  def sub(a, b), do: a - b
  def div(a, b), do: Kernel.div(a, b)
  def pow(a, b), do: Integer.pow(a, b)
  def inv(a), do: Kernel.div(1, a)
  def eq(a, b), do: a == b
  def neg(a), do: -a
  def lt(a, b), do: a < b
  def sgn0(a), do: if(a < 0, do: -1, else: 1)
  def mod_int(a, b), do: rem(a, b)
  def type(_), do: ExEcc.IntegerMath

  def pow(integer, exponent, modulus) when is_integer(exponent) and is_integer(modulus) do
    cond do
      exponent == 0 ->
        1

      exponent == 1 ->
        integer

      rem(exponent, 2) == 0 ->
        pow(rem(integer * integer, modulus), Kernel.div(exponent, 2), modulus)

      true ->
        pow(rem(integer * integer, modulus), Kernel.div(exponent, 2), modulus) * integer
    end
    |> rem(modulus)
  end

  def zero(), do: 0
  def one(), do: 1
end
