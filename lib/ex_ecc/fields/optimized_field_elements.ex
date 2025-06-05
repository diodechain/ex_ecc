defmodule ExEcc.Fields.OptimizedFQ do
  alias ExEcc.Utils
  alias ExEcc.FieldMath

  defstruct n: 0, field_modulus: nil

  def new(fq \\ %__MODULE__{}, val) do
    if FieldMath.field_modulus(fq) == nil do
      raise "Field Modulus hasn't been specified"
    end

    n =
      cond do
        FieldMath.isinstance(val, FieldMath.type(fq)) ->
          val.n

        FieldMath.isinstance(val, ExEcc.IntegerMath) ->
          Integer.mod(val, FieldMath.field_modulus(fq))

        true ->
          raise "Expected an int or FQ object, but got #{inspect(val)}"
      end

    %{fq | n: n}
  end

  def add(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    n = FieldMath.mod_int(fq.n + on, FieldMath.field_modulus(fq))
    FieldMath.new(fq, n)
  end

  def mul(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    n = FieldMath.mod_int(fq.n * on, FieldMath.field_modulus(fq))
    FieldMath.new(fq, n)
  end

  def sub(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    n = FieldMath.mod_int(fq.n - on, FieldMath.field_modulus(fq))
    FieldMath.new(fq, n)
  end

  def div(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    n =
      FieldMath.mod_int(
        fq.n * Utils.prime_field_inv(on, FieldMath.field_modulus(fq)),
        FieldMath.field_modulus(fq)
      )

    FieldMath.new(fq, n)
  end

  def pow(fq, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        FieldMath.type(fq).new(1)

      exponent == 1 ->
        FieldMath.type(fq).new(fq.n)

      Integer.mod(exponent, 2) == 0 ->
        FieldMath.pow(FieldMath.mul(fq, fq), Kernel.div(exponent, 2))

      true ->
        FieldMath.pow(FieldMath.mul(fq, fq), Kernel.div(exponent, 2)) |> FieldMath.mul(fq)
    end
  end

  def eq(fq1, other) do
    case other do
      %{n: n} ->
        fq1.n == n

      n when is_integer(n) ->
        fq1.n == n

      _ ->
        raise "Expected an int or FQ object, but got #{inspect(other)}"
    end
  end

  def ne(fq, other) do
    not FieldMath.eq(fq, other)
  end

  def neg(fq) do
    FieldMath.type(fq).new(-fq.n)
  end

  def repr(fq) do
    inspect(fq.n)
  end

  def int(fq) do
    fq.n
  end

  def lt(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    fq.n < on
  end

  def one(cls) do
    cls.new(1)
  end

  def zero(cls) do
    cls.new(0)
  end

  # Optimized sgn0 implementation for m = 1
  def sgn0(fq) do
    Integer.mod(fq.n, 2)
  end

  # Comparison functions for ordering (Python's @total_ordering)
  def compare(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    cond do
      fq.n > on -> 1
      fq.n < on -> -1
      true -> 0
    end
  end
end

defmodule ExEcc.Fields.OptimizedFQP do
  @moduledoc """
  A module for elements in polynomial extension fields with optimized operations
  """

  alias ExEcc.Utils
  alias ExEcc.FieldMath
  alias __MODULE__, as: FQP
  import While
  import Bitwise

  defstruct coeffs: {}, modulus_coeffs: {}, degree: 0, field_modulus: nil, mc_tuples: []

  def new(fqp, coeffs, modulus_coeffs \\ {}) do
    if FieldMath.field_modulus(fqp) == nil do
      raise "Field Modulus hasn't been specified"
    end

    if tuple_size(coeffs) != tuple_size(modulus_coeffs) do
      raise "coeffs (#{tuple_size(coeffs)}) and modulus_coeffs (#{tuple_size(modulus_coeffs)}) aren't of the same length"
    end

    # Not converting coeffs to FQ or explicitly making them integers
    # for performance reasons
    coeffs =
      if FieldMath.isinstance(elem(coeffs, 0), ExEcc.IntegerMath) do
        Enum.map(Tuple.to_list(coeffs), fn c -> Integer.mod(c, FieldMath.field_modulus(fqp)) end)
        |> List.to_tuple()
      else
        coeffs
      end

    # The coefficients of the modulus, without the leading [1]
    modulus_coeffs = Enum.map(Tuple.to_list(modulus_coeffs), fn c -> c end)
    # The degree of the extension field
    degree = length(modulus_coeffs)

    # Precompute tuples of (i, c) for non-zero coefficients
    mc_tuples =
      Enum.with_index(modulus_coeffs)
      |> Enum.filter(fn {c, _} -> c != 0 end)
      |> Enum.map(fn {c, i} -> {i, c} end)

    %{
      fqp
      | degree: degree,
        coeffs: coeffs,
        modulus_coeffs: List.to_tuple(modulus_coeffs),
        mc_tuples: mc_tuples
    }
  end

  def add(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    Enum.zip(FieldMath.coeffs_list(fqp), FieldMath.coeffs_list(other))
    |> Enum.map(fn {x, y} -> Integer.mod(x + y, FieldMath.field_modulus(fqp)) end)
    |> List.to_tuple()
    |> FieldMath.type(fqp).new()
  end

  def sub(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(other)}"
    end

    Enum.zip(FieldMath.coeffs_list(fqp), FieldMath.coeffs_list(other))
    |> Enum.map(fn {x, y} -> Integer.mod(x - y, FieldMath.field_modulus(fqp)) end)
    |> List.to_tuple()
    |> FieldMath.type(fqp).new()
  end

  def mul(fqp, other) do
    cond do
      is_integer(other) ->
        for c <- FieldMath.coeffs_list(fqp) do
          Integer.mod(c * other, FieldMath.field_modulus(fqp))
        end
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      FieldMath.isinstance(other, FQP) ->
        b = List.duplicate(0, FieldMath.degree(fqp) * 2 - 1)

        # Optimized polynomial multiplication
        inner_enumerate = Enum.with_index(FieldMath.coeffs_list(other))
        self_enumerate = Enum.with_index(FieldMath.coeffs_list(fqp))

        b =
          for {eli, i} <- self_enumerate, {elj, j} <- inner_enumerate do
            {eli, elj, i, j}
          end
          |> Enum.reduce(b, fn {eli, elj, i, j}, b ->
            List.update_at(b, i + j, fn val -> val + eli * elj end)
          end)

        # MID = len(self.coeffs) // 2
        Enum.reduce((FieldMath.degree(fqp) - 2)..0//-1, b, fn exp, b ->
          {top, b} = List.pop_at(b, -1)

          FieldMath.mc_tuples(fqp)
          |> Enum.reduce(b, fn {i, c}, b ->
            List.update_at(b, exp + i, fn val -> val - top * c end)
          end)
        end)
        |> Enum.map(&Integer.mod(&1, FieldMath.field_modulus(fqp)))
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      true ->
        raise "Expected an int or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def div(fqp, other) do
    cond do
      is_integer(other) ->
        for c <- FieldMath.coeffs_list(fqp) do
          Integer.mod(
            c * Utils.prime_field_inv(other, FieldMath.field_modulus(fqp)),
            FieldMath.field_modulus(fqp)
          )
        end
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      FieldMath.isinstance(other, FQP) ->
        FieldMath.mul(fqp, FieldMath.inv(other))

      true ->
        raise "Expected an int or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def pow(fqp, other) when is_integer(other) do
    o =
      [1 | List.duplicate(0, FieldMath.degree(fqp) - 1)]
      |> List.to_tuple()
      |> FieldMath.type(fqp).new()

    reduce_while({o, other, fqp}, fn {o, other, t} ->
      if other > 0 do
        IO.inspect({o, t, other, (other &&& 1) == 1}, label: "POW-LOOP")
        o = if (other &&& 1) == 1, do: FieldMath.mul(o, t), else: o
        {:cont, {o, other >>> 1, FieldMath.mul(t, t)}}
      else
        {:halt, o}
      end
    end)
  end

  def optimized_poly_rounded_div(fqp, a, b) do
    dega = Utils.deg(a)
    degb = Utils.deg(b)
    temp = Enum.to_list(a)
    o = List.duplicate(0, tuple_size(dega))

    {o, _temp} =
      Enum.reduce((dega - degb)..0//-1, {o, temp}, fn i, {o, temp} ->
        o =
          List.update_at(o, i, fn val ->
            val +
              Enum.at(temp, degb + i) *
                Utils.prime_field_inv(Enum.at(b, degb), FieldMath.field_modulus(fqp))
          end)

        temp =
          Enum.reduce(0..degb, temp, fn c, temp ->
            List.update_at(temp, c + i, fn val -> val - Enum.at(o, c) end)
          end)

        {o, temp}
      end)

    Enum.map(o, &Integer.mod(&1, FieldMath.field_modulus(fqp)))
    |> Enum.take(Utils.deg(o) + 1)
  end

  def inv(fqp) do
    {lm, hm} = {
      [1] ++ List.duplicate(0, FieldMath.degree(fqp)),
      List.duplicate(0, FieldMath.degree(fqp) + 1)
    }

    {low, high} = {
      FieldMath.coeffs_list(fqp) ++ [0],
      FieldMath.modulus_coeffs_list(fqp) ++ [1]
    }

    {lm, low, _hm, _high} =
      reduce_while({lm, low, hm, high}, fn {lm, low, hm, high} ->
        if Utils.deg(low) > 0 do
          r = optimized_poly_rounded_div(fqp, high, low)
          r = r ++ List.duplicate(0, FieldMath.degree(fqp) + 1 - length(r))
          nm = hm
          new = high
          # assert len(lm) == len(hm) == len(low) == len(high) == self.degree + 1
          ref = length(lm)

          if ref != length(hm) or ref != length(low) or ref != length(high) or
               ref != FieldMath.degree(fqp) + 1 do
            raise "len(lm) != len(hm) != len(low) != len(high)"
          end

          {nm, new} =
            for(i <- 0..FieldMath.degree(fqp), j <- 0..(FieldMath.degree(fqp) - i), do: {i, j})
            |> Enum.reduce({nm, new}, fn {i, j}, {nm, new} ->
              nm = List.update_at(nm, i + j, fn val -> val - Enum.at(lm, i) * Enum.at(r, j) end)

              new =
                List.update_at(new, i + j, fn val -> val - Enum.at(low, i) * Enum.at(r, j) end)

              {nm, new}
            end)

          nm = Enum.map(nm, &Integer.mod(&1, FieldMath.field_modulus(fqp)))
          new = Enum.map(new, &Integer.mod(&1, FieldMath.field_modulus(fqp)))
          {:cont, {nm, new, lm, low}}
        else
          {:halt, {lm, low, hm, high}}
        end
      end)

    Enum.take(lm, FieldMath.degree(fqp))
    |> List.to_tuple()
    |> FieldMath.type(fqp).new()
    |> FieldMath.div(List.first(low))
  end

  def repr(fqp) do
    inspect(FieldMath.coeffs(fqp))
  end

  def eq(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(other)}"
    end

    Enum.zip(FieldMath.coeffs_list(fqp), FieldMath.coeffs_list(other))
    |> Enum.all?(fn {c1, c2} -> c1 == c2 end)
  end

  def ne(fqp, other) do
    not FieldMath.eq(fqp, other)
  end

  def neg(fqp) do
    Enum.map(FieldMath.coeffs_list(fqp), &(-&1))
    |> List.to_tuple()
    |> FieldMath.type(fqp).new()
  end

  # Optimized sgn0 implementation
  def sgn0(fqp) do
    {sign, _zero} =
      FieldMath.coeffs_list(fqp)
      |> Enum.reduce({false, true}, fn x_i, {sign, zero} ->
        sign_i = FieldMath.mod_int(x_i, 2) == 1
        zero_i = x_i == 0
        {sign || (zero && sign_i), zero && zero_i}
      end)

    if sign, do: 1, else: 0
  end

  def one(cls) do
    cls.new(List.to_tuple([1] ++ List.duplicate(0, FieldMath.degree(cls) - 1)))
  end

  def zero(cls) do
    cls.new(List.to_tuple(List.duplicate(0, FieldMath.degree(cls))))
  end
end

defmodule ExEcc.Fields.OptimizedFQ2 do
  @moduledoc """
  The quadratic extension field with optimized operations
  """

  alias ExEcc.Fields.OptimizedFQP
  alias ExEcc.FieldMath

  defstruct degree: 2,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq2_modulus_coeffs: "FQ2_modulus_coeffs_type",
            mc_tuples: []

  def parent(), do: OptimizedFQP
  def degree(), do: 2

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if FieldMath.fq2_modulus_coeffs(fqp) == nil do
      raise "FQ2 Modulus Coeffs haven't been specified"
    end

    mc_tuples =
      Enum.with_index(Tuple.to_list(FieldMath.fq2_modulus_coeffs(fqp)))
      |> Enum.filter(fn {c, _} -> c != 0 end)
      |> Enum.map(fn {c, i} -> {i, c} end)

    parent().new(%{fqp | mc_tuples: mc_tuples}, coeffs, FieldMath.fq2_modulus_coeffs(fqp))
  end

  # Optimized sgn0 implementation for m = 2
  def sgn0(fq2) do
    {x_0, x_1} = fq2.coeffs
    sign_0 = Integer.mod(x_0, 2)
    zero_0 = x_0 == 0
    sign_1 = Integer.mod(x_1, 2)
    sign_0 || (zero_0 && sign_1)
  end
end

defmodule ExEcc.Fields.OptimizedFQ12 do
  @moduledoc """
  The 12th-degree extension field with optimized operations
  """

  alias ExEcc.Fields.OptimizedFQP
  alias ExEcc.FieldMath

  defstruct degree: 12,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq12_modulus_coeffs: "FQ12_modulus_coeffs_type"

  def parent(), do: OptimizedFQP
  def degree(), do: 12

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if FieldMath.fq12_modulus_coeffs(fqp) == nil do
      raise "FQ12 Modulus Coeffs haven't been specified"
    end

    parent().new(fqp, coeffs, FieldMath.fq12_modulus_coeffs(fqp))
  end
end
