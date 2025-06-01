defmodule ExEcc.Fields.OptimizedFQ do
  alias ExEcc.Utils
  alias ExEcc.FieldMath

  defstruct n: 0, field_modulus: nil

  def new(fq \\ %__MODULE__{}, val) do
    if not FieldMath.field_modulus(fq) do
      raise "Field Modulus hasn't been specified"
    end

    n =
      cond do
        FieldMath.isinstance(val, FieldMath.type(fq)) ->
          val.n

        FieldMath.isinstance(val, :int) ->
          rem(val, FieldMath.field_modulus(fq))

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

    FieldMath.rem(fq.n + on, FieldMath.field_modulus(fq))
  end

  def mul(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq.n * on, FieldMath.field_modulus(fq))
  end

  def sub(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq.n - on, FieldMath.field_modulus(fq))
  end

  def div(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(
      fq.n * Utils.prime_field_inv(on, FieldMath.field_modulus(fq)),
      FieldMath.field_modulus(fq)
    )
  end

  def pow(fq, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        FieldMath.type(fq).new(1)

      exponent == 1 ->
        FieldMath.type(fq).new(fq.n)

      rem(exponent, 2) == 0 ->
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
    %type{} = fq
    type.new(-fq.n)
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
    FieldMath.type(cls).new(1)
  end

  def zero(cls) do
    cls.new(0)
  end

  # Optimized sgn0 implementation for m = 1
  def sgn0(fq) do
    rem(fq.n, 2)
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
  import While

  defstruct coeffs: [], modulus_coeffs: [], degree: 0, field_modulus: nil, mc_tuples: []

  def new(fqp \\ %__MODULE__{}, coeffs, modulus_coeffs \\ []) do
    if not FieldMath.field_modulus(fqp) do
      raise "Field Modulus hasn't been specified"
    end

    if length(coeffs) != length(modulus_coeffs) do
      raise "coeffs and modulus_coeffs aren't of the same length"
    end

    # Not converting coeffs to FQ or explicitly making them integers
    # for performance reasons
    coeffs =
      if is_integer(List.first(coeffs)) do
        Enum.map(coeffs, fn c -> rem(c, fqp.field_modulus) end)
      else
        coeffs
      end

    # The coefficients of the modulus, without the leading [1]
    modulus_coeffs = Enum.map(modulus_coeffs, fn c -> c end)
    # The degree of the extension field
    degree = length(modulus_coeffs)

    # Precompute tuples of (i, c) for non-zero coefficients
    mc_tuples = Enum.with_index(modulus_coeffs) |> Enum.filter(fn {c, _} -> c != 0 end)

    %{
      fqp
      | degree: degree,
        coeffs: coeffs,
        modulus_coeffs: modulus_coeffs,
        mc_tuples: mc_tuples
    }
  end

  def add(fqp, other) do
    if not FieldMath.isinstance(fqp, other) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(fqp.coeffs, other.coeffs),
          do: rem(x + y, fqp.field_modulus)
    )
  end

  def sub(fqp, other) do
    if not FieldMath.isinstance(fqp, other) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(fqp.coeffs, other.coeffs),
          do: rem(x - y, fqp.field_modulus)
    )
  end

  def mul(fqp, other) do
    cond do
      is_integer(other) ->
        FieldMath.type(fqp).new(for c <- fqp.coeffs, do: rem(c * other, fqp.field_modulus))

      FieldMath.isinstance(other, FQP) ->
        b = List.duplicate(0, fqp.degree * 2 - 1)

        # Optimized polynomial multiplication
        b =
          Enum.reduce(0..(fqp.degree - 1), b, fn i, acc ->
            Enum.reduce(0..(fqp.degree - 1), acc, fn j, acc ->
              List.update_at(acc, i + j, fn val -> val + fqp.coeffs[i] * other.coeffs[j] end)
            end)
          end)

        # Optimized reduction using precomputed mc_tuples
        b =
          while b, length(b) > fqp.degree do
            {exp, top} = {length(b) - fqp.degree - 1, List.last(b)}
            b = List.delete_at(b, -1)

            Enum.reduce(fqp.mc_tuples, b, fn {i, c}, acc ->
              List.update_at(acc, exp + i, fn val -> val - top * c end)
            end)
          end

        FieldMath.type(fqp).new(Enum.map(b, &rem(&1, fqp.field_modulus)))

      true ->
        raise "Expected an int or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def div(fqp, other) do
    cond do
      is_integer(other) ->
        FieldMath.type(fqp).new(
          for c <- fqp.coeffs,
              do: rem(c * Utils.prime_field_inv(other, fqp.field_modulus), fqp.field_modulus)
        )

      FieldMath.isinstance(other, FQP) ->
        FieldMath.mul(fqp, FieldMath.inv(other))

      true ->
        raise "Expected an int or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def pow(fqp, other) do
    cond do
      other == 0 ->
        FieldMath.type(fqp).new([1] ++ List.duplicate(0, fqp.degree - 1))

      other == 1 ->
        FieldMath.type(fqp).new(fqp.coeffs)

      rem(other, 2) == 0 ->
        FieldMath.pow(FieldMath.mul(fqp, fqp), Kernel.div(other, 2))

      true ->
        FieldMath.pow(FieldMath.mul(fqp, fqp), Kernel.div(other, 2)) |> FieldMath.mul(fqp)
    end
  end

  def inv(fqp) do
    {lm, hm} = {
      [1] ++ List.duplicate(0, fqp.degree),
      List.duplicate(0, fqp.degree + 1)
    }

    {low, high} = {
      fqp.coeffs ++ [0],
      fqp.modulus_coeffs ++ [1]
    }

    {lm, low, _hm, _high} =
      reduce_while({lm, low, hm, high}, fn {lm, low, hm, high} ->
        if Utils.deg(low) do
          r = Utils.poly_rounded_div(high, low)
          r = r ++ List.duplicate(0, fqp.degree + 1 - length(r))
          nm = Enum.map(hm, & &1)
          new = Enum.map(high, & &1)

          {nm, _new} =
            for(i <- 0..fqp.degree, j <- 0..(fqp.degree - i), do: {i, j})
            |> Enum.reduce({nm, new}, fn {i, j}, {nm, new} ->
              nm = List.update_at(nm, i + j, fn val -> val - lm[i] * r[j] end)
              new = List.update_at(new, i + j, fn val -> val - low[i] * r[j] end)
              {nm, new}
            end)

          {nm, new} = Enum.map(nm, &rem(&1, fqp.field_modulus))
          {new, _} = Enum.map(new, &rem(&1, fqp.field_modulus))

          {:cont, {nm, new, lm, low}}
        else
          {:halt, {lm, low, hm, high}}
        end
      end)

    FieldMath.type(fqp).new(Enum.take(lm, fqp.degree)) |> FieldMath.div(List.first(low))
  end

  def repr(fqp) do
    inspect(fqp.coeffs)
  end

  def eq(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(other)}"
    end

    Enum.zip(fqp.coeffs, other.coeffs)
    |> Enum.all?(fn {c1, c2} -> c1 == c2 end)
  end

  def ne(fqp, other) do
    not FieldMath.eq(fqp, other)
  end

  def neg(fqp) do
    FieldMath.type(fqp).new(Enum.map(fqp.coeffs, &(-&1)))
  end

  def one(cls) do
    cls.new([1] ++ List.duplicate(0, cls.degree - 1))
  end

  def zero(cls) do
    cls.new(List.duplicate(0, cls.degree))
  end

  # Optimized sgn0 implementation
  def sgn0(fqp) do
    {sign, _zero} =
      Enum.reduce(fqp.coeffs, {0, 1}, fn x_i, {sign, zero} ->
        sign_i = rem(x_i, 2)
        zero_i = x_i == 0
        {sign || (zero && sign_i), zero && zero_i}
      end)

    sign
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
            fq2_modulus_coeffs: "FQ2_modulus_coeffs_type"

  def parent(), do: OptimizedFQP

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if not FieldMath.fq2_modulus_coeffs(fqp) do
      raise "FQ2 Modulus Coeffs haven't been specified"
    end

    parent().new(coeffs, fqp.fq2_modulus_coeffs)
  end

  # Optimized sgn0 implementation for m = 2
  def sgn0(fq2) do
    [x_0, x_1] = fq2.coeffs
    sign_0 = rem(x_0, 2)
    zero_0 = x_0 == 0
    sign_1 = rem(x_1, 2)
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

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if not FieldMath.fq12_modulus_coeffs(fqp) do
      raise "FQ12 Modulus Coeffs haven't been specified"
    end

    parent().new(coeffs, fqp.fq12_modulus_coeffs)
  end
end
