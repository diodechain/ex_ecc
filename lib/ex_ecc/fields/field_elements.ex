defmodule ExEcc.Fields.FQ do
  alias ExEcc.Utils
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
      %{n: n} -> fq1.n == n
      n when is_integer(n) -> fq1.n == n
      _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
    end
  end

  def ne(fq, other) do
    FieldMath.neg(fq) |> FieldMath.eq(other)
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
        %{n: n} -> fq.n < n
        n when is_integer(n) -> fq.n < n
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

defmodule ExEcc.Fields.FQP do
  @moduledoc """
  A module for elements in polynomial extension fields
  """

  alias ExEcc.Utils
  alias ExEcc.Fields.FQ
  import While

  defstruct coeffs: [], modulus_coeffs: [], degree: 0, field_modulus: nil

  def new(fqp \\ %__MODULE__{}, coeffs, modulus_coeffs) do
    if not FieldMath.field_modulus(fqp) do
      raise "Field Modulus hasn't been specified"
    end

    if length(coeffs) != length(modulus_coeffs) do
      raise "coeffs and modulus_coeffs aren't of the same length"
    end

    # Encoding all coefficients in the corresponding type FQ
    fqp_corresponding_fq_class =
      FieldMath.type("FQP_corresponding_FQ_class", FQ, field_modulus: fqp.field_modulus)

    coeffs = Enum.map(coeffs, fn c -> fqp_corresponding_fq_class.new(c) end)
    # The coefficients of the modulus, without the leading [1]
    modulus_coeffs = Enum.map(modulus_coeffs, fn c -> fqp_corresponding_fq_class.new(c) end)
    # The degree of the extension field
    %{
      fqp
      | degree: length(modulus_coeffs),
        coeffs: coeffs,
        modulus_coeffs: modulus_coeffs,
        corresponding_fq_class: fqp_corresponding_fq_class
    }
  end

  def add(fqp, other) do
    if not FieldMath.isinstance(fqp, other) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(fqp.coeffs, other.coeffs), do: FieldMath.add(x, y)
    )
  end

  def sub(fqp, other) do
    if not FieldMath.isinstance(fqp, other) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(fqp.coeffs, other.coeffs), do: FieldMath.sub(x, y)
    )
  end

  def mul(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.type(fqp).new(for c <- FieldMath.coeffs(fqp), do: FieldMath.mul(c, other))

      FieldMath.isinstance(other, FQP) ->
        b =
          List.duplicate(
            FieldMath.corresponding_fq_class(fqp).new(0),
            FieldMath.degree(fqp) * 2 - 1
          )

        b =
          for(
            i <- 0..(FieldMath.degree(fqp) - 1),
            j <- 0..(FieldMath.degree(fqp) - 1),
            do: {i, j}
          )
          |> Enum.reduce(b, fn {i, j}, b ->
            Map.put(
              b,
              i + j,
              FieldMath.add(
                Map.get(b, i + j),
                FieldMath.mul(FieldMath.coeffs(fqp)[i], FieldMath.coeffs(other)[j])
              )
            )
          end)

        b =
          reduce_while(b, fn b ->
            if length(b) > FieldMath.degree(fqp) do
              {exp, top} = {length(b) - FieldMath.degree(fqp) - 1, List.pop_at(b, 0)}

              b =
                Enum.reduce(0..(FieldMath.degree(fqp) - 1), b, fn i, b ->
                  Map.put(
                    b,
                    exp + i,
                    FieldMath.sub(
                      Map.get(b, exp + i),
                      FieldMath.mul(top, FieldMath.modulus_coeffs(fqp)[i])
                    )
                  )
                end)

              {:cont, b}
            else
              {:halt, b}
            end
          end)

        FieldMath.type(fqp).new(b)

      true ->
        raise "Expected an int or FQ object or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def div(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.type(fqp).new(for c <- fqp.coeffs, do: FieldMath.div(c, other))

      FieldMath.isinstance(other, FQP) ->
        FieldMath.mul(fqp, FieldMath.inv(other))

      true ->
        raise "Expected an int or FQ object or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def pow(fqp, other) do
    cond do
      other == 0 ->
        FieldMath.type(fqp).new([1] ++ ([0] * (fqp.degree - 1)))

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
      Tuple.to_list(FieldMath.coeffs(fqp)) ++ [0],
      Tuple.to_list(FieldMath.modulus_coeffs(fqp)) ++ [1]
    }

    {lm, low, _hm, _high} =
      while Utils.deg(low) do
        r = Utils.poly_rounded_div(high, low)
        r = r ++ List.duplicate(0, FieldMath.degree(fqp) + 1 - length(r))
        nm = Enum.map(hm, & &1)
        new = Enum.map(high, & &1)

        cond do
          length(lm) != FieldMath.degree(fqp) + 1 ->
            raise "Length of lm is not #{fqp.degree + 1}"

          length(hm) != FieldMath.degree(fqp) + 1 ->
            raise "Length of hm is not #{fqp.degree + 1}"

          length(nm) != FieldMath.degree(fqp) + 1 ->
            raise "Length of nm is not #{fqp.degree + 1}"

          length(low) != FieldMath.degree(fqp) + 1 ->
            raise "Length of low is not #{fqp.degree + 1}"

          length(high) != FieldMath.degree(fqp) + 1 ->
            raise "Length of high is not #{fqp.degree + 1}"

          length(new) != FieldMath.degree(fqp) + 1 ->
            raise "Length of new is not #{fqp.degree + 1}"

          true ->
            :ok
        end

        {nm, new} =
          for(i <- 0..FieldMath.degree(fqp), j <- 0..(FieldMath.degree(fqp) - i), do: {i, j})
          |> Enum.reduce({nm, new}, fn {i, j}, {nm, new} ->
            nm =
              Map.put(
                nm,
                i + j,
                FieldMath.sub(Map.get(nm, i + j), FieldMath.mul(lm[i], r[j]))
              )

            new =
              Map.put(
                new,
                i + j,
                FieldMath.sub(Map.get(new, i + j), FieldMath.mul(low[i], r[j]))
              )

            {nm, new}
          end)

        {_lm, _low, _hm, _high} = {nm, new, lm, low}
      end

    FieldMath.type(fqp).new(Enum.take(lm, FieldMath.degree(fqp)) / FieldMath.int(low[0]))
  end

  def repr(fqp) do
    inspect(fqp.coeffs)
  end

  def eq(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(other)}"
    end

    Enum.zip(FieldMath.coeffs(fqp), FieldMath.coeffs(other))
    |> Enum.all?(fn {c1, c2} -> c1 == c2 end)
  end

  def ne(fqp, other) do
    not FieldMath.eq(fqp, other)
  end

  def neg(fqp) do
    FieldMath.type(fqp).new(Enum.map(FieldMath.coeffs(fqp), &(-&1)))
  end

  def one(cls) do
    cls.new([1] ++ List.duplicate(0, cls.degree - 1))
  end

  def zero(cls) do
    cls.new(List.duplicate(0, cls.degree))
  end
end

defmodule ExEcc.Fields.FQ2 do
  @doc """
  The quadratic extension field
  """
  alias ExEcc.Fields.FQP

  defstruct degree: 2,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq2_modulus_coeffs: "FQ2_modulus_coeffs_type"

  def parent(), do: FQP

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if not FieldMath.fq2_modulus_coeffs(fqp) do
      raise "FQ2 Modulus Coeffs haven't been specified"
    end

    parent().new(coeffs, fqp.fq2_modulus_coeffs)
  end
end

defmodule ExEcc.Fields.FQ12 do
  @moduledoc """
  The 12th-degree extension field
  """

  alias ExEcc.Fields.FQP

  defstruct degree: 12,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq12_modulus_coeffs: "FQ12_modulus_coeffs_type"

  def parent(), do: FQP

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if not FieldMath.fq12_modulus_coeffs(fqp) do
      raise "FQ12 Modulus Coeffs haven't been specified"
    end

    parent().new(coeffs, fqp.fq12_modulus_coeffs)
  end
end
