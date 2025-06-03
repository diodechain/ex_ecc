defmodule ExEcc.Fields.FQ do
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

    FieldMath.mod_int(fq.n + on, FieldMath.field_modulus(fq))
  end

  def mul(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.mod_int(fq.n * on, FieldMath.field_modulus(fq))
  end

  def sub(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.mod_int(fq.n - on, FieldMath.field_modulus(fq))
  end

  def div(fq, other) do
    on =
      case other do
        %{n: n} -> n
        n when is_integer(n) -> n
        _ -> raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    ret = FieldMath.mod_int(
      fq.n * Utils.prime_field_inv(on, FieldMath.field_modulus(fq)),
      FieldMath.field_modulus(fq)
    )

    FieldMath.new(fq, ret)
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
    cls.new(1)
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
  alias ExEcc.FieldMath
  import While

  defstruct coeffs: {}, modulus_coeffs: {}, degree: 0, field_modulus: nil

  def new(fqp \\ %__MODULE__{}, coeffs, modulus_coeffs) do
    if FieldMath.field_modulus(fqp) == nil do
      raise "Field Modulus hasn't been specified"
    end

    if tuple_size(coeffs) != tuple_size(modulus_coeffs) do
      raise "coeffs and modulus_coeffs aren't of the same length"
    end

    # Encoding all coefficients in the corresponding type FQ
    fqp_corresponding_fq_class =
      FieldMath.type("FQP_corresponding_FQ_class", FQ,
        field_modulus: FieldMath.field_modulus(fqp)
      )

    coeffs =
      Tuple.to_list(coeffs)
      |> Enum.map(fn c -> FieldMath.new(fqp_corresponding_fq_class, c) end)
      |> List.to_tuple()

    # The coefficients of the modulus, without the leading [1]
    modulus_coeffs =
      Tuple.to_list(modulus_coeffs)
      |> Enum.map(fn c -> FieldMath.new(fqp_corresponding_fq_class, c) end)
      |> List.to_tuple()

    # The degree of the extension field
    %{
      fqp
      | degree: tuple_size(modulus_coeffs),
        coeffs: coeffs,
        modulus_coeffs: modulus_coeffs,
        corresponding_fq_class: fqp_corresponding_fq_class
    }
  end

  def add(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(FieldMath.coeffs(fqp), FieldMath.coeffs(other)),
          do: FieldMath.add(x, y)
    )
  end

  def sub(fqp, other) do
    if not FieldMath.isinstance(other, FieldMath.type(fqp)) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(
      for {x, y} <- Enum.zip(FieldMath.coeffs(fqp), FieldMath.coeffs(other)),
          do: FieldMath.sub(x, y)
    )
  end

  def mul(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.coeffs(fqp)
        |> Tuple.to_list()
        |> Enum.map(&FieldMath.mul(&1, other))
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      FieldMath.isinstance(other, ExEcc.Fields.FQP) ->
        b =
          List.duplicate(
            FieldMath.new(FieldMath.corresponding_fq_class(fqp), 0),
            FieldMath.degree(fqp) * 2 - 1
          )

        for i <- 0..(FieldMath.degree(fqp) - 1), j <- 0..(FieldMath.degree(fqp) - 1) do
          {i, j}
        end
        |> Enum.reduce(b, fn {i, j}, b ->
          List.update_at(
            b,
            i + j,
            fn x ->
              FieldMath.add(
                x,
                FieldMath.mul(FieldMath.coeffs(fqp, i), FieldMath.coeffs(other, j))
              )
            end
          )
        end)
        |> reduce_while(fn b ->
          if length(b) > FieldMath.degree(fqp) do
            exp = length(b) - FieldMath.degree(fqp) - 1
            [top | b] = b

            b =
              Enum.reduce(0..(FieldMath.degree(fqp) - 1), b, fn i, b ->
                List.update_at(
                  b,
                  exp + i,
                  fn x ->
                    FieldMath.sub(
                      x,
                      FieldMath.mul(top, FieldMath.modulus_coeffs(fqp, i))
                    )
                  end
                )
              end)

            {:cont, b}
          else
            {:halt, b}
          end
        end)
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      true ->
        raise "Expected an int or FQ object or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def div(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.coeffs(fqp)
        |> Tuple.to_list()
        |> Enum.map(&FieldMath.div(&1, other))
        |> List.to_tuple()
        |> FieldMath.type(fqp).new()

      FieldMath.isinstance(other, FieldMath.type(fqp)) ->
        FieldMath.mul(fqp, FieldMath.inv(other))

      true ->
        raise "Expected an int or FQ object or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def pow(fqp, other) do
    cond do
      other == 0 ->
        List.to_tuple([1] ++ List.duplicate(0, FieldMath.degree(fqp) - 1))
        |> FieldMath.type(fqp).new()

      other == 1 ->
        FieldMath.type(fqp).new(FieldMath.coeffs(fqp))

      rem(other, 2) == 0 ->
        FieldMath.pow(FieldMath.mul(fqp, fqp), Kernel.div(other, 2))

      true ->
        FieldMath.pow(FieldMath.mul(fqp, fqp), Kernel.div(other, 2)) |> FieldMath.mul(fqp)
    end
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
        IO.inspect(Utils.deg(low), label: "deg(low)")
        if Utils.deg(low) > 0 do
          r = Utils.poly_rounded_div(high, low)
          r = r ++ List.duplicate(0, FieldMath.degree(fqp) + 1 - length(r))
          nm = hm
          new = high

          cond do
            length(lm) != FieldMath.degree(fqp) + 1 ->
              raise "Length of lm is not #{FieldMath.degree(fqp) + 1}"

            length(hm) != FieldMath.degree(fqp) + 1 ->
              raise "Length of hm is not #{FieldMath.degree(fqp) + 1}"

            length(nm) != FieldMath.degree(fqp) + 1 ->
              raise "Length of nm is not #{FieldMath.degree(fqp) + 1}"

            length(low) != FieldMath.degree(fqp) + 1 ->
              raise "Length of low is not #{FieldMath.degree(fqp) + 1}"

            length(high) != FieldMath.degree(fqp) + 1 ->
              raise "Length of high is not #{FieldMath.degree(fqp) + 1}"

            length(new) != FieldMath.degree(fqp) + 1 ->
              raise "Length of new is not #{FieldMath.degree(fqp) + 1}"

            true ->
              :ok
          end

          {nm, new} =
            for(i <- 0..FieldMath.degree(fqp), j <- 0..(FieldMath.degree(fqp) - i), do: {i, j})
            |> Enum.reduce({nm, new}, fn {i, j}, {nm, new} ->
              nm =
                List.update_at(
                  nm,
                  i + j,
                  fn x ->
                    FieldMath.sub(x, FieldMath.mul(Enum.at(lm, i), Enum.at(r, j)))
                  end
                )

              new =
                List.update_at(
                  new,
                  i + j,
                  fn x ->
                    FieldMath.sub(x, FieldMath.mul(Enum.at(low, i), Enum.at(r, j)))
                  end
                )

              {nm, new}
            end)

          {:cont, {nm, new, lm, low}}
        else
          {:halt, {lm, low, hm, high}}
        end
      end)

    FieldMath.type(fqp).new(
      List.to_tuple(Enum.take(lm, FieldMath.degree(fqp)) |> FieldMath.div(trunc(Enum.at(low, 0))))
    )
  end

  def repr(fqp) do
    inspect(FieldMath.coeffs(fqp))
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
    cls.new(List.to_tuple([1] ++ List.duplicate(0, FieldMath.degree(cls) - 1)))
  end

  def zero(cls) do
    cls.new(List.to_tuple(List.duplicate(0, FieldMath.degree(cls))))
  end
end

defmodule ExEcc.Fields.FQ2 do
  @doc """
  The quadratic extension field
  """
  alias ExEcc.Fields.FQP
  alias ExEcc.FieldMath

  defstruct degree: 2,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq2_modulus_coeffs: "FQ2_modulus_coeffs_type"

  def parent(), do: FQP
  def degree(), do: 2

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if FieldMath.fq2_modulus_coeffs(fqp) == nil do
      raise "FQ2 Modulus Coeffs haven't been specified"
    end

    parent().new(fqp, coeffs, FieldMath.fq2_modulus_coeffs(fqp))
  end
end

defmodule ExEcc.Fields.FQ12 do
  @moduledoc """
  The 12th-degree extension field
  """

  alias ExEcc.Fields.FQP
  alias ExEcc.FieldMath

  defstruct degree: 12,
            modulus_coeffs: nil,
            field_modulus: nil,
            fq12_modulus_coeffs: "FQ12_modulus_coeffs_type"

  def parent(), do: FQP
  def degree(), do: 12

  def new(fqp \\ %__MODULE__{}, coeffs) do
    if FieldMath.fq12_modulus_coeffs(fqp) == nil do
      raise "FQ12 Modulus Coeffs haven't been specified"
    end

    parent().new(fqp, coeffs, FieldMath.fq12_modulus_coeffs(fqp))
  end
end
