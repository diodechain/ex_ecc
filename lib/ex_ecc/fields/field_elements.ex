defmodule ExEcc.Fields.FQ do
  alias ExEcc.Utils
  # alias ExEcc.Typing # Assuming types will be defined here

  # Python's TypeVar is for generic types. Elixir uses different patterns for this,
  # often relying on protocols or behaviours, or simply by passing module names as arguments
  # to achieve polymorphism. For now, we'll define structs and functions that operate on them.

  # @type int_or_fq :: integer | ExEcc.Fields.FieldElements.FQ.t()
  # Placeholder for the FQ struct type, which will be defined below.

  @doc """
  Represents an element in a prime field FQ.
  `n` is the integer value of the element.
  `field_modulus` is the modulus of the field.

  In Elixir, we'd typically pass field_modulus to functions or make it part of a
  module that represents a specific field (e.g., ExEcc.Fields.BN128.FQ).
  For a direct translation, we can include it in the struct, but this is not idiomatic.
  A better approach would be to define field-specific modules with their own FQ structs
  or functions that operate on integers directly with an explicit modulus.
  """
  defstruct n: 0, field_modulus: nil

  @type t_fq :: %__MODULE__{n: integer, field_modulus: integer}

  @doc """
  Creates a new FQ element. The `field_modulus` must be set before operations.
  This function would typically be part of a specific field module that provides the modulus.
  Example: `MyField.FQ.new(value)`
  """
  def new(val, field_modulus) when is_integer(val) and is_integer(field_modulus) do
    %__MODULE__{n: rem(val, field_modulus), field_modulus: field_modulus}
  end

  def new(fq_element = %__MODULE__{}, field_modulus) when is_integer(field_modulus) do
    if fq_element.field_modulus == field_modulus do
      fq_element
    else
      %__MODULE__{n: rem(fq_element.n, field_modulus), field_modulus: field_modulus}
    end
  end

  # --- FQ Operations ---
  # Note: In Elixir, operations are typically defined as functions in a module,
  # not as methods on a struct. The `total_ordering` from Python would be achieved
  # by implementing comparison functions and potentially a custom `compare/2`.

  def add(fq1, other) do
    on =
      case other do
        %{n: n} ->
          n

        n when is_integer(n) ->
          n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq1.n + on, fq1.field_modulus)
  end

  def mul(fq1, other) do
    on =
      case other do
        %{n: n} ->
          n

        n when is_integer(n) ->
          n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq1.n * on, fq1.field_modulus)
  end

  def sub(fq1, other) do
    on =
      case other do
        %{n: n} ->
          n

        n when is_integer(n) ->
          n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq1.n - on, fq1.field_modulus)
  end

  def div(fq1, other) do
    on =
      case other do
        %{n: n} ->
          n

        n when is_integer(n) ->
          n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    FieldMath.rem(fq1.n * Utils.prime_field_inv(on, fq1.field_modulus), fq1.field_modulus)
  end

  def pow(fq, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        %type{} = fq
        type.new(1)

      exponent == 1 ->
        %type{} = fq
        type.new(fq.n)

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
        raise "Expected an int or FQ object, but got #{inspect(other)}"
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
        %{n: n} ->
          fq.n == n

        n when is_integer(n) ->
          fq.n == n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    fq.n < on
  end

  @spec one(integer) :: t_fq
  def one(cls) do
    cls.new(1)
  end

  @spec zero(integer) :: t_fq
  def zero(cls) do
    cls.new(0)
  end

  # Comparison functions for ordering (Python's @total_ordering)
  def compare(fq1, other) do
    on =
      case other do
        %{n: n} ->
          n

        n when is_integer(n) ->
          n
          raise "Expected an int or FQ object, but got #{inspect(other)}"
      end

    cond do
      fq1.n > fq2_n -> 1
      fq1.n < fq2_n -> -1
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

  def new(fqp \\ %__MODULE__{}, coeffs, modulus_coeffs, field_modulus) do
    if not Map.has_key?(fqp, :field_modulus) do
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

    FieldMath.type(fqp).new(for x, y in zip(fqp.coeffs, other.coeffs), do: FieldMath.add(x, y))
  end

  def sub(fqp, other) do
    if not FieldMath.isinstance(fqp, other) do
      raise "Expected an FQP object, but got object of type #{FieldMath.type(fqp)}"
    end

    FieldMath.type(fqp).new(for x, y in zip(fqp.coeffs, other.coeffs), do: FieldMath.sub(x, y))
  end

  def mul(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.type(fqp).new(for c in fqp.coeffs, do: FieldMath.mul(c, other))

      FieldMath.isinstance(other, FQP) ->
        b = for i in 0..(fqp.degree * 2 - 1), do: FieldMath.corresponding_fq_class(fqp).new(0)
        b = Map.new(b, Enum.with_index(b, fn x, i -> {i, x} end))

        b =
          for i in 0..(fqp.degree - 1), j in 0..(fqp.degree - 1),
            do:
              {i, j}
              |> Enum.reduce(b, fn {i, j}, acc ->
                Map.put(
                  acc,
                  i + j,
                  FieldMath.add(
                    Map.get(acc, i + j),
                    FieldMath.mul(fqp.coeffs[i], other.coeffs[j])
                  )
                )
              end)

        b =
          while(b, length(b) > fqp.degree, fn acc ->
            {exp, top} = {length(acc) - fqp.degree - 1, List.pop_at(acc, 0)}

            for i in 0..(fqp.degree - 1),
              do:
                Map.put(
                  acc,
                  exp + i,
                  FieldMath.sub(Map.get(acc, exp + i), FieldMath.mul(top, fqp.modulus_coeffs[i]))
                )
          end)

        FieldMath.type(fqp).new(b)

      true ->
        raise "Expected an int or FQ object or FQP object, but got object of type #{FieldMath.type(other)}"
    end
  end

  def div(fqp, other) do
    cond do
      FieldMath.isinstance(other, :int_types_or_FQ) ->
        FieldMath.type(fqp).new(for c in fqp.coeffs, do: FieldMath.div(c, other))

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

    {lm, low, hm, high} =
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
          for i in 0..FieldMath.degree(fqp), j in 0..(FieldMath.degree(fqp) - i),
            do:
              {i, j}
              |> Enum.reduce({nm, new}, fn {i, j}, acc ->
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

        {lm, low, hm, high} = {nm, new, lm, low}
      end

    FieldMath.type(fqp).new(Enum.take(lm, fqp.degree) / FieldMath.int(low[0]))
  end

  def neg(fqp = %__MODULE__{}) do
    new_coeffs = Enum.map(fqp.coeffs, &FQMain.neg/1)
    %__MODULE__{fqp | coeffs: new_coeffs}
  end

  def neg(fqp = %__MODULE__{}) do
    new_coeffs = Enum.map(fqp.coeffs, &FQMain.neg/1)
    %__MODULE__{fqp | coeffs: new_coeffs}
  end

  def equal?(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
    if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
      false
    else
      Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &FQMain.equal?/2)
      |> Enum.all?()
    end
  end

  def is_zero(fqp = %__MODULE__{}) do
    Enum.all?(fqp.coeffs, &FQMain.equal?(&1, 0))
  end

  def is_one(fqp = %__MODULE__{}) do
    [first | rest] = fqp.coeffs
    FQMain.equal?(first, 1) and Enum.all?(rest, &FQMain.equal?(&1, 0))
  end

  def is_one(coeffs) when is_list(coeffs) do
    [first | rest] = coeffs
    FQMain.equal?(first, 1) and Enum.all?(rest, &FQMain.equal?(&1, 0))
  end

  def one(field_modulus, degree, modulus_coeffs) do
    coeffs = [FQMain.one(field_modulus) | List.duplicate(FQMain.zero(field_modulus), degree - 1)]

    %__MODULE__{
      coeffs: coeffs,
      modulus_coeffs: modulus_coeffs,
      degree: degree,
      field_modulus: field_modulus
    }
  end

  def zero(field_modulus, degree, modulus_coeffs) do
    coeffs = List.duplicate(FQMain.zero(field_modulus), degree)

    %__MODULE__{
      coeffs: coeffs,
      modulus_coeffs: modulus_coeffs,
      degree: degree,
      field_modulus: field_modulus
    }
  end

  # Helper functions for polynomial operations
  defp reduce_polynomial(coeffs, modulus_coeffs, field_modulus) do
    degree = length(modulus_coeffs)

    if length(coeffs) < degree do
      # Pad with FQ zeros to ensure correct length
      coeffs ++ List.duplicate(FQMain.zero(field_modulus), degree - length(coeffs))
    else
      Enum.take(coeffs, degree)
    end
  end

  def negate(fqp), do: neg(fqp)
  def multiply(fqp1, fqp2), do: mul(fqp1, fqp2)
  def eq(fqp1, fqp2), do: equal?(fqp1, fqp2)

  def conjugate(fqp = %__MODULE__{}) do
    # For FQP, conjugation is applying the Frobenius map n times
    frobenius(fqp, fqp.degree)
  end

  def frobenius(fqp = %__MODULE__{}, _power) do
    # Frobenius map: (a + b*w)^p = a^p + b^p*w^p
    # For FQP, w^p = w^(p mod n)
    p = fqp.field_modulus

    new_coeffs =
      Enum.map(fqp.coeffs, fn coeff ->
        FQMain.pow(coeff, p)
      end)

    %__MODULE__{fqp | coeffs: new_coeffs}
  end
end

# defmodule FQP

# FQ2 - Quadratic extension field
# FQ2 is FQP where degree = 2 and modulus_coeffs are fixed (e.g., u^2 - beta = 0)
defmodule ExEcc.Fields.FQ2 do
  alias ExEcc.Fields.FQP

  @type t_fq2 :: FQP.t_fqp()

  def new_fq2(coeffs, field_modulus) when is_list(coeffs) and is_integer(field_modulus) do
    # For FQ2, the modulus polynomial is x^2 + 1 = 0 for BLS12-381
    # The modulus coefficients are [-1, 0] for x^2 + 1 = 0
    # Using -1 as the non-residue for BLS12-381
    modulus_coeffs = [-1, 0]
    FQP.new_fqp(coeffs, modulus_coeffs, field_modulus)
  end

  def add(a, b), do: FQP.add(a, b)
  def sub(a, b), do: FQP.sub(a, b)
  def mul(a, b), do: FQP.mul(a, b)
  def divide(a, b), do: FQP.divide(a, b)
  def pow(a, n), do: FQP.pow(a, n)
  def neg(a), do: FQP.neg(a)
  def equal?(a, b), do: FQP.equal?(a, b)
  def is_zero(a), do: FQP.is_zero(a)
  def is_one(a), do: FQP.is_one(a)

  @doc """
  Creates a new FQ2 element representing one in the field.
  """
  def one(field_modulus) when is_integer(field_modulus) do
    FQP.one(field_modulus, 2, [-1, 0])
  end

  def zero(field_modulus) do
    FQP.zero(field_modulus, 2, [-1, 0])
  end

  def sgn0(fq2 = %FQP{}) do
    [x0, x1] = fq2.coeffs
    sign_0 = rem(x0.n, 2)
    zero_0 = x0.n == 0
    sign_1 = rem(x1.n, 2)
    rem(sign_0 + if(zero_0, do: sign_1, else: 0), 2)
  end
end

# FQ12 - Twelfth extension field
# FQ12 is FQP where degree = 12 and modulus_coeffs are fixed (e.g., u^12 - beta = 0)
defmodule ExEcc.Fields.FQ12 do
  alias ExEcc.Fields.FQP

  @type t_fq12 :: FQP.t_fqp()

  def new_fq12(coeffs, field_modulus) when is_list(coeffs) and is_integer(field_modulus) do
    # For BLS12-381, FQ12 is constructed as a tower of extensions:
    # 1. FQ2: x^2 + 1 = 0 (non-residue -1)
    # 2. FQ6: v^3 - u = 0 where u is the non-residue in FQ2
    # 3. FQ12: w^2 - v = 0 where v is the non-residue in FQ6
    # The modulus polynomial for FQ12 is w^2 - v = 0
    # The modulus coefficients are [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] for w^2 - v = 0
    # Using the correct non-residue for BLS12-381
    modulus_coeffs = List.duplicate(0, 11) ++ [1]
    FQP.new_fqp(coeffs, modulus_coeffs, field_modulus)
  end

  def add(a, b), do: FQP.add(a, b)
  def sub(a, b), do: FQP.sub(a, b)
  def mul(a, b), do: FQP.mul(a, b)
  def divide(a, b), do: FQP.divide(a, b)
  def pow(a, n), do: FQP.pow(a, n)
  def neg(a), do: FQP.neg(a)
  def equal?(a, b), do: FQP.equal?(a, b)
  def is_zero(a), do: FQP.is_zero(a)
  def is_one(a), do: FQP.is_one(a)

  def one(field_modulus) do
    FQP.one(field_modulus, 12, List.duplicate(0, 11) ++ [1])
  end

  def zero(field_modulus) do
    FQP.zero(field_modulus, 12, List.duplicate(0, 11) ++ [1])
  end

  def sgn0(fq12 = %FQP{}) do
    Enum.with_index(fq12.coeffs)
    |> Enum.reduce(0, fn {c, i}, acc ->
      (acc + rem(c.n, 2) * :math.pow(2, i)) |> round()
    end)
    |> rem(2)
  end
end
