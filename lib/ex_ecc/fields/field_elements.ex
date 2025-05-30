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

  def add(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n + fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def mul(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n * fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def sub(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n - fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def field_div(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    inv_fq2_n = Utils.prime_field_inv(fq2.n, fq1.field_modulus)
    val = rem(fq1.n * inv_fq2_n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def negate(fq), do: neg(fq)

  def pow(fq_base = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        %__MODULE__{n: 1, field_modulus: fq_base.field_modulus}

      exponent == 1 ->
        fq_base

      rem(exponent, 2) == 0 ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        mul(half_pow, half_pow)

      true ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        mul(mul(half_pow, half_pow), fq_base)
    end
  end

  def neg(fq = %__MODULE__{}) do
    %__MODULE__{n: rem(-fq.n, fq.field_modulus), field_modulus: fq.field_modulus}
  end

  def equal?(fq1 = %__MODULE__{}, fq2_val) do
    cond do
      is_integer(fq2_val) ->
        fq1.n == fq2_val

      is_map(fq2_val) and Map.has_key?(fq2_val, :n) ->
        fq1.n == fq2_val.n and fq1.field_modulus == fq2_val.field_modulus

      true ->
        false
    end
  end

  # Comparison functions for ordering (Python's @total_ordering)
  def compare(fq1 = %__MODULE__{}, fq2_val) do
    fq2_n = if is_integer(fq2_val), do: fq2_val, else: ensure_fq(fq2_val, fq1.field_modulus).n

    cond do
      fq1.n > fq2_n -> 1
      fq1.n < fq2_n -> -1
      true -> 0
    end
  end

  # Helper to ensure a value is an FQ element for operations
  defp ensure_fq(val, field_modulus) when is_integer(val) do
    new(val, field_modulus)
  end

  defp ensure_fq(fq = %__MODULE__{}, field_modulus) do
    if fq.field_modulus != field_modulus do
      raise "Cannot operate on FQ elements from different fields without explicit conversion."
    end

    fq
  end

  defp ensure_fq(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ element, got: #{inspect(other)}"
  end

  @spec one(integer) :: t_fq
  def one(field_modulus) when is_integer(field_modulus) do
    %__MODULE__{n: 1, field_modulus: field_modulus}
  end

  @spec zero(integer) :: t_fq
  def zero(field_modulus) when is_integer(field_modulus) do
    %__MODULE__{n: 0, field_modulus: field_modulus}
  end

  # Add aliases for backward compatibility
  def multiply(fq1, fq2), do: mul(fq1, fq2)
  def divide(fq1, fq2), do: field_div(fq1, fq2)
  def div(fq1, fq2), do: field_div(fq1, fq2)
  def eq(fq1, fq2), do: equal?(fq1, fq2)

  def inv(fq = %__MODULE__{}) do
    if fq.n == 0 do
      raise "Cannot invert zero element"
    end

    pow(fq, fq.field_modulus - 2)
  end

  def conjugate(fq = %__MODULE__{}) do
    # For FQ, conjugation is identity
    fq
  end

  def frobenius(fq = %__MODULE__{}, _power) do
    # For FQ, Frobenius map is identity
    fq
  end
end

# FQP - Elements in polynomial extension fields
# This will be a struct containing a list of FQ elements (coeffs)
# and modulus_coeffs (also FQ elements or integers representing them).
# The `degree` will be the length of modulus_coeffs.

defmodule ExEcc.Fields.FQP do
  alias ExEcc.Utils
  alias ExEcc.Fields.FQ

  defstruct coeffs: [], modulus_coeffs: [], degree: 0, field_modulus: nil

  def new(coeffs, modulus_coeffs, field_modulus)
      when is_list(coeffs) and is_list(modulus_coeffs) and is_integer(field_modulus) do
    if Enum.any?(coeffs, &is_nil/1) do
      raise "FQP.new_fqp: One of the element coefficients is nil: #{inspect(coeffs)}"
    end

    if Enum.any?(modulus_coeffs, &is_nil/1) do
      raise "FQP.new_fqp: One of the modulus coefficients is nil: #{inspect(modulus_coeffs)}"
    end

    degree = length(modulus_coeffs)

    padded_coeffs =
      if length(coeffs) < degree do
        coeffs ++ List.duplicate(FQ.zero(field_modulus), degree - length(coeffs))
      else
        Enum.take(coeffs, degree)
      end

    fq_coeffs = Enum.map(padded_coeffs, &FQ.new(&1, field_modulus))
    fq_modulus_coeffs = Enum.map(modulus_coeffs, &FQ.new(&1, field_modulus))

    %__MODULE__{
      coeffs: fq_coeffs,
      modulus_coeffs: fq_modulus_coeffs,
      degree: degree,
      field_modulus: field_modulus
    }
  end

  def add(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
    if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
      raise "Cannot add FQP elements from different fields or degrees"
    end

    new_coeffs = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &FQ.add(&1, &2))
    %__MODULE__{fqp1 | coeffs: new_coeffs}
  end

  def sub(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
    if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
      raise "Cannot subtract FQP elements from different fields or degrees"
    end

    new_coeffs = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &FQMain.sub(&1, &2))
    %__MODULE__{fqp1 | coeffs: new_coeffs}
  end

  def mul(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
    if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
      raise "Cannot multiply FQP elements from different fields or degrees"
    end

    # Polynomial multiplication
    product_coeffs =
      Enum.reduce(
        0..(fqp1.degree - 1),
        List.duplicate(FQMain.zero(fqp1.field_modulus), 2 * fqp1.degree - 1),
        fn i, acc ->
          Enum.reduce(0..(fqp2.degree - 1), acc, fn j, acc2 ->
            idx = i + j
            current = Enum.at(acc2, idx)

            new_val =
              FQMain.add(current, FQMain.mul(Enum.at(fqp1.coeffs, i), Enum.at(fqp2.coeffs, j)))

            List.replace_at(acc2, idx, new_val)
          end)
        end
      )

    # Reduce modulo the field polynomial
    reduced_coeffs = reduce_polynomial(product_coeffs, fqp1.modulus_coeffs, fqp1.field_modulus)
    %__MODULE__{fqp1 | coeffs: reduced_coeffs}
  end

  def divide(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
    if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
      raise "Cannot divide FQP elements from different fields or degrees"
    end

    # Division in a field extension is multiplication by the inverse
    mul(fqp1, inv(fqp2))
  end

  # Add alias for div
  def div(fqp1, fqp2), do: divide(fqp1, fqp2)

  def inv(fqp = %__MODULE__{}) do
    if is_zero(fqp) do
      raise "Cannot invert zero FQP element"
    end

    if fqp.degree == 1 do
      # This is effectively an FQ element
      inv_coeff0 = FQMain.pow(Enum.at(fqp.coeffs, 0), fqp.field_modulus - 2)
      %__MODULE__{fqp | coeffs: [inv_coeff0]}
    else
      # Use Fermat's Little Theorem for extension fields: a^(q^d - 2)
      q_power_d = :math.pow(fqp.field_modulus, fqp.degree) |> round()
      exponent = q_power_d - 2
      pow(fqp, exponent)
    end
  end

  def pow(fqp_base = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        one(fqp_base.field_modulus, fqp_base.degree, fqp_base.modulus_coeffs)

      exponent == 1 ->
        fqp_base

      rem(exponent, 2) == 0 ->
        half_pow = pow(fqp_base, Kernel.div(exponent, 2))
        mul(half_pow, half_pow)

      true ->
        half_pow = pow(fqp_base, Kernel.div(exponent, 2))
        mul(mul(half_pow, half_pow), fqp_base)
    end
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
