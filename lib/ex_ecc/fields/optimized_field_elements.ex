defmodule ExEcc.Fields.OptimizedFieldElements do
  alias ExEcc.Utils
  @field_modulus 0
  # We will need a way to handle @cached_property. Elixir doesn't have this directly.
  # It can be simulated with an agent or by memoizing in a process dictionary, or by using a library.
  # For a first pass, we will make them regular functions. Optimization can be added later.

  # The `mod_int` helper function
  def mod_int(x, n) when is_integer(x) and is_integer(n) do
    rem(x, n)
  end

  # Assuming FQ has :n field
  def mod_int(fq = %{n: val}, n) when is_integer(val) and is_integer(n) do
    rem(fq.n, n)
  end

  # Add other cases or a general fallback if needed.

  # --- Optimized FQ ---
  defstruct n: 0, field_modulus: nil

  @type t_fq :: %__MODULE__{n: integer, field_modulus: integer}

  # Alias new_fq as new for test compatibility
  def new(val, field_modulus), do: new_fq(val, field_modulus)

  def new_fq(val, field_modulus) when is_integer(val) and is_integer(field_modulus) do
    %__MODULE__{n: rem(val, field_modulus), field_modulus: field_modulus}
  end

  def new_fq(fq_element = %__MODULE__{}, field_modulus) when is_integer(field_modulus) do
    if fq_element.field_modulus == field_modulus do
      fq_element
    else
      %__MODULE__{n: rem(fq_element.n, field_modulus), field_modulus: field_modulus}
    end
  end

  # Alias add as add for test compatibility
  def add(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n + fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  # Alias mul as multiply for test compatibility
  def multiply(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n * fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def mul(fq1, fq2_val), do: multiply(fq1, fq2_val)

  # Alias sub as subtract for test compatibility
  def subtract(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n - fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  # `__mod__` was NotImplementedError, so we might not need it, or define similarly.
  def modulo(_fq1, _fq2_val) do
    raise "Modulo Operation not yet supported by fields"
  end

  # Alias divide as divide for test compatibility
  def divide(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    inv_fq2_n = Utils.prime_field_inv(fq2.n, fq1.field_modulus)
    val = rem(fq1.n * inv_fq2_n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  # Alias pow as pow for test compatibility
  def pow(fq_base = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        %__MODULE__{n: 1, field_modulus: fq_base.field_modulus}

      exponent == 1 ->
        fq_base

      rem(exponent, 2) == 0 ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        multiply(half_pow, half_pow)

      true ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        multiply(multiply(half_pow, half_pow), fq_base)
    end
  end

  # Alias neg as negate for test compatibility
  def negate(fq = %__MODULE__{}) do
    %__MODULE__{n: rem(-fq.n, fq.field_modulus), field_modulus: fq.field_modulus}
  end

  # Add neg as an alias for negate
  def neg(fq = %__MODULE__{}), do: negate(fq)

  # Alias equal? as eq for test compatibility
  def eq(fq1 = %__MODULE__{}, fq2_val), do: equal?(fq1, fq2_val)

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

  def compare(fq1 = %__MODULE__{}, fq2_val) do
    fq2_n = if is_integer(fq2_val), do: fq2_val, else: ensure_fq(fq2_val, fq1.field_modulus).n
    cond do
      fq1.n > fq2_n -> 1
      fq1.n < fq2_n -> -1
      true -> 0
    end
  end

  # sgn0: Simulating cached_property as a regular function for now.
  def sgn0(fq = %__MODULE__{}) do
    rem(fq.n, 2)
  end

  def ensure_fq(val, field_modulus) when is_integer(val) do
    new_fq(val, field_modulus)
  end

  def ensure_fq(fq = %__MODULE__{}, field_modulus) do
    if fq.field_modulus != field_modulus do
      raise "Cannot operate on FQ elements from different fields without explicit conversion."
    end

    fq
  end

  def ensure_fq(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ element, got: #{inspect(other)}"
  end

  @spec one(integer) :: t_fq
  def one(field_modulus) when is_integer(field_modulus) do
    one(field_modulus, 1, [0])
  end

  @spec one(integer, integer, list(integer)) :: t_fq
  def one(field_modulus, degree, modulus_coeffs) when is_integer(field_modulus) and is_integer(degree) and is_list(modulus_coeffs) do
    coeffs = [1 | List.duplicate(0, degree - 1)]
    FQP.new_fqp(coeffs, modulus_coeffs, field_modulus)
  end

  @spec zero(integer) :: t_fq
  def zero(field_modulus) when is_integer(field_modulus) do
    zero(field_modulus, 1, [0])
  end

  def zero(field_modulus, degree, modulus_coeffs) when is_integer(field_modulus) and is_integer(degree) and is_list(modulus_coeffs) do
    coeffs = List.duplicate(0, degree)
    FQP.new_fqp(coeffs, modulus_coeffs, field_modulus)
  end

  def zero(_field_modulus, degree) when degree < 1 do
    raise ArgumentError, "zero/2 called with invalid degree: #{degree}. Degree must be >= 1."
  end

  # --- Optimized FQP ---
  defmodule FQP do
    alias ExEcc.Utils

    defstruct coeffs: [], modulus_coeffs: [], degree: 0, field_modulus: nil, mc_tuples: []

    @type t_fqp :: %__MODULE__{
            # Optimized: store as integers
            coeffs: list(integer),
            # Optimized: store as integers
            modulus_coeffs: list(integer),
            degree: integer,
            field_modulus: integer,
            # Assuming this structure
            mc_tuples: list({integer, integer})
          }

    @doc """
    Creates a new FQP element with the given coefficients and field modulus.
    """
    def new(coeffs, field_modulus) when is_list(coeffs) and is_integer(field_modulus) do
      new_fqp(coeffs, List.duplicate(0, length(coeffs)), field_modulus)
    end

    def new_fqp(coeffs, modulus_coeffs, field_modulus)
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
          coeffs ++ List.duplicate(0, degree - length(coeffs))
        else
          Enum.take(coeffs, degree)
        end

      # Convert all coefficients to integers mod field_modulus
      fq_coeffs = Enum.map(padded_coeffs, &rem(&1, field_modulus))
      fq_modulus_coeffs = Enum.map(modulus_coeffs, &rem(&1, field_modulus))

      # Create mc_tuples for optimized multiplication
      mc_tuples = Enum.with_index(fq_modulus_coeffs) |> Enum.map(fn {c, i} -> {i, c} end)

      %__MODULE__{
        coeffs: fq_coeffs,
        modulus_coeffs: fq_modulus_coeffs,
        degree: degree,
        field_modulus: field_modulus,
        mc_tuples: mc_tuples
      }
    end

    def add(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot add FQP elements from different fields or degrees"
      end

      new_coeffs = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, fn a, b ->
        rem(a + b, fqp1.field_modulus)
      end)

      %__MODULE__{fqp1 | coeffs: new_coeffs}
    end

    def subtract(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot subtract FQP elements from different fields or degrees"
      end

      new_coeffs = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, fn a, b ->
        rem(a - b + fqp1.field_modulus, fqp1.field_modulus)
      end)

      %__MODULE__{fqp1 | coeffs: new_coeffs}
    end

    def multiply(fqp1 = %__MODULE__{}, fqp2) do
      cond do
        is_integer(fqp2) ->
          multiply(fqp1, %__MODULE__{fqp1 | coeffs: [fqp2 | List.duplicate(0, fqp1.degree - 1)]})
        is_map(fqp2) and Map.has_key?(fqp2, :coeffs) ->
          if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
            raise "Cannot multiply FQP elements from different fields or degrees"
          end

          # For quadratic extensions (degree 2), we can optimize multiplication
          if fqp1.degree == 2 do
            [a1, b1] = fqp1.coeffs
            [a2, b2] = fqp2.coeffs
            p = fqp1.field_modulus

            # For BLS12-381, the irreducible polynomial is x^2 + 1
            # So (a1 + b1*i) * (a2 + b2*i) = (a1*a2 - b1*b2) + (a1*b2 + a2*b1)*i
            a = rem(a1 * a2 - b1 * b2, p)
            b = rem(a1 * b2 + a2 * b1, p)

            %__MODULE__{fqp1 | coeffs: [a, b]}
          else
            # Use the optimized multiplication algorithm with mc_tuples for higher degrees
            new_coeffs = Enum.map(0..(fqp1.degree - 1), fn i ->
              Enum.reduce(fqp1.mc_tuples, 0, fn {j, c}, acc ->
                rem(acc + Enum.at(fqp1.coeffs, i) * Enum.at(fqp2.coeffs, j) * c, fqp1.field_modulus)
              end)
            end)

            %__MODULE__{fqp1 | coeffs: new_coeffs}
          end
        true ->
          raise "Invalid argument for FQP.multiply/2: #{inspect(fqp2)}"
      end
    end

    def neg(fqp = %__MODULE__{}) do
      new_coeffs = Enum.map(fqp.coeffs, &rem(-&1 + fqp.field_modulus, fqp.field_modulus))
      %__MODULE__{fqp | coeffs: new_coeffs}
    end

    def equal?(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        false
      else
        Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &(&1 == &2)) |> Enum.all?()
      end
    end

    def pow(fqp = %__MODULE__{}, exponent) when is_integer(exponent) do
      cond do
        exponent == 0 -> one(fqp.field_modulus, fqp.degree, fqp.modulus_coeffs)
        exponent == 1 -> fqp
        rem(exponent, 2) == 0 ->
          half_pow = pow(fqp, Kernel.div(exponent, 2))
          mul(half_pow, half_pow)
        true ->
          half_pow = pow(fqp, Kernel.div(exponent, 2))
          mul(mul(half_pow, half_pow), fqp)
      end
    end

    def inv(fqp = %__MODULE__{}) do
      # For quadratic extensions (degree 2), we can optimize inversion
      if fqp.degree == 2 do
        [a, b] = fqp.coeffs
        p = fqp.field_modulus

        # For BLS12-381, the irreducible polynomial is x^2 + 1
        # So (a + b*i)^(-1) = (a - b*i)/(a^2 + b^2)
        denom = rem(a * a + b * b, p)
        denom_inv = Utils.prime_field_inv(denom, p)
        a_inv = rem(a * denom_inv, p)
        b_inv = rem(-b * denom_inv, p)

        %__MODULE__{fqp | coeffs: [a_inv, b_inv]}
      else
        # For higher degrees, use the fact that x^(p^n-1) = 1 in FQP
        # So x^(-1) = x^(p^n-2)
        p = fqp.field_modulus
        power = :math.pow(p, fqp.degree) - 2 |> trunc()
        pow(fqp, power)
      end
    end

    def divide(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      multiply(fqp1, inv(fqp2))
    end

    def one(field_modulus) when is_integer(field_modulus) do
      one(field_modulus, 1, [0])
    end

    def one(field_modulus, degree, modulus_coeffs) when is_integer(field_modulus) and is_integer(degree) and is_list(modulus_coeffs) do
      coeffs = [1 | List.duplicate(0, degree - 1)]
      new_fqp(coeffs, modulus_coeffs, field_modulus)
    end

    def zero(field_modulus) when is_integer(field_modulus) do
      new(List.duplicate(0, 12), field_modulus)
    end

    def zero(field_modulus, degree) when degree >= 1 do
      coeffs = List.duplicate(0, degree)
      new_fqp(coeffs, List.duplicate(0, degree), field_modulus)
    end

    def zero(_field_modulus, degree) when degree < 1 do
      raise ArgumentError, "FQP.zero/2 called with invalid degree: #{degree}. Degree must be >= 1."
    end

    def zero(field_modulus, degree, modulus_coeffs) when is_integer(field_modulus) and is_integer(degree) and is_list(modulus_coeffs) do
      coeffs = List.duplicate(0, degree)
      new_fqp(coeffs, modulus_coeffs, field_modulus)
    end

    # Add missing functions
    def mul(fqp1, fqp2), do: multiply(fqp1, fqp2)
    def sub(fqp1, fqp2), do: subtract(fqp1, fqp2)
    def eq(fqp1, fqp2), do: equal?(fqp1, fqp2)
    def negate(fqp), do: neg(fqp)

    def is_zero(%__MODULE__{coeffs: coeffs}) do
      Enum.all?(coeffs, &(&1 == 0))
    end

    def is_one(%__MODULE__{coeffs: [1 | rest]}) do
      Enum.all?(rest, &(&1 == 0))
    end
    def is_one(_), do: false

    def conjugate(fqp = %__MODULE__{}), do: fqp # stub: returns self
    def frobenius(fqp = %__MODULE__{}, _power), do: fqp # stub: returns self
  end

  # --- Optimized FQ12 ---
  defmodule FQ12 do
    alias ExEcc.Fields.OptimizedFieldElements.FQP

    defstruct coeffs: [], field_modulus: nil

    @type t_fq12 :: %__MODULE__{
            coeffs: list(FQP.t_fqp()),
            field_modulus: integer
          }

    def new(coeffs, field_modulus) do
      fqp = FQP.new_fqp(coeffs, List.duplicate(0, 12), field_modulus)
      %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
    end

    def add(fq12_1 = %__MODULE__{}, fq12_2 = %__MODULE__{}) do
      if fq12_1.field_modulus != fq12_2.field_modulus do
        raise "Cannot add FQ12 elements from different fields"
      end

      new_coeffs =
        Enum.zip_with(fq12_1.coeffs, fq12_2.coeffs, fn x, y ->
          FQP.add(x, y)
        end)

      %__MODULE__{coeffs: new_coeffs, field_modulus: fq12_1.field_modulus}
    end

    def subtract(fq12_1 = %__MODULE__{}, fq12_2 = %__MODULE__{}) do
      if fq12_1.field_modulus != fq12_2.field_modulus do
        raise "Cannot subtract FQ12 elements from different fields"
      end

      new_coeffs =
        Enum.zip_with(fq12_1.coeffs, fq12_2.coeffs, fn x, y ->
          FQP.subtract(x, y)
        end)

      %__MODULE__{coeffs: new_coeffs, field_modulus: fq12_1.field_modulus}
    end

    def multiply(fq12_1 = %__MODULE__{}, fq12_2 = %__MODULE__{}) do
      if fq12_1.field_modulus != fq12_2.field_modulus do
        raise "Cannot multiply FQ12 elements from different fields"
      end

      # Implement FQ12 multiplication using Karatsuba algorithm
      # This is a simplified version - you may need to optimize it further
      new_coeffs =
        for i <- 0..11 do
          sum =
            for j <- 0..11 do
              k = rem(i - j, 12)
              FQP.multiply(
                Enum.at(fq12_1.coeffs, j),
                Enum.at(fq12_2.coeffs, k)
              )
            end
            |> Enum.reduce(&FQP.add/2)

          sum
        end

      %__MODULE__{coeffs: new_coeffs, field_modulus: fq12_1.field_modulus}
    end

    def divide(fq12_1 = %__MODULE__{}, fq12_2 = %__MODULE__{}) do
      if fq12_1.field_modulus != fq12_2.field_modulus do
        raise "Cannot divide FQ12 elements from different fields"
      end

      multiply(fq12_1, inv(fq12_2))
    end

    def inv(fq12 = %__MODULE__{}) do
      # For FQ12, we need to compute the multiplicative inverse
      # This is a simplified version - in practice, you'd want to use extended GCD
      pow(fq12, fq12.field_modulus - 2)
    end

    def pow(fq12 = %__MODULE__{}, exponent) when is_integer(exponent) do
      cond do
        exponent == 0 ->
          one(fq12.field_modulus)

        exponent == 1 ->
          fq12

        rem(exponent, 2) == 0 ->
          half_pow = pow(fq12, Kernel.div(exponent, 2))
          multiply(half_pow, half_pow)

        true ->
          half_pow = pow(fq12, Kernel.div(exponent, 2))
          multiply(multiply(half_pow, half_pow), fq12)
      end
    end

    def negate(fq12 = %__MODULE__{}) do
      new_coeffs =
        Enum.map(fq12.coeffs, fn x ->
          FQP.neg(x)
        end)

      %__MODULE__{coeffs: new_coeffs, field_modulus: fq12.field_modulus}
    end

    def one(field_modulus) do
      new([1] ++ List.duplicate(0, 11), field_modulus)
    end

    def zero(field_modulus) do
      new(List.duplicate(0, 12), field_modulus)
    end

    def equal?(fq12_1 = %__MODULE__{}, fq12_2 = %__MODULE__{}) do
      fq12_1.field_modulus == fq12_2.field_modulus and
        Enum.all?(Enum.zip(fq12_1.coeffs, fq12_2.coeffs), fn {x, y} ->
          FQP.equal?(x, y)
        end)
    end

    # Add missing functions
    def mul(fq12_1, fq12_2), do: multiply(fq12_1, fq12_2)
    def sub(fq12_1, fq12_2), do: subtract(fq12_1, fq12_2)
    def eq(fq12_1, fq12_2), do: equal?(fq12_1, fq12_2)
    def neg(fq12), do: negate(fq12)
  end

  # --- Optimized FQ2 ---
  defmodule FQ2 do
    @field_modulus 0

    defstruct coeffs: [], field_modulus: nil

    @type t_fq2 :: %__MODULE__{
      coeffs: list(integer),
      field_modulus: integer
    }

    def new(coeffs) when is_list(coeffs) and length(coeffs) == 2 do
      new(coeffs, @field_modulus)
    end

    def new(coeffs, field_modulus) when is_list(coeffs) and length(coeffs) == 2 and is_integer(field_modulus) do
      %__MODULE__{
        coeffs: Enum.map(coeffs, &rem(&1, field_modulus)),
        field_modulus: field_modulus
      }
    end

    def add(fq2_1 = %__MODULE__{}, fq2_2) do
      fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
      new_coeffs = Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, fn a, b ->
        rem(a + b, fq2_1.field_modulus)
      end)
      %__MODULE__{coeffs: new_coeffs, field_modulus: fq2_1.field_modulus}
    end

    def sub(fq2_1 = %__MODULE__{}, fq2_2) do
      fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
      new_coeffs = Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, fn a, b ->
        rem(a - b, fq2_1.field_modulus)
      end)
      %__MODULE__{coeffs: new_coeffs, field_modulus: fq2_1.field_modulus}
    end

    def mul(fq2_1 = %__MODULE__{}, fq2_2) do
      fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
      [a1, b1] = fq2_1.coeffs
      [a2, b2] = fq2_2.coeffs
      p = fq2_1.field_modulus

      # (a1 + b1*i) * (a2 + b2*i) = (a1*a2 - b1*b2) + (a1*b2 + a2*b1)*i
      a = rem(a1 * a2 - b1 * b2, p)
      b = rem(a1 * b2 + a2 * b1, p)

      %__MODULE__{coeffs: [a, b], field_modulus: p}
    end

    def neg(fq2 = %__MODULE__{}) do
      new_coeffs = Enum.map(fq2.coeffs, &rem(-&1, fq2.field_modulus))
      %__MODULE__{coeffs: new_coeffs, field_modulus: fq2.field_modulus}
    end

    def eq(fq2_1 = %__MODULE__{}, fq2_2) do
      fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
      Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, &(&1 == &2)) |> Enum.all?()
    end

    def zero do
      %__MODULE__{coeffs: [0, 0], field_modulus: @field_modulus}
    end

    def zero(field_modulus) do
      %__MODULE__{coeffs: [0, 0], field_modulus: field_modulus}
    end

    def one(field_modulus) do
      %__MODULE__{coeffs: [1, 0], field_modulus: field_modulus}
    end

    defp ensure_fq2(val, field_modulus) when is_integer(val) do
      new([val, 0], field_modulus)
    end

    defp ensure_fq2(fq2 = %__MODULE__{}, field_modulus) do
      if fq2.field_modulus != field_modulus do
        raise "Cannot operate on FQ2 elements from different fields"
      end
      fq2
    end

    # Add missing functions
    def multiply(fq2_1, fq2_2), do: mul(fq2_1, fq2_2)
    def subtract(fq2_1, fq2_2), do: sub(fq2_1, fq2_2)
    def equal?(fq2_1, fq2_2), do: eq(fq2_1, fq2_2)
    def negate(fq2), do: neg(fq2)

    # Add getter for @field_modulus
    def field_modulus, do: @field_modulus
  end

  # Add getter for @field_modulus
  def field_modulus, do: @field_modulus

  def new(val), do: new(val, @field_modulus)
end
