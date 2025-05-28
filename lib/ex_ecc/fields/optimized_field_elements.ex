defmodule ExEcc.Fields.OptimizedFieldElements do
  alias ExEcc.Utils
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

  # `__mod__` was NotImplementedError, so we might not need it, or define similarly.
  def modulo(_fq1, _fq2_val) do
    raise "Modulo Operation not yet supported by fields"
  end

  def divide(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    inv_fq2_n = Utils.prime_field_inv(fq2.n, fq1.field_modulus)
    val = rem(fq1.n * inv_fq2_n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

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

  def compare(fq1 = %__MODULE__{}, fq2_val) do
    fq2_n = if is_integer(fq2_val), do: fq2_val, else: ensure_fq(fq2_val, fq1.field_modulus).n
    Kernel.compare(fq1.n, fq2_n)
  end

  # sgn0: Simulating cached_property as a regular function for now.
  def sgn0(fq = %__MODULE__{}) do
    rem(fq.n, 2)
  end

  defp ensure_fq(val, field_modulus) when is_integer(val) do
    new_fq(val, field_modulus)
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

  # --- Optimized FQP ---
  defmodule FQP do
    # Alias to current module for FQ ops
    alias ExEcc.Fields.OptimizedFieldElements, as: FQMain
    alias ExEcc.Utils

    # In optimized version, coeffs are stored as integers, not FQ objects, for performance.
    # `modulus_coeffs` might also be integers.
    # `mc_tuples` is new here: List[Tuple[int, int]] - its usage needs to be understood from Python code.
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

    def new_fqp(coeffs, modulus_coeffs, field_modulus)
        when is_list(coeffs) and is_list(modulus_coeffs) and is_integer(field_modulus) do
      # Optimized FQP doesn't convert coeffs to FQ objects immediately.
      # It stores them as integers modulo field_modulus.
      # Python: `self.coeffs = tuple(coeff % self.field_modulus for coeff in coeffs)` if coeffs[0] is int.
      # Or `tuple(coeff.n % self.field_modulus for coeff in coeffs)` if FQ objects.
      # Assuming input `coeffs` are integers for the optimized path.
      processed_coeffs = Enum.map(coeffs, &rem(&1, field_modulus))
      processed_modulus_coeffs = Enum.map(modulus_coeffs, &rem(&1, field_modulus))

      # `mc_tuples` initialization - this needs to be figured out from its usage in Python.
      # For now, an empty list or based on modulus_coeffs.
      # Python FQP `__init__`: No `mc_tuples` init. It appears in FQ2/FQ12 optimized versions.
      # So, for generic FQP, it might remain uninitialized or default.

      %__MODULE__{
        coeffs: processed_coeffs,
        modulus_coeffs: processed_modulus_coeffs,
        # Or fixed based on type (FQ2, FQ12)
        degree: length(modulus_coeffs),
        field_modulus: field_modulus,
        # Placeholder, to be confirmed by FQ2/FQ12 optimized versions
        mc_tuples: []
      }
    end

    # Basic arithmetic operations for FQP (coeffs are integers)
    # These will need to perform operations and then take `rem field_modulus`.

    def add(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot add FQP elements from different fields or degrees"
      end

      # Ensure coeffs lists are aligned (e.g. by padding, or assuming they are already correct length)
      # Python code `coeffs = [(x + y) % self.field_modulus for x, y in zip(self.coeffs, other.coeffs)]`
      # This assumes `coeffs` are same length.
      new_coeffs =
        Enum.zip_with(fqp1.coeffs, fqp2.coeffs, fn x, y ->
          rem(x + y, fqp1.field_modulus)
        end)

      %__MODULE__{fqp1 | coeffs: new_coeffs}
    end

    def sub(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot subtract FQP elements from different fields or degrees"
      end

      new_coeffs =
        Enum.zip_with(fqp1.coeffs, fqp2.coeffs, fn x, y ->
          rem(x - y, fqp1.field_modulus)
        end)

      %__MODULE__{fqp1 | coeffs: new_coeffs}
    end

    # `__mod__` for FQP was NotImplementedError
    def modulo(_fqp1, _other) do
      raise "Modulo Operation not yet supported by fields"
    end

    # Optimized mul: FQP by scalar (int or FQ) or FQP by FQP
    def mul(fqp1 = %__MODULE__{}, other) do
      fm = fqp1.field_modulus

      cond do
        is_integer(other) ->
          scalar = rem(other, fm)
          new_coeffs = Enum.map(fqp1.coeffs, &rem(&1 * scalar, fm))
          %__MODULE__{fqp1 | coeffs: new_coeffs}

        # FQ object
        is_map(other) and Map.has_key?(other, :n) and Map.has_key?(other, :field_modulus) ->
          # This case assumes `other` is an FQ struct from FQMain (OptimizedFieldElements.FQ)
          # Its .n is already an integer mod fm.
          scalar = other.n
          new_coeffs = Enum.map(fqp1.coeffs, &rem(&1 * scalar, fm))
          %__MODULE__{fqp1 | coeffs: new_coeffs}

        # FQP multiplication
        is_map(other) and Map.has_key?(other, :coeffs) ->
          fqp2 = other

          if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
            raise "Cannot multiply FQP elements from different fields or degrees"
          end

          # Python optimized FQP `__mul__`:
          #   coeffs = [0] * self.degree
          #   for i, c1 in enumerate(self.coeffs):
          #       if c1 == 0: continue
          #       for j, c2 in enumerate(other.coeffs):
          #           if c2 == 0: continue
          #           coeffs[(i + j) % self.degree] = (
          #               coeffs[(i + j) % self.degree] + c1 * c2
          #           ) % self.field_modulus
          # This is the same (potentially simplified) polynomial multiplication logic as non-optimized.
          res_coeffs_int = List.duplicate(0, fqp1.degree)

          updates =
            for {c1, i} <- Enum.with_index(fqp1.coeffs), c1 != 0 do
              for {c2, j} <- Enum.with_index(fqp2.coeffs), c2 != 0 do
                # Raw integer product
                product = c1 * c2
                target_idx = rem(i + j, fqp1.degree)
                {target_idx, product}
              end
            end
            |> List.flatten()

          final_coeffs_int =
            Enum.reduce(updates, res_coeffs_int, fn {idx, val_to_add}, acc_coeffs ->
              current_val_at_idx = Enum.at(acc_coeffs, idx)
              new_val_at_idx = rem(current_val_at_idx + val_to_add, fm)
              List.replace_at(acc_coeffs, idx, new_val_at_idx)
            end)

          %__MODULE__{fqp1 | coeffs: final_coeffs_int}

        true ->
          raise "Type error: Expected an integer, FQ, or FQP element for multiplication"
      end
    end

    # Optimized div: FQP by scalar (int or FQ) or FQP by FQP
    def divide(fqp1 = %__MODULE__{}, other) do
      fm = fqp1.field_modulus

      cond do
        is_integer(other) ->
          inv_scalar = Utils.prime_field_inv(rem(other, fm), fm)
          new_coeffs = Enum.map(fqp1.coeffs, &rem(&1 * inv_scalar, fm))
          %__MODULE__{fqp1 | coeffs: new_coeffs}

        # FQ object
        is_map(other) and Map.has_key?(other, :n) and Map.has_key?(other, :field_modulus) ->
          # other.n is already mod fm
          inv_scalar = Utils.prime_field_inv(other.n, fm)
          new_coeffs = Enum.map(fqp1.coeffs, &rem(&1 * inv_scalar, fm))
          %__MODULE__{fqp1 | coeffs: new_coeffs}

        # FQP division by FQP
        is_map(other) and Map.has_key?(other, :coeffs) ->
          fqp2 = other
          # a / b = a * inv(b)
          mul(fqp1, inv(fqp2))

        true ->
          raise "Type error: Expected an integer, FQ, or FQP element for division"
      end
    end

    # Optimized pow for FQP (exponent is an integer)
    def pow(fqp_base = %__MODULE__{}, exponent) when is_integer(exponent) do
      cond do
        exponent == 0 ->
          %__MODULE__{fqp_base | coeffs: set_coeffs_to_one_opt(fqp_base)}

        exponent == 1 ->
          fqp_base

        exponent < 0 ->
          pow(inv(fqp_base), -exponent)

        rem(exponent, 2) == 0 ->
          half_pow = pow(fqp_base, Kernel.div(exponent, 2))
          mul(half_pow, half_pow)

        true ->
          half_pow = pow(fqp_base, Kernel.div(exponent, 2))
          mul(mul(half_pow, half_pow), fqp_base)
      end
    end

    defp set_coeffs_to_one_opt(fqp = %__MODULE__{}) do
      # Coeffs are integers: [1, 0, ..., 0]
      [1 | List.duplicate(0, fqp.degree - 1)]
    end

    # `optimized_poly_rounded_div` - This was a method in Python, may need translation if used by inv().
    # It seems it was not used by the FQP.inv in Python optimized code, which uses EEA instead.
    # Python: `inv` uses `extended_euclidean_algorithm_polynomial` for FQP.
    # Similar to non-optimized, will use Fermat's Little Theorem for now: a^(q^d - 2)
    def inv(fqp = %__MODULE__{}) do
      if equal?(fqp, zero(fqp.field_modulus, fqp.degree, fqp.modulus_coeffs, fqp.mc_tuples)) do
        raise ZeroDivisionError, "Cannot invert zero FQP element"
      end

      # For optimized FQP, coeffs are integers. Field order is field_modulus^degree.
      # If degree 1, it's an FQ element (represented by a single integer).
      if fqp.degree == 1 do
        inv_coeff0 = Utils.prime_field_inv(Enum.at(fqp.coeffs, 0), fqp.field_modulus)
        %__MODULE__{fqp | coeffs: [inv_coeff0]}
      else
        q_power_d = :math.pow(fqp.field_modulus, fqp.degree) |> round()
        exponent = q_power_d - 2
        pow(fqp, exponent)
      end
    end

    def equal?(fqp1 = %__MODULE__{}, fqp2) do
      cond do
        is_map(fqp2) and Map.has_key?(fqp2, :coeffs) and Map.has_key?(fqp2, :degree) ->
          # Comparing two FQP-like structs (coeffs are lists of integers)
          # Direct list comparison for integers
          fqp1.field_modulus == fqp2.field_modulus and
            fqp1.degree == fqp2.degree and
            fqp1.coeffs == fqp2.coeffs

        true ->
          # Compare to FQP.zero
          if fqp2 == 0 do
            equal?(
              fqp1,
              zero(fqp1.field_modulus, fqp1.degree, fqp1.modulus_coeffs, fqp1.mc_tuples)
            )
          else
            false
          end
      end
    end

    def neg(fqp = %__MODULE__{}) do
      fm = fqp.field_modulus
      neg_coeffs = Enum.map(fqp.coeffs, &rem(-&1, fm))
      %__MODULE__{fqp | coeffs: neg_coeffs}
    end

    # sgn0 for FQP: Python code is complex, uses `sum(sgn0(c) * 2**i for i,c in enumerate(self.coeffs))`
    # where sgn0 for individual coeffs (which are FQ like) is `n % 2`.
    # Since our coeffs are integers, sgn0(c) is `rem(c,2)`.
    def sgn0(fqp = %__MODULE__{}) do
      Enum.with_index(fqp.coeffs)
      |> Enum.reduce(0, fn {c, i}, acc ->
        acc + rem(c, 2) * round(:math.pow(2, i))
      end)
      # The final result seems to be mod 2 based on hash_to_curve draft
      |> rem(2)
    end

    @spec one(integer, integer, list(integer), list({integer, integer})) :: t_fqp
    def one(field_modulus, degree, modulus_coeffs_repr, mc_tuples_repr) do
      coeffs = [1 | List.duplicate(0, degree - 1)]

      %__MODULE__{
        coeffs: coeffs,
        # Assumed to be list of ints
        modulus_coeffs: modulus_coeffs_repr,
        degree: degree,
        field_modulus: field_modulus,
        mc_tuples: mc_tuples_repr
      }
    end

    @spec zero(integer, integer, list(integer), list({integer, integer})) :: t_fqp
    def zero(field_modulus, degree, modulus_coeffs_repr, mc_tuples_repr) do
      coeffs = List.duplicate(0, degree)

      %__MODULE__{
        coeffs: coeffs,
        modulus_coeffs: modulus_coeffs_repr,
        degree: degree,
        field_modulus: field_modulus,
        mc_tuples: mc_tuples_repr
      }
    end
  end

  # defmodule FQP

  # --- Optimized FQ2 ---
  defmodule FQ2 do
    alias ExEcc.Fields.OptimizedFieldElements.FQP

    @type t_fq2 :: FQP.t_fqp()
    # FQ2 specific modulus_coeffs (e.g. for u^2 - beta = 0) and mc_tuples
    # These would typically come from curve parameters.
    defstruct [:coeffs, :modulus_coeffs, :degree, :field_modulus, :mc_tuples]

    def new_fq2(coeffs_list, fq2_modulus_coeffs, mc_tuples_for_fq2, field_modulus)
        # e.g. [-1, 0] for u^2+1=0
        when is_list(coeffs_list) and length(coeffs_list) == 2 and
               is_list(fq2_modulus_coeffs) and
               is_list(mc_tuples_for_fq2) and
               is_integer(field_modulus) do
      %FQP{}
      |> FQP.new_fqp(coeffs_list, fq2_modulus_coeffs, field_modulus)
      |> Map.put(:__struct__, __MODULE__)
      |> Map.put(:mc_tuples, mc_tuples_for_fq2)
      # Explicitly degree 2
      |> Map.put(:degree, 2)
    end

    # sgn0 for FQ2: Python code has specific override.
    # `(sgn0(self.coeffs[0]) + sgn0(self.coeffs[1]) * 2) % 2` if `Q == -1` (BLS12_381)
    # `sgn0(self.coeffs[0])` otherwise. This depends on curve params (Q = FQ2_MODULUS_COEFFS[0])
    # For now, general FQP sgn0 will be inherited. Needs specialization if porting specific curves.
    # The optimized FQ2 `sgn0` in Python is:
    # if self.FQ2_MODULUS_COEFFS[0] == -1:  # BLS12_381 specific optimization
    #   return (self.coeffs[0] % 2 + self.coeffs[1] % 2 * 2) % 2
    # else:
    #   return self.coeffs[0] % 2
    # This requires FQ2_MODULUS_COEFFS to be accessible here.
    # Let's pass it or retrieve from the struct.
    def sgn0(fq2 = %__MODULE__{coeffs: [c0, c1], modulus_coeffs: [mc0 | _]}) do
      # Assuming mc0 is the equivalent of FQ2_MODULUS_COEFFS[0] (beta for u^2-beta=0)
      # BLS12-381 specific like optimization
      if mc0 == -1 do
        rem(rem(c0, 2) + rem(c1, 2) * 2, 2)
      else
        rem(c0, 2)
      end
    end
  end

  # defmodule FQ2

  # --- Optimized FQ12 ---
  defmodule FQ12 do
    alias ExEcc.Fields.OptimizedFieldElements.FQP

    @type t_fq12 :: FQP.t_fqp()
    defstruct [:coeffs, :modulus_coeffs, :degree, :field_modulus, :mc_tuples]

    def new_fq12(coeffs_list, fq12_modulus_coeffs, mc_tuples_for_fq12, field_modulus)
        # Should be length 12 for FQP representation
        when is_list(coeffs_list) and length(coeffs_list) == 12 and
               is_list(fq12_modulus_coeffs) and
               is_list(mc_tuples_for_fq12) and
               is_integer(field_modulus) do
      %FQP{}
      |> FQP.new_fqp(coeffs_list, fq12_modulus_coeffs, field_modulus)
      |> Map.put(:__struct__, __MODULE__)
      |> Map.put(:mc_tuples, mc_tuples_for_fq12)
      # Explicitly degree 12
      |> Map.put(:degree, 12)
    end

    # sgn0 for FQ12 would also be inherited from FQP by default.
    # Python FQ12.sgn0 is `super().sgn0` which calls FQP.sgn0.
  end

  # defmodule FQ12
end
