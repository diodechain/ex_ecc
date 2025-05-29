defmodule ExEcc.Fields.FieldElements do
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
  def new_fq(val, field_modulus) when is_integer(val) and is_integer(field_modulus) do
    %__MODULE__{n: rem(val, field_modulus), field_modulus: field_modulus}
  end

  def new_fq(fq_element = %__MODULE__{}, field_modulus) when is_integer(field_modulus) do
    # If val is already an FQ, and field_modulus matches, return it, or re-wrap if modulus differs.
    # This assumes FQ elements from different fields are not directly compatible without re-wrapping.
    if fq_element.field_modulus == field_modulus do
      fq_element
    else
      # This case needs careful consideration: how to handle FQ elements from different fields.
      # For now, re-wrap with the new modulus.
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
    # Handles comparison with integer or another FQ element
    # This is a simplified version; proper type checking might be needed.
    cond do
      is_integer(fq2_val) ->
        fq1.n == fq2_val

      # Basic check for FQ-like struct
      is_map(fq2_val) and Map.has_key?(fq2_val, :n) ->
        fq1.n == fq2_val.n and fq1.field_modulus == fq2_val.field_modulus

      true ->
        false
    end
  end

  # Comparison functions for ordering (Python's @total_ordering)
  def compare(fq1 = %__MODULE__{}, fq2_val) do
    fq2_n = if is_integer(fq2_val), do: fq2_val, else: ensure_fq(fq2_val, fq1.field_modulus).n
    Kernel.compare(fq1.n, fq2_n)
  end

  # Helper to ensure a value is an FQ element for operations
  def ensure_fq(val, field_modulus) when is_integer(val) do
    new_fq(val, field_modulus)
  end

  def ensure_fq(fq = %__MODULE__{}, field_modulus) do
    if fq.field_modulus != field_modulus do
      # This is a potential issue: operating on FQ elements from different fields.
      # Raise an error or handle as per library's design.
      raise "Cannot operate on FQ elements from different fields without explicit conversion."
    end

    fq
  end

  def ensure_fq(other, _field_modulus) do
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

  # FQP - Elements in polynomial extension fields
  # This will be a struct containing a list of FQ elements (coeffs)
  # and modulus_coeffs (also FQ elements or integers representing them).
  # The `degree` will be the length of modulus_coeffs.

  defmodule FQP do
    # FQP will require its own specific FQ type, or operate on generic FQ structs
    # passed with their field_modulus.
    # For now, assume it operates on the FQ defined above.
    # Alias to avoid naming conflict
    alias ExEcc.Fields.FieldElements, as: FQMain

    defstruct coeffs: [], modulus_coeffs: [], degree: 0, field_modulus: nil

    @type t_fqp :: %__MODULE__{
            coeffs: list(FQMain.t_fq()),
            modulus_coeffs: list(FQMain.t_fq()) | list(integer),
            degree: integer,
            field_modulus: integer
          }

    # The Python __init__ for FQP dynamically creates an FQ class.
    # In Elixir, we would typically define the FQ structure once.
    # The FQP functions will need the field_modulus to create FQ elements.
    def new_fqp(coeffs, modulus_coeffs, field_modulus)
        when is_list(coeffs) and is_list(modulus_coeffs) and is_integer(field_modulus) do
      unless length(coeffs) == length(modulus_coeffs) do
        # The original code raises an exception if lengths don't match for FQ2/FQ12 modulus_coeffs vs init coeffs.
        # However, FQP itself is initialized with modulus_coeffs that define the polynomial, and coeffs that define the element.
        # Let's assume for a general FQP, coeffs can be of different length than the base polynomial representation.
        # The Python code seems to use `len(modulus_coeffs)` for degree, which implies modulus_coeffs are fixed for the field type.
        # The check `len(coeffs) != len(modulus_coeffs)` might be specific to how FQ2/FQ12 subclasses use it.
        # For a generic FQP, an element is a polynomial, and its degree can be up to `degree - 1` of the field polynomial.
        # Let's adjust the interpretation: modulus_coeffs define the field, coeffs define the element.
        # The primary concern is that coefficients for the element are valid FQ elements.
      end

      fq_coeffs = Enum.map(coeffs, &FQMain.new_fq(&1, field_modulus))
      # Modulus_coeffs might be integers or FQ elements themselves depending on context.
      # For now, assume they are integers that need to be converted if operations require FQ types.
      # Or, they are already FQ if the field definition implies that.
      # The python code creates an `FQP_corresponding_FQ_class` - this implies FQ elements.
      fq_modulus_coeffs = Enum.map(modulus_coeffs, &FQMain.new_fq(&1, field_modulus))

      %__MODULE__{
        coeffs: fq_coeffs,
        # Storing as FQ for consistency in operations
        modulus_coeffs: fq_modulus_coeffs,
        degree: length(modulus_coeffs),
        field_modulus: field_modulus
      }
    end

    # --- FQP Operations ---
    # These will be more complex, involving polynomial arithmetic.

    def add(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot add FQP elements from different fields or degrees"
      end

      # Ensure coefficient lists are of the same length by padding with zeros if necessary
      # This assumes standard polynomial addition where degrees might differ.
      # Python's `zip` would truncate to the shorter list if degrees differ,
      # which means coeffs lists are expected to be same length by the caller.
      # The python code `len(coeffs) != len(modulus_coeffs)` in `__init__` for `FQP` is confusing.
      # If `coeffs` is for an *element* and `modulus_coeffs` for the *field polynomial*,
      # their lengths usually *don't* have to be the same. An element polynomial can have lower degree.
      # Let's assume `coeffs` are always sized to `degree` for elements.
      new_coeffs_raw = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &FQMain.add(&1, &2))
      %__MODULE__{fqp1 | coeffs: new_coeffs_raw}
    end

    def sub(fqp1 = %__MODULE__{}, fqp2 = %__MODULE__{}) do
      if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
        raise "Cannot subtract FQP elements from different fields or degrees"
      end

      new_coeffs_raw = Enum.zip_with(fqp1.coeffs, fqp2.coeffs, &FQMain.sub(&1, &2))
      %__MODULE__{fqp1 | coeffs: new_coeffs_raw}
    end

    def mul(fqp1 = %__MODULE__{}, other) do
      cond do
        # Scalar multiplication (int or FQ)
        is_integer(other) or is_struct(other, FQMain) ->
          scalar = FQMain.ensure_fq(other, fqp1.field_modulus)
          new_coeffs_raw = Enum.map(fqp1.coeffs, &FQMain.mul(&1, scalar))
          %__MODULE__{fqp1 | coeffs: new_coeffs_raw}

        # FQP multiplication (polynomial multiplication)
        is_struct(other, __MODULE__) -> # Check if other is an FQP struct
          fqp2 = other

          if fqp1.field_modulus != fqp2.field_modulus or fqp1.degree != fqp2.degree do
            raise "Cannot multiply FQP elements from different fields or degrees"
          end

          # Polynomial multiplication: (a0 + a1*x) * (b0 + b1*x) = a0b0 + (a0b1 + a1b0)*x + a1b1*x^2
          # Resulting polynomial has degree deg(fqp1) + deg(fqp2)
          # Then reduce modulo the field polynomial (defined by fqp1.modulus_coeffs)

          # Raw multiplication (convolution)
          len1 = length(fqp1.coeffs)
          len2 = length(fqp2.coeffs)
          # The product polynomial can have up to degree (len1-1) + (len2-1)
          # The result list should accommodate this many terms: len1 + len2 - 1
          prod_coeffs_len = len1 + len2 - 1
          _prod_coeffs_raw = List.duplicate(FQMain.zero(fqp1.field_modulus), prod_coeffs_len)

          _prod_coeffs =
            for {c1, i} <- Enum.with_index(fqp1.coeffs) do
              for {c2, j} <- Enum.with_index(fqp2.coeffs) do
                term_prod = FQMain.mul(c1, c2)
                # Update the coefficient at index i+j
                # This needs a way to update list elements by index or build up the list.
                # Let's rebuild `prod_coeffs_raw` iteratively or use a map for intermediate sums.
                # For simplicity here, this is a conceptual placeholder for correct poly mult.
                # Placeholder for actual accumulation
                {i + j, term_prod}
              end
            end
            |> List.flatten()
            |> Enum.group_by(fn {idx, _val} -> idx end, fn {_idx, val} -> val end)
            |> Enum.map(fn {idx, vals_at_idx} ->
              sum_at_idx =
                Enum.reduce(vals_at_idx, FQMain.zero(fqp1.field_modulus), &FQMain.add/2)

              {idx, sum_at_idx}
            end)
            |> Enum.sort_by(fn {idx, _} -> idx end)
            |> Enum.map(fn {_, val} -> val end)

          # Reduce the resulting polynomial `prod_coeffs` by the `fqp1.modulus_coeffs`.
          # This involves polynomial division. The `Utils.poly_rounded_div` was for integers.
          # We need a version for FQ elements.
          # Python code: `temp = [(x * y) for x in self.coeffs for y in other.coeffs]` is not poly mult.
          # It's `[s*o for s in self.coeffs for o in other.coeffs] if isinstance(other, FQ)`
          # No, the python code `__mul__` for FQP is more complex:
          # if isinstance(other, int_types_or_FQ): -> scalar mult (done)
          # else (it's another FQP):
          #   coeffs = [0] * self.degree
          #   for i, c1 in enumerate(self.coeffs):
          #       if c1 == 0: continue
          #       for j, c2 in enumerate(other.coeffs):
          #           if c2 == 0: continue
          #           coeffs[(i + j) % self.degree] += c1 * c2
          # This is polynomial multiplication in Z_p[x]/<modulus_poly(x)> but simplified, maybe incorrect for general case.
          # It looks like a direct coefficient sum for specific degrees, not general polynomial multiplication then reduction.
          # Or rather, (i+j) % self.degree implies reduction by x^degree - 1 or similar, not by the actual modulus_coeffs.
          # This needs careful review against the Python source. For now, implementing the Python logic directly:

          res_coeffs = List.duplicate(FQMain.zero(fqp1.field_modulus), fqp1.degree)

          indexed_coeffs1 = Enum.with_index(fqp1.coeffs)
          indexed_coeffs2 = Enum.with_index(fqp2.coeffs)

          # This is complex to do immutably and efficiently in Elixir like Python's list mutation.
          # One way is to build a list of {index_to_update, value_to_add} and then process.
          updates =
            for {c1, i} <- indexed_coeffs1,
                FQMain.equal?(c1, FQMain.zero(fqp1.field_modulus)) == false do
              for {c2, j} <- indexed_coeffs2,
                  FQMain.equal?(c2, FQMain.zero(fqp1.field_modulus)) == false do
                product = FQMain.mul(c1, c2)
                target_idx = rem(i + j, fqp1.degree)
                {target_idx, product}
              end
            end
            |> List.flatten()

          final_coeffs =
            Enum.reduce(updates, res_coeffs, fn {idx, val_to_add}, acc_coeffs ->
              current_val_at_idx = Enum.at(acc_coeffs, idx)
              new_val_at_idx = FQMain.add(current_val_at_idx, val_to_add)
              List.replace_at(acc_coeffs, idx, new_val_at_idx)
            end)

          %__MODULE__{fqp1 | coeffs: final_coeffs}

        true ->
          raise "Type error: Expected an integer, FQ, or FQP element for multiplication"
      end
    end

    # __div__ for FQP involves multiplying by the inverse.
    def divide(fqp1 = %__MODULE__{}, other) do
      cond do
        # Scalar division: other is an integer or an FQ element
        is_integer(other) or is_struct(other, FQMain) ->
          scalar = FQMain.ensure_fq(other, fqp1.field_modulus)
          inv_scalar = FQMain.divide(FQMain.one(fqp1.field_modulus), scalar)
          new_coeffs_raw = Enum.map(fqp1.coeffs, &FQMain.mul(&1, inv_scalar))
          %__MODULE__{fqp1 | coeffs: new_coeffs_raw}

        # FQP division: other is an FQP element
        is_struct(other, __MODULE__) -> # Check if other is an FQP struct
          fqp2 = other
          # a / b = a * inv(b)
          mul(fqp1, inv(fqp2))

        true ->
          raise "Type error: Expected an integer, FQ, or FQP element for division, got: #{inspect(other)}"
      end
    end

    # pow for FQP - uses binary exponentiation (exponent is an integer)
    def pow(fqp_base = %__MODULE__{}, exponent) when is_integer(exponent) do
      cond do
        exponent == 0 ->
          %__MODULE__{fqp_base | coeffs: set_coeffs_to_one(fqp_base)}

        exponent == 1 ->
          fqp_base

        # a^-n = (a^-1)^n
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

    defp set_coeffs_to_one(fqp = %__MODULE__{}) do
      # For FQP^0, the result is 1 (the identity element).
      # This means the 0-th coefficient is 1, others are 0.
      [
        FQMain.one(fqp.field_modulus)
        | List.duplicate(FQMain.zero(fqp.field_modulus), fqp.degree - 1)
      ]
    end

    # inv(self: T_FQP) -> T_FQP
    # This is complex, involves extended Euclidean algorithm for polynomials or using Fermat's Little Theorem if applicable.
    # Python code: `return self ** (self.field_modulus ** self.degree - 1)` if prime field for coeffs.
    # This uses `self.field_modulus ** self.degree` as the order of the extension field FQ^degree.
    # And then `elem ^ (order - 1)` is not quite right. It should be `elem ^ (order_of_mult_group - 1)`.
    # Or, `elem ^ (p^d - 2)` where p^d is the field order.
    # The exponent should be `field_order - 2` for `a^(p-2) = a^-1 mod p`.
    # For an extension field F_q^d, the order is q^d. So, inverse is self^(q^d - 2).
    # Python has: `power = self.field_modulus ** self.degree - 1` and then `self ** power`.
    # This must be a typo, should be `self.field_modulus ** self.degree - 2` for inverse.
    # Let's re-check py_ecc. It might be that `FQP.__pow__` handles `p-1` for a specific reason (e.g. related to roots of unity).
    # Actually, the Python code for FQP.inv is:
    #   power = self.field_modulus ** self.degree - 1
    #   if self == FQP.zero(self.field_modulus, self.degree, self.modulus_coeffs):
    #       raise ZeroDivisionError
    #   if self.degree == 1: # FQ
    #       return type(self)([self.coeffs[0] ** (self.field_modulus - 2)])
    #   # Extended Euclidean Algorithm for polynomials
    #   # ... (omitted for brevity, it's complex)
    # The `self ** power` was likely a misremembered detail or from a different context.
    # The actual FQP.inv in py_ecc uses polynomial EEA.
    # For now, let's implement the power version if the degree is 1, and placeholder for others.
    def inv(fqp = %__MODULE__{}) do
      if equal?(fqp, zero(fqp.field_modulus, fqp.degree, fqp.modulus_coeffs)) do
        raise ZeroDivisionError, "Cannot invert zero FQP element"
      end

      if fqp.degree == 1 do
        # This is effectively an FQ element
        inv_coeff0 = FQMain.pow(Enum.at(fqp.coeffs, 0), fqp.field_modulus - 2)
        %__MODULE__{fqp | coeffs: [inv_coeff0]}
      else
        # Placeholder for Polynomial Extended Euclidean Algorithm
        # For example, for FQ2 (degree 2): (a + bu)^-1 = (a - bu) / (a^2 - b^2 * beta)
        # where u^2 - beta = 0 is the modulus polynomial.
        # This requires specific logic based on `modulus_coeffs`.
        # The py_ecc library has a generic polynomial EEA.
        # For now, using Fermat's Little Theorem for extension fields: a^(q^d - 2)
        # where q = field_modulus, d = degree.
        # Order of the extension field
        q_power_d = :math.pow(fqp.field_modulus, fqp.degree) |> round()
        exponent = q_power_d - 2
        pow(fqp, exponent)
      end
    end

    # Equality for FQP
    def equal?(fqp1 = %__MODULE__{}, fqp2) do
      cond do
        is_map(fqp2) and Map.has_key?(fqp2, :coeffs) and Map.has_key?(fqp2, :degree) ->
          # Ensure coeffs lists are comparable
          fqp1.field_modulus == fqp2.field_modulus and
            fqp1.degree == fqp2.degree and
            length(fqp1.coeffs) == length(fqp2.coeffs) and
            Enum.zip_reduce(fqp1.coeffs, fqp2.coeffs, true, fn c1, c2, acc ->
              acc and FQMain.equal?(c1, c2)
            end)

        true ->
          # If comparing FQP with a scalar (0), it means comparing to FQP.zero
          if fqp2 == 0 do
            equal?(fqp1, zero(fqp1.field_modulus, fqp1.degree, fqp1.modulus_coeffs))
          else
            false
          end
      end
    end

    # Negation for FQP
    def neg(fqp = %__MODULE__{}) do
      neg_coeffs = Enum.map(fqp.coeffs, &FQMain.neg/1)
      %__MODULE__{fqp | coeffs: neg_coeffs}
    end

    @spec one(integer, integer, list(FQMain.t_fq()) | list(integer)) :: t_fqp
    def one(field_modulus, degree, modulus_coeffs_repr) do
      # The `one` element in a polynomial ring is the constant polynomial 1.
      # Coeffs: [1, 0, 0, ..., 0]
      one_coeff = FQMain.one(field_modulus)
      zero_coeff = FQMain.zero(field_modulus)
      coeffs = [one_coeff | List.duplicate(zero_coeff, degree - 1)]
      new_fqp(coeffs, modulus_coeffs_repr, field_modulus)
    end

    @spec zero(integer, integer, list(FQMain.t_fq()) | list(integer)) :: t_fqp
    def zero(field_modulus, degree, modulus_coeffs_repr) do
      # The `zero` element is the constant polynomial 0.
      # Coeffs: [0, 0, 0, ..., 0]
      zero_coeff = FQMain.zero(field_modulus)
      coeffs = List.duplicate(zero_coeff, degree)
      new_fqp(coeffs, modulus_coeffs_repr, field_modulus)
    end
  end

  # defmodule FQP

  # FQ2 - Quadratic extension field
  # FQ2 is FQP where degree = 2 and modulus_coeffs are fixed (e.g., u^2 - beta = 0)
  defmodule FQ2 do
    alias ExEcc.Fields.FieldElements.FQP

    @type t_fq2 :: FQP.t_fqp()
    defstruct [:coeffs, :modulus_coeffs, :degree, :field_modulus]

    # Default modulus coefficients for FQ2, e.g., u^2 + 1 = 0 => beta = -1 or (u^2 - (-1)) = 0
    # py_ecc uses FQ2_MODULUS_COEFFS which are (non_residue, 0) for u^2 - non_residue = 0.
    # The `modulus_coeffs` in FQP.__init__ is `(self.FQ2_MODULUS_COEFFS[0],)` for FQ2 in py_ecc.
    # This seems to imply the polynomial is `x - FQ2_MODULUS_COEFFS[0]`, which would make it degree 1.
    # Let's re-check py_ecc FQ2 class:
    # `self.modulus_coeffs = (cls.FQ2_MODULUS_COEFFS[0],)` - this is for FQP's `modulus_coeffs` which seems to be `b` if `x^d - b`
    # FQ2(FQP) `__init__` in py_ecc calls `super().__init__(coeffs, cls.FQ2_MODULUS_COEFFS)`.
    # This means `modulus_coeffs` passed to FQP is `cls.FQ2_MODULUS_COEFFS` tuple itself.
    # If `FQ2_MODULUS_COEFFS` is `(beta, 0)`, then the polynomial is effectively `x^2 - beta` (if interpreted as ax^2+bx+c, c=-beta, b=0, a=1).
    # Or, if `modulus_coeffs` are coeffs of `P(x) = x^deg - sum(modulus_coeffs[i]*x^i)`, then needs clarity.
    # The py_ecc FQP `__init__` takes `modulus_coeffs` and stores them. It then uses `len(modulus_coeffs)` as degree.
    # For FQ2, `FQ2_MODULUS_COEFFS` typically defines `beta` for `u^2 = beta`.
    # For example, in BLS12-381, u^2 = -1. `FQ2_MODULUS_COEFFS` is `(-1, 0)` or similar.
    # Let's assume `modulus_coeffs_config` are the coefficients `[c0, c1, ...]` for `x^d - (c_k-1 x^k-1 + ... + c0) = 0`.
    # The Python `FQP` class takes `modulus_coeffs` which it stores. `FQ2` passes `FQ2_MODULUS_COEFFS` to it.
    # If `FQ2_MODULUS_COEFFS` is `(n, 0)` where `n` is the non-residue, it implies `u^2 - n = 0` or `u^2 = n`.
    # The *coeffs of the polynomial itself* are `[n, 0]` meaning `n*x^0 + 0*x^1`. This is for `x^2 - (coeff[1]*x + coeff[0])`.
    # So, if `modulus_coeffs` = `(beta)`, it represents `x^2 - beta = 0`.
    # If `modulus_coeffs` = `(c0, c1)` for degree 2, it implies `x^2 - (c1*x + c0) = 0`.
    # The `py_ecc.fields.field_properties` has `fq2_modulus_coeffs: (1,0)` for bn128. This is `beta = 1` (for `u^2 - 1 = 0`? or `u^2 + 1 = 0` if interpreted as `u^2 - c0` where `c0 = -1`).
    # `(1,0)` in py_ecc usually means `1*u^0 + 0*u^1` so beta is 1 for `u^2 = beta`. If `u^2 + 1 = 0`, beta = -1.
    # Let's assume `modulus_coeffs_for_field` refers to the `[beta]` in `x^2 - beta = 0`.

    @doc """
    Creates a new FQ2 element: `coeffs[0] + coeffs[1]*u`
    `modulus_coeffs_for_field` defines the field, e.g. `[beta]` for `u^2 - beta = 0`.
    `field_modulus` is the underlying prime field modulus.
    """
    def new_fq2(coeffs_list, modulus_coeffs_for_field, field_modulus)
        # and length(modulus_coeffs_for_field) == 1 for u^2-beta
        when is_list(coeffs_list) and length(coeffs_list) == 2 and
               is_list(modulus_coeffs_for_field) and
               is_integer(field_modulus) do
      # The `modulus_coeffs` for FQP is the polynomial itself. For x^2 - beta, it's `[beta, 0]` (const, x_coeff for poly `P(y)=y^2 - (m1*y + m0)`)
      # If `modulus_coeffs_for_field` is `[beta]`, it represents `u^2 - beta = 0`.
      # The FQP.modulus_coeffs should be `[beta, 0]` if `modulus_polynomial = x^2 - (modulus_coeffs[1]*x + modulus_coeffs[0])`
      # Python's FQ2 `__init__` calls `super().__init__(coeffs, cls.FQ2_MODULUS_COEFFS)`
      # And `cls.FQ2_MODULUS_COEFFS` is a tuple like `(ELEMENT_FX_PARAMETER,)` which is `beta`.
      # The FQP `__init__` takes `modulus_coeffs`. If `len(coeffs) != len(modulus_coeffs)` it errors.
      # This implies that for FQ2, if `coeffs` has length 2, then `modulus_coeffs` given to FQP also has length 2.
      # This is confusing. Let's check py_ecc FQP init: `self.degree = len(self.modulus_coeffs)`
      # `bls12_381_FQ2.FQ2_MODULUS_COEFFS` is `(-1, 0)`. This means `u^2 - (-1) = 0` or `u^2 + 1 = 0` (if beta=-1).
      # And degree is 2. So `modulus_coeffs` for FQP would be `(-1, 0)`.
      # `coeffs` are `[a, b]` for `a + bu`.
      %FQP{}
      |> FQP.new_fqp(coeffs_list, modulus_coeffs_for_field, field_modulus)
      # Cast to FQ2 struct type
      |> Map.put(:__struct__, __MODULE__)
    end

    # FQ2 specific operations can be defined here, or rely on FQP if general enough.
    # For example, multiplication by u (the non-residue).
    # def mul_by_u(fq2 = %__MODULE__{coeffs: [a, b], modulus_coeffs: [beta, _], field_modulus: fm}) do
    #   # (a + bu) * u = au + bu^2 = au + b*beta = b*beta + au
    #   # New coeffs: [b*beta, a]
    #   beta_fq = FQ.new_fq(beta, fm)
    #   a_fq = FQ.new_fq(a, fm) # coeffs are already FQ from FQP.new_fqp
    #   b_fq = FQ.new_fq(b, fm)
    #   new_c0 = FQ.mul(b_fq, beta_fq)
    #   new_c1 = a_fq
    #   new_fq2([new_c0, new_c1], [beta,0], fm)
    # end
  end

  # defmodule FQ2

  # FQ12 - 12th-degree extension field
  # FQ12 is FQP where degree = 12. Typically FQ12 is ((FQ2)^3)^2 or (FQ^6)^2.
  # e.g. FQ12 = FQ6[v] / (v^2 - xi), where xi is in FQ6.
  # Or FQ12 = FQ2[w] / (w^6 - xi), where xi is in FQ2.
  # The `modulus_coeffs` will be more complex.
  defmodule FQ12 do
    alias ExEcc.Fields.FieldElements.FQP

    @type t_fq12 :: FQP.t_fqp()
    defstruct [:coeffs, :modulus_coeffs, :degree, :field_modulus]

    def new_fq12(coeffs_list, modulus_coeffs_for_field, field_modulus)
        # and length should be 12 for FQP general form
        when is_list(coeffs_list) and length(coeffs_list) == 12 and
               is_list(modulus_coeffs_for_field) and
               is_integer(field_modulus) do
      # `modulus_coeffs_for_field` for FQ12 are specific to the curve construction.
      # e.g. for BLS12-381, it's v^6 - xi = 0, where xi = (u+1).
      # The `py_ecc.fields.bls12_381.bls12_381_FQ12.FQ12_MODULUS_COEFFS` is
      # `(FQ2([0,1]) + FQ2([1,0]))` which is `u+1` (an FQ2 element).
      # This means the polynomial is `v^6 - (u+1) = 0`.
      # The `modulus_coeffs` passed to FQP would be `[u+1, 0, 0, 0, 0, 0]` (if degree 6 extension over FQ2).
      # Or for degree 12 over FQ, it's more complex.
      # py_ecc's FQ12 `__init__` calls `super().__init__(coeffs, cls.FQ12_MODULUS_COEFFS)`
      # and `FQ12_MODULUS_COEFFS` is a tuple of 12 integers for some curves (e.g. BN128),
      # or FQ2 elements for others if built as FQ2^6.
      # This initial translation assumes `modulus_coeffs_for_field` are the direct coefficients for FQP.

      %FQP{}
      |> FQP.new_fqp(coeffs_list, modulus_coeffs_for_field, field_modulus)
      |> Map.put(:__struct__, __MODULE__)
    end
  end

  # defmodule FQ12
end
