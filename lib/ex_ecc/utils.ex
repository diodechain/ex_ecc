defmodule ExEcc.Utils do
  # NOTE: This is a very rough, direct translation. It will need significant
  # idiomatic Elixir changes, especially around type handling and function definitions.

  # Elixir doesn't have a direct TYPE_CHECKING equivalent. Imports are handled differently.
  # We'll need to define or import FQ and optimized_FQ types/modules when they are created.

  # @type int_or_fq :: integer | ExEcc.Fields.FieldElements.FQ.t()
  # Assuming FQ will be a struct, its type would be FQ.t(). This is a placeholder.

  @doc """
  Extended Euclidean algorithm to find modular inverses for integers.

  To address a == n edge case.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-4
  inv0(x): This function returns the multiplicative inverse of x in
  F, extended to all of F by fixing inv0(0) == 0.
  """
  def prime_field_inv(a, n) when is_integer(a) and is_integer(n) do
    a = rem(a, n)

    if a == 0 do
      0
    else
      # Elixir's :crypto.mod_inverse/2 might be a more direct equivalent for positive integers.
      # This is a direct port of the Python logic.
      loop_prime_field_inv(a, n, 1, 0, rem(a, n), n)
    end
  end

  defp loop_prime_field_inv(_a, n, lm, _hm, low, _high) when low <= 1 do
    rem(lm, n)
  end

  defp loop_prime_field_inv(a, n, lm, hm, low, high) do
    r = div(high, low)
    nm = hm - lm * r
    new = high - low * r
    loop_prime_field_inv(a, n, nm, lm, new, low)
  end

  # Utility methods for polynomial math
  # Elixir typically uses lists for sequences. `p` is assumed to be a list.
  # The type `Union[int, "FQ", "optimized_FQ"]` needs to be resolved once FQ types are defined.
  def deg(p) when is_list(p) do
    d = length(p) - 1
    do_deg(p, d)
  end

  defp do_deg(_p, d) when d == 0 do
    0
  end

  defp do_deg(p, d) do
    if Enum.at(p, d) == 0 and d > 0 do
      do_deg(p, d - 1)
    else
      d
    end
  end

  # Placeholder for poly_rounded_div. This function is more complex and will require
  # careful handling of types and arithmetic operations in Elixir.
  # The `IntOrFQ` type and operations like division (/) will behave differently.
  # `cast` is not directly applicable; Elixir uses pattern matching and guards for type safety.
  def poly_rounded_div(a, b) when is_list(a) and is_list(b) do
    # dega = deg(a)
    # degb = deg(b)
    # temp = Enum.to_list(a) # or simply `a` if it's already a list
    # o = List.duplicate(0, length(a))

    # for i <- (dega - degb)..0//-1 do
    #   # o_val_at_i = o[i] + div(Enum.at(temp, degb + i), Enum.at(b, degb))
    #   # # update o
    #   # for c <- 0..degb do
    #   #   # temp_val_at_c_plus_i = Enum.at(temp, c + i) - o[c]
    #   #   # # update temp
    #   # end
    # end
    # # List.to_tuple(Enum.slice(o, 0, deg(o) + 1))
    :not_implemented_yet
  end
end
