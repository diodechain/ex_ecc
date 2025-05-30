defmodule ExEcc.Utils do
  # NOTE: This is a very rough, direct translation. It will need significant
  # idiomatic Elixir changes, especially around type handling and function definitions.

  # Elixir doesn't have a direct TYPE_CHECKING equivalent. Imports are handled differently.
  # We'll need to define or import FQ and optimized_FQ types/modules when they are created.

  # @type int_or_fq :: integer | ExEcc.Fields.FieldElements.FQ.t()
  # Assuming FQ will be a struct, its type would be FQ.t(). This is a placeholder.

  @doc """
  Extended Euclidean algorithm to find modular inverses for integers.
  Returns the modular multiplicative inverse of a modulo n.
  """
  def prime_field_inv(a, n) do
    # Ensure a is positive and within the field
    a = rem(a + n, n)
    cond do
      a == 0 -> raise "Cannot find modular inverse of 0"
      a == n -> raise "Cannot find modular inverse of n"
      true ->
        {g, x, _y} = egcd(a, n)
        if g != 1 do
          raise "No modular inverse for #{a} mod #{n}"
        else
          rem(x + n, n)
        end
    end
  end

  defp egcd(a, 0), do: {a, 1, 0}
  defp egcd(a, b) do
    {g, x1, y1} = egcd(b, rem(a, b))
    {g, y1, x1 - div(a, b) * y1}
  end

  # Utility methods for polynomial math
  # Elixir typically uses lists for sequences. `p` is assumed to be a list.
  # The type `Union[int, "FQ", "optimized_FQ"]` needs to be resolved once FQ types are defined.
  def deg([]), do: 0
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

  @doc """
  Computes the greatest common divisor of two integers.
  """
  def gcd(a, b) when is_integer(a) and is_integer(b) do
    if b == 0, do: abs(a), else: gcd(b, rem(a, b))
  end

  @doc """
  Computes the least common multiple of two integers.
  """
  def lcm(a, b) when is_integer(a) and is_integer(b) do
    div(abs(a * b), gcd(a, b))
  end

  @doc """
  Computes the modular exponentiation of a number.
  """
  def mod_pow(base, exponent, modulus) when is_integer(base) and is_integer(exponent) and is_integer(modulus) do
    cond do
      exponent == 0 -> 1
      exponent == 1 -> rem(base, modulus)
      rem(exponent, 2) == 0 ->
        half_pow = mod_pow(base, Kernel.div(exponent, 2), modulus)
        rem(half_pow * half_pow, modulus)
      true ->
        half_pow = mod_pow(base, Kernel.div(exponent, 2), modulus)
        rem(rem(half_pow * half_pow, modulus) * rem(base, modulus), modulus)
    end
  end

  @doc """
  Computes the Jacobi symbol (a/n).
  """
  def jacobi(a, n) when is_integer(a) and is_integer(n) and n > 0 and rem(n, 2) == 1 do
    a = rem(a, n)
    cond do
      a == 0 -> 0
      a == 1 -> 1
      a == 2 ->
        case rem(n, 8) do
          1 -> 1
          3 -> -1
          5 -> -1
          7 -> 1
        end
      rem(a, 2) == 0 ->
        jacobi(2, n) * jacobi(div(a, 2), n)
      true ->
        if rem(a, 4) == 3 and rem(n, 4) == 3 do
          -jacobi(n, a)
        else
          jacobi(n, a)
        end
    end
  end

  @doc """
  Computes the Legendre symbol (a/p).
  """
  def legendre(a, p) when is_integer(a) and is_integer(p) and p > 2 and rem(p, 2) == 1 do
    jacobi(a, p)
  end

  @doc """
  Computes the square root of a number modulo a prime.
  """
  def sqrt_mod(n, p) do
    # Tonelli-Shanks algorithm
    cond do
      n == 0 -> 0
      n == 1 -> 1
      true ->
        # Find Q and S such that p-1 = Q*2^S
        {q, s} = find_q_and_s(p - 1)

        # Find a quadratic non-residue z
        z = find_non_residue(p)

        # Initialize variables
        c = mod_pow(z, q, p)
        t = mod_pow(n, q, p)
        r = mod_pow(n, div(q + 1, 2), p)

        # Main loop
        tonelli_shanks_loop(t, r, c, s, p)
    end
  end

  defp find_q_and_s(n, s \\ 0) do
    if rem(n, 2) == 0 do
      find_q_and_s(div(n, 2), s + 1)
    else
      {n, s}
    end
  end

  defp tonelli_shanks_loop(1, r, _c, _m, _p), do: r
  defp tonelli_shanks_loop(t, r, c, m, p) do
    i = find_i(t, p)
    b = mod_pow(c, :math.pow(2, m - i - 1) |> trunc(), p)
    new_c = mod_pow(b, 2, p)
    new_t = rem(t * new_c, p)
    new_r = rem(r * b, p)
    tonelli_shanks_loop(new_t, new_r, new_c, i, p)
  end

  defp find_i(t, p, i \\ 0) do
    if mod_pow(t, :math.pow(2, i) |> trunc(), p) == 1 do
      i
    else
      find_i(t, p, i + 1)
    end
  end

  defp find_non_residue(p) do
    Enum.find(2..(p - 1), fn z ->
      legendre(z, p) == -1
    end)
  end

  defp while(condition, body) do
    if condition.() do
      body.()
      while(condition, body)
    end
  end
end
