defmodule ExEcc.Secp256k1 do
  # Elliptic curve parameters (secp256k1)
  @p 2_256 - 2_32 - 977
  @n 115_792_089_237_316_195_423_570_985_008_687_907_852_837_564_279_074_904_382_605_163_141_518_161_494_337
  @a 0
  @b 7
  @gx 550_662_630_222_773_436_695_787_188_951_685_343_262_506_034_537_775_941_755_001_873_603_891_167_292_40
  @gy 326_705_100_207_588_169_780_830_851_305_070_431_844_712_733_806_592_432_759_389_043_357_573_374_824_24
  @g {@gx, @gy}

  # Type for plain points (x, y)
  @type plain_point_2d :: {integer, integer}
  # Type for Jacobian points (x, y, z)
  @type plain_point_3d :: {integer, integer, integer}

  defp bytes_to_int(bytes) when is_binary(bytes) do
    <<int::big-size(byte_size(bytes) * 8)>> = bytes
    int
  end

  # Extended Euclidean Algorithm for modular inverse
  def inv(a, n) when is_integer(a) and is_integer(n) do
    if a == 0 do
      0
    else
      {_g, lm, _hm} = extended_gcd(a, n)
      # Ensure result is positive
      res = rem(lm, n)
      if res < 0, do: res + n, else: res
    end
  end

  # Helper for Extended Euclidean Algorithm: returns {gcd, s, t} where as + nt = gcd
  defp extended_gcd(a, 0), do: {a, 1, 0}

  defp extended_gcd(a, b) do
    {g, s_prev, t_prev} = extended_gcd(b, rem(a, b))
    s = t_prev
    t = s_prev - div(a, b) * t_prev
    {g, s, t}
  end

  def to_jacobian({x, y}) when is_integer(x) and is_integer(y) do
    {x, y, 1}
  end

  def jacobian_double({x, y, z}) do
    cond do
      # Point at infinity or involves division by zero if z=0
      y == 0 or z == 0 ->
        {0, 0, 0}

      true ->
        ysq = rem(y * y, @p)
        s_val = rem(4 * x * ysq, @p)
        # a*z^4 term
        m_val = rem(3 * x * x + @a * z * z * z * z, @p)
        nx = rem(m_val * m_val - 2 * s_val, @p)
        ny = rem(m_val * (s_val - nx) - 8 * ysq * ysq, @p)
        nz = rem(2 * y * z, @p)
        # Ensure results are positive
        {(nx + @p) |> rem(@p), (ny + @p) |> rem(@p), (nz + @p) |> rem(@p)}
    end
  end

  def jacobian_add({x1, y1, z1}, {x2, y2, z2}) do
    cond do
      # p1 is infinity
      y1 == 0 or z1 == 0 ->
        {x2, y2, z2}

      # p2 is infinity
      y2 == 0 or z2 == 0 ->
        {x1, y1, z1}

      true ->
        z1_sq = rem(z1 * z1, @p)
        z2_sq = rem(z2 * z2, @p)
        u1 = rem(x1 * z2_sq, @p)
        u2 = rem(x2 * z1_sq, @p)
        # y1 * z2^3
        s1 = rem(y1 * z2_sq * z2, @p)
        # y2 * z1^3
        s2 = rem(y2 * z1_sq * z1, @p)

        if u1 == u2 do
          if s1 != s2 do
            # Points are inverses
            {0, 0, 1}
          else
            jacobian_double({x1, y1, z1})
          end
        else
          h_val = rem(u2 - u1, @p)
          r_val = rem(s2 - s1, @p)
          h2 = rem(h_val * h_val, @p)
          h3 = rem(h_val * h2, @p)
          u1h2 = rem(u1 * h2, @p)

          nx = rem(r_val * r_val - h3 - 2 * u1h2, @p)
          ny = rem(r_val * (u1h2 - nx) - s1 * h3, @p)
          nz = rem(h_val * z1 * z2, @p)
          # Ensure results are positive
          {(nx + @p) |> rem(@p), (ny + @p) |> rem(@p), (nz + @p) |> rem(@p)}
        end
    end
  end

  def from_jacobian({x, y, z}) do
    if z == 0 do
      # This case implies point at infinity, typically represented differently or handled by context.
      # For secp256k1, usually {0,0} is not a valid point. Let's return a marker or raise.
      # Given inv(0, P) would be problematic. Python returns (0,0) if z=0 via inv(0,P)=0.
      # Let's align: if z_inv is 0, coords will be 0.
      # Or consider a specific representation for infinity like :infinity
      {0, 0}
    else
      z_inv = inv(z, @p)
      z_inv_sq = rem(z_inv * z_inv, @p)
      z_inv_cub = rem(z_inv_sq * z_inv, @p)
      nx = rem(x * z_inv_sq, @p)
      ny = rem(y * z_inv_cub, @p)
      {(nx + @p) |> rem(@p), (ny + @p) |> rem(@p)}
    end
  end

  def jacobian_multiply({jx, jy, jz}, n_val) when is_integer(n_val) do
    cond do
      # Point at infinity or n=0
      jy == 0 or jz == 0 or n_val == 0 ->
        {0, 0, 1}

      n_val == 1 ->
        {jx, jy, jz}

      n_val < 0 or n_val >= @n ->
        jacobian_multiply({jx, jy, jz}, rem(n_val, @n))

      rem(n_val, 2) == 0 ->
        jacobian_double(jacobian_multiply({jx, jy, jz}, div(n_val, 2)))

      rem(n_val, 2) == 1 ->
        jacobian_add(
          jacobian_double(jacobian_multiply({jx, jy, jz}, div(n_val, 2))),
          {jx, jy, jz}
        )
    end
  end

  def multiply({px, py}, n_val) when is_integer(n_val) do
    from_jacobian(jacobian_multiply(to_jacobian({px, py}), n_val))
  end

  def add({p1x, p1y}, {p2x, p2y}) do
    from_jacobian(jacobian_add(to_jacobian({p1x, p1y}), to_jacobian({p2x, p2y})))
  end

  def privtopub(privkey_bytes) when is_binary(privkey_bytes) do
    multiply(@g, bytes_to_int(privkey_bytes))
  end

  def deterministic_generate_k(msghash_bytes, priv_bytes) do
    # Equivalent to b"\x01" * 32, but random
    v = :crypto.strong_rand_bytes(32)
    # Equivalent to b"\x00" * 32, but random
    k = :crypto.strong_rand_bytes(32)

    # RFC 6979 Step D: k = HMAC_K(V || 0x00 || int2octets(x) || bits2octets(h1))
    k = :crypto.mac(:hmac, :sha256, k, v <> <<0>> <> priv_bytes <> msghash_bytes)
    # RFC 6979 Step F: V = HMAC_K(V)
    v = :crypto.mac(:hmac, :sha256, k, v)
    # RFC 6979 Step G: K = HMAC_K(V || 0x01 || int2octets(x) || bits2octets(h1))
    k = :crypto.mac(:hmac, :sha256, k, v <> <<1>> <> priv_bytes <> msghash_bytes)
    # RFC 6979 Step H: V = HMAC_K(V)
    v = :crypto.mac(:hmac, :sha256, k, v)

    # RFC 6979 Step H.3 (loop): Generate T until 0 < T < q
    # The Python code directly returns bytes_to_int(hmac.new(k, v, hashlib.sha256).digest())
    # This implies one iteration is assumed sufficient or the RFC loop is simplified.
    # We will follow py_ecc's simplification for now.
    # T = HMAC_K(V)
    t_bytes = :crypto.mac(:hmac, :sha256, k, v)
    bytes_to_int(t_bytes)
  end

  def ecdsa_raw_sign(msghash_bytes, priv_bytes) do
    z = bytes_to_int(msghash_bytes)
    k_val = deterministic_generate_k(msghash_bytes, priv_bytes)

    {r_val, y_val} = multiply(@g, k_val)

    # s = k^-1 * (z + r * priv_int) mod N
    priv_int = bytes_to_int(priv_bytes)
    k_inv = inv(k_val, @n)
    s_val = rem(k_inv * (z + r_val * priv_int), @n)

    # Recovery ID (v)
    # v = 27 + (y % 2) ^ (0 if s * 2 < N else 1)
    # s_final = if s * 2 < N, do: s, else: N - s
    s_final = if s_val * 2 < @n, do: s_val, else: @n - s_val

    # y_parity = rem(y_val, 2)
    # s_parity_check = if s_final == s_val, do: 0, else: 1 # 0 if s was not flipped, 1 if it was
    # v_val = 27 + (y_parity ^ s_parity_check)
    # Simpler: v = 27 + (y % 2) if s == s_final else 27 + ((y % 2) ^ 1)
    # Or from py_ecc: 27 + ((y % 2) ^ (0 if s * 2 < N else 1))
    # This means y_parity is XORed with 1 if s was flipped (s*2 >= N implies s_final = N-s)
    v_val = 27 + Bitwise.bxor(rem(y_val, 2), if(s_val * 2 < @n, do: 0, else: 1))

    {v_val, r_val, s_final}
  end

  def ecdsa_raw_recover(msghash_bytes, {v_val, r_val, s_val}) do
    unless v_val >= 27 and v_val <= 34 do
      raise ArgumentError, "Invalid recovery ID v: #{v_val}"
    end

    unless r_val > 0 and r_val < @n do
      raise ArgumentError, "Invalid r value: #{r_val}"
    end

    unless s_val > 0 and s_val < @n do
      raise ArgumentError, "Invalid s value: #{s_val}"
    end

    z = bytes_to_int(msghash_bytes)
    # x = r + n*k for k = 0 or 1
    x_val = rem(r_val + @n * div(v_val - 27, 2), @p)

    # y^2 = x^3 + ax + b (mod p)
    # y^2 = x^3 + 7 (mod p) for secp256k1
    y_sq = rem(x_val * x_val * x_val + @a * x_val + @b, @p)
    y = modular_sqrt(y_sq, @p)

    if rem(y, 2) != rem(v_val - 27, 2) do
      y = rem(@p - y, @p)
    end

    # Point R = (x, y)
    r_inv = inv(r_val, @n)

    # e = -z * r^-1 mod N
    e_val = rem(-z * r_inv, @n)
    # f = s * r^-1 mod N
    f_val = rem(s_val * r_inv, @n)

    # Q = eG + fR
    eG = multiply(@g, e_val)
    fR = multiply({x_val, y}, f_val)
    add(eG, fR)
  end

  # Modular square root using Tonelli-Shanks or similar for y^2 = n (mod p)
  # For secp256k1, p % 4 == 3, so sqrt(n) = n^((p+1)/4) mod p
  defp modular_sqrt(n, p) do
    # Since P % 4 == 3 for secp256k1, we can use the simpler formula:
    # x = n^((P+1)/4) mod P
    exponent = div(p + 1, 4)
    # This requires a modular exponentiation function for large integers.
    # :crypto.mod_pow(base, exp, mod)
    candidate = :crypto.mod_pow(n, exponent, p)

    # Verify: candidate^2 mod p == n
    if rem(candidate * candidate, p) == rem(n, p) do
      candidate
    else
      # This should not happen if n is a quadratic residue and p % 4 == 3.
      # If n is not a quadratic residue, there is no sqrt.
      # The calling context (ecdsa_raw_recover) implies a solution should exist.
      raise "Failed to find modular square root, n may not be a quadratic residue or p is not suitable for this simple formula."
    end
  end
end
