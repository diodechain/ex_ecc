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
      res = Integer.mod(lm, n)
      if res < 0, do: res + n, else: res
    end
  end

  # Helper for Extended Euclidean Algorithm: returns {gcd, s, t} where as + nt = gcd
  defp extended_gcd(a, 0), do: {a, 1, 0}

  defp extended_gcd(a, b) do
    {g, s_prev, t_prev} = extended_gcd(b, Integer.mod(a, b))
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
        ysq = Integer.mod(y * y, @p)
        s_val = Integer.mod(4 * x * ysq, @p)
        # a*z^4 term
        m_val = Integer.mod(3 * x * x + @a * z * z * z * z, @p)
        nx = Integer.mod(m_val * m_val - 2 * s_val, @p)
        ny = Integer.mod(m_val * (s_val - nx) - 8 * ysq * ysq, @p)
        nz = Integer.mod(2 * y * z, @p)
        # Ensure results are positive
        {(nx + @p) |> Integer.mod(@p), (ny + @p) |> Integer.mod(@p), (nz + @p) |> Integer.mod(@p)}
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
        z1_sq = Integer.mod(z1 * z1, @p)
        z2_sq = Integer.mod(z2 * z2, @p)
        u1 = Integer.mod(x1 * z2_sq, @p)
        u2 = Integer.mod(x2 * z1_sq, @p)
        # y1 * z2^3
        s1 = Integer.mod(y1 * z2_sq * z2, @p)
        # y2 * z1^3
        s2 = Integer.mod(y2 * z1_sq * z1, @p)

        if u1 == u2 do
          if s1 != s2 do
            # Points are inverses
            {0, 0, 1}
          else
            jacobian_double({x1, y1, z1})
          end
        else
          h_val = Integer.mod(u2 - u1, @p)
          r_val = Integer.mod(s2 - s1, @p)
          h2 = Integer.mod(h_val * h_val, @p)
          h3 = Integer.mod(h_val * h2, @p)
          u1h2 = Integer.mod(u1 * h2, @p)

          nx = Integer.mod(r_val * r_val - h3 - 2 * u1h2, @p)
          ny = Integer.mod(r_val * (u1h2 - nx) - s1 * h3, @p)
          nz = Integer.mod(h_val * z1 * z2, @p)
          # Ensure results are positive
          {(nx + @p) |> Integer.mod(@p), (ny + @p) |> Integer.mod(@p),
           (nz + @p) |> Integer.mod(@p)}
        end
    end
  end

  def from_jacobian({x, y, z}) do
    if z == 0 do
      {0, 0}
    else
      z_inv = inv(z, @p)
      z_inv_sq = Integer.mod(z_inv * z_inv, @p)
      z_inv_cub = Integer.mod(z_inv_sq * z_inv, @p)
      nx = Integer.mod(x * z_inv_sq, @p)
      ny = Integer.mod(y * z_inv_cub, @p)
      {(nx + @p) |> Integer.mod(@p), (ny + @p) |> Integer.mod(@p)}
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
        jacobian_multiply({jx, jy, jz}, Integer.mod(n_val, @n))

      Integer.mod(n_val, 2) == 0 ->
        jacobian_double(jacobian_multiply({jx, jy, jz}, div(n_val, 2)))

      Integer.mod(n_val, 2) == 1 ->
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
    priv_int = bytes_to_int(privkey_bytes)

    if priv_int >= @n do
      raise ArgumentError, "Private key must be less than the curve order"
    end

    multiply(@g, priv_int)
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
    t_bytes = :crypto.mac(:hmac, :sha256, k, v)
    t = bytes_to_int(t_bytes)
    if t >= @n, do: deterministic_generate_k(msghash_bytes, priv_bytes), else: t
  end

  def ecdsa_raw_sign(msghash_bytes, priv_bytes) do
    z = bytes_to_int(msghash_bytes)
    k_val = deterministic_generate_k(msghash_bytes, priv_bytes)

    {r_val, y_val} = multiply(@g, k_val)

    # s = k^-1 * (z + r * priv_int) mod N
    priv_int = bytes_to_int(priv_bytes)
    k_inv = inv(k_val, @n)
    s_val = Integer.mod(k_inv * (z + r_val * priv_int), @n)

    # Recovery ID (v)
    # v = 27 + (y % 2) ^ (0 if s * 2 < N else 1)
    # s_final = if s * 2 < N, do: s, else: N - s
    s_final = if s_val * 2 < @n, do: s_val, else: @n - s_val
    v = 27 + Integer.mod(y_val, 2) + if s_val * 2 < @n, do: 0, else: 1

    {v, r_val, s_final}
  end

  def ecdsa_raw_recover(msghash_bytes, {v, r, s}) do
    z = bytes_to_int(msghash_bytes)
    x = r
    y_squared = Integer.mod(x * x * x + @a * x + @b, @p)
    y = pow_mod(y_squared, div(@p + 1, 4), @p)

    # Determine which y to use based on v
    y = if Integer.mod(v, 2) == 0, do: y, else: @p - y

    # Calculate R = (x, y)
    r_point = {x, y}

    # Calculate Q = (z * s^-1) * R + (r * s^-1) * G
    s_inv = inv(s, @n)
    u1 = multiply(r_point, Integer.mod(z * s_inv, @n))
    u2 = multiply(@g, Integer.mod(r * s_inv, @n))
    add(u1, u2)
  end

  defp pow_mod(base, exp, mod) do
    pow_mod(base, exp, mod, 1)
  end

  defp pow_mod(_base, 0, _mod, acc), do: acc

  defp pow_mod(base, exp, mod, acc) do
    if Integer.mod(exp, 2) == 1 do
      pow_mod(Integer.mod(base * base, mod), div(exp, 2), mod, Integer.mod(acc * base, mod))
    else
      pow_mod(Integer.mod(base * base, mod), div(exp, 2), mod, acc)
    end
  end
end
