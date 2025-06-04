defmodule ExEcc.BLS.Hash do
  # Elixir's :crypto module provides HMAC and hash functions.

  @doc """
  HKDF-Extract (using SHA-256)

  https://tools.ietf.org/html/rfc5869
  """

  def hkdf_extract(salt, ikm) when is_binary(salt) and is_binary(ikm) do
    :crypto.mac(:hmac, :sha256, salt, ikm)
  end

  @doc """
  HKDF-Expand (using SHA-256, digest size 32 bytes)

  https://tools.ietf.org/html/rfc5869
  """

  def hkdf_expand(prk, info, length)
      when is_binary(prk) and is_binary(info) and is_integer(length) and length >= 0 do
    # :sha256 digest size
    n = trunc(:math.ceil(length / 32))

    # okm = T(1) || T(2) || T(3) || ... || T(n)
    {_previous, okm} =
      Enum.reduce(0..(n - 1), {"", ""}, fn i, {previous, okm} ->
        # Concatenate (T(i) || info || i)
        text = previous <> info <> <<i + 1>>

        # T(i + 1) = HMAC(T(i) || info || i)
        previous = :crypto.mac(:hmac, :sha256, prk, text)
        {previous, okm <> previous}
      end)

    # Return first `length` bytes.
    binary_part(okm, 0, length)
  end

  @doc """
  Convert a nonnegative integer `x` to an octet string of a specified length `xlen`.
  Big-endian byte order.
  https://tools.ietf.org/html/rfc8017#section-4.1
  """

  def i2osp(x, xlen) when is_integer(x) and x >= 0 and is_integer(xlen) and xlen >= 0 do
    <<x::big-integer-size(xlen * 8)>>
  end

  @doc """
  Convert an octet string `x` to a nonnegative integer.
  Big-endian byte order.
  https://tools.ietf.org/html/rfc8017#section-4.2
  """

  def os2ip(x) when is_binary(x) do
    byte_size = byte_size(x)
    <<int_val::big-integer-size(byte_size * 8)>> = x
    int_val
  end

  @doc """
  SHA-256 hash of a binary string.
  """

  def sha256(x) when is_binary(x) do
    :crypto.hash(:sha256, x)
  end

  @doc """
  XOR two binary strings. They must be of the same length.
  """

  def xor(a, b) when is_binary(a) and is_binary(b) and byte_size(a) == byte_size(b) do
    :crypto.exor(a, b)
  end

  @doc """
  expand_message_xmd from RFC for hashing to curves.
  Uses SHA-256 as the hash function.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-11#section-5.3.1
  """

  defmodule Hash do
    defstruct [:digest_size, :block_size, :fun]
  end

  def sha256_function() do
    %Hash{digest_size: 32, block_size: 64, fun: &sha256/1}
  end

  def expand_message_xmd(msg, dst, len_in_bytes, %Hash{} = hash_function) do
    b_in_bytes = hash_function.digest_size
    r_in_bytes = hash_function.block_size

    if byte_size(dst) > 255 do
      raise "DST must be <= 255 bytes"
    end

    ell = trunc(:math.ceil(len_in_bytes / b_in_bytes))

    if ell > 255 do
      raise "invalid len_in_bytes for hash function (ell > 255)"
    end

    dst_prime = dst <> i2osp(byte_size(dst), 1)
    # Append the length of the DST as a single byte
    z_pad = <<0::size(r_in_bytes * 8)>>
    l_i_b_str = i2osp(len_in_bytes, 2)

    b_0 =
      (z_pad <> msg <> l_i_b_str <> <<0>> <> dst_prime)
      |> hash_function.fun.()

    b = [hash_function.fun.(b_0 <> <<1::size(8)>> <> dst_prime)]

    b =
      Enum.reduce(2..ell, b, fn i, b ->
        b ++ [hash_function.fun.(xor(b_0, Enum.at(b, i - 2)) <> i2osp(i, 1) <> dst_prime)]
      end)

    pseudo_random_bytes = Enum.join(b, "")
    binary_part(pseudo_random_bytes, 0, len_in_bytes)
  end
end
