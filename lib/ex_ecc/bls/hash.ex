defmodule ExEcc.Bls.Hash do
  # Elixir's :crypto module provides HMAC and hash functions.

  @doc """
  HKDF-Extract (using SHA-256)

  https://tools.ietf.org/html/rfc5869
  """
  @spec hkdf_extract(binary, binary) :: binary
  def hkdf_extract(salt, ikm) when is_binary(salt) and is_binary(ikm) do
    :crypto.mac(:hmac, :sha256, salt, ikm)
  end

  @doc """
  HKDF-Expand (using SHA-256, digest size 32 bytes)

  https://tools.ietf.org/html/rfc5869
  """
  @spec hkdf_expand(binary, binary, integer) :: binary
  def hkdf_expand(prk, info, length)
      when is_binary(prk) and is_binary(info) and is_integer(length) and length >= 0 do
    # :sha256 digest size
    hash_len = 32
    n = :math.ceil(length / hash_len) |> round()

    okm_acc = รอบ_hkdf_expand(prk, info, n, <<>>, <<>>, 1)
    :binary.part(okm_acc, 0, length)
  end

  defp รอบ_hkdf_expand(_prk, _info, n, okm, _previous, i) when i > n do
    okm
  end

  defp รอบ_hkdf_expand(prk, info, n, okm, previous, i) do
    text = previous <> info <> <<i::size(8)>>
    current_t = :crypto.hmac(:sha256, prk, text)
    new_okm = okm <> current_t
    รอบ_hkdf_expand(prk, info, n, new_okm, current_t, i + 1)
  end

  @doc """
  Convert a nonnegative integer `x` to an octet string of a specified length `xlen`.
  Big-endian byte order.
  https://tools.ietf.org/html/rfc8017#section-4.1
  """
  @spec i2osp(integer, integer) :: binary
  def i2osp(x, xlen) when is_integer(x) and x >= 0 and is_integer(xlen) and xlen >= 0 do
    <<x::big-integer-size(xlen * 8)>>
  end

  @doc """
  Convert an octet string `x` to a nonnegative integer.
  Big-endian byte order.
  https://tools.ietf.org/html/rfc8017#section-4.2
  """
  @spec os2ip(binary) :: integer
  def os2ip(x) when is_binary(x) do
    byte_size = byte_size(x)
    <<int_val::big-integer-size(byte_size * 8)>> = x
    int_val
  end

  @doc """
  SHA-256 hash of a binary string.
  """
  @spec sha256(binary) :: binary
  def sha256(x) when is_binary(x) do
    :crypto.hash(:sha256, x)
  end

  @doc """
  XOR two binary strings. They must be of the same length.
  """
  @spec xor(binary, binary) :: binary
  def xor(a, b) when is_binary(a) and is_binary(b) and byte_size(a) == byte_size(b) do
    :binary.bin_to_list(a)
    |> Enum.zip(:binary.bin_to_list(b))
    |> Enum.map(fn {byte_a, byte_b} -> Bitwise.bxor(byte_a, byte_b) end)
    |> Enum.into(<<>>)

    # More efficient way using binary comprehensions if Elixir version supports well or custom NIF.
    # For now, list conversion is straightforward.
    # A more direct binary approach (less readable but potentially faster for large binaries):
    # size = byte_size(a)
    # << result :: binary-size(size) >> =
    #   for i <- 0..(size-1), into: <<>> do
    #     << Bitwise.bxor(:binary.at(a,i), :binary.at(b,i)) :: size(8) >>
    #   end
    # result
  end

  @doc """
  expand_message_xmd from RFC for hashing to curves.
  Uses SHA-256 as the hash function.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-11#section-5.3.1
  """
  @spec expand_message_xmd(binary, binary, integer) :: binary
  def expand_message_xmd(msg, dst, len_in_bytes) do
    # Using :sha256, so b_in_bytes = 32, r_in_bytes = 64 (block size)
    # :crypto.hash_info(:sha256).digest_size
    b_in_bytes = 32

    # :crypto.hash_info(:sha256).block_size (not directly available, SHA256 block size is 512 bits = 64 bytes)
    r_in_bytes = 64

    if byte_size(dst) > 255 do
      raise ArgumentError, "DST must be <= 255 bytes"
    end

    ell = :math.ceil(len_in_bytes / b_in_bytes) |> round()

    if ell > 255 do
      raise ArgumentError, "invalid len_in_bytes for hash function (ell > 255)"
    end

    dst_prime = dst <> i2osp(byte_size(dst), 1)
    # binary of r_in_bytes zeros
    z_pad = <<0::size(r_in_bytes * 8)>>
    l_i_b_str = i2osp(len_in_bytes, 2)

    b_0_input = z_pad <> msg <> l_i_b_str <> <<0::size(8)>> <> dst_prime
    b_0 = sha256(b_0_input)

    b_1_input = b_0 <> <<1::size(8)>> <> dst_prime
    b_1 = sha256(b_1_input)

    # Iteratively compute b_i
    # pseudorandom_bytes = b_1 <> b_2 <> ... <> b_ell
    # {[list_of_b_i_digests], prev_b_0, prev_b_i_minus_1}
    initial_acc = {[b_1], b_0, b_1}

    {all_b_digests, _, _} =
      Enum.reduce(2..ell, initial_acc, fn i, {acc_b_list, acc_b_0, acc_b_i_minus_1} ->
        input_for_b_i = xor(acc_b_0, acc_b_i_minus_1) <> i2osp(i, 1) <> dst_prime
        current_b_i = sha256(input_for_b_i)
        # Prepending, so will reverse later
        {[current_b_i | acc_b_list], acc_b_0, current_b_i}
      end)

    pseudo_random_bytes = Enum.reverse(all_b_digests) |> IO.iodata_to_binary()
    :binary.part(pseudo_random_bytes, 0, len_in_bytes)
  end
end
