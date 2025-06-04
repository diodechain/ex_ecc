defmodule ExEcc.BLS.HashToCurve do
  alias ExEcc.BLS.Constants
  alias ExEcc.BLS.Hash
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2

  @doc """
  Convert a message to a point on G2 as defined here:
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-6.6.3

  The idea is to first hash into FQ2 and then use SSWU to map the result into G2.

  Contents and inputs follow the ciphersuite ``BLS12381G2_XMD:SHA-256_SSWU_RO_``
  defined here:
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-8.8.2
  """
  def hash_to_g2(message, dst, hash_function) do
    {u0, u1} = hash_to_field_fq2(message, 2, dst, hash_function)
    q0 = map_to_curve_g2(u0)
    q1 = map_to_curve_g2(u1)
    r = ExEcc.OptimizedBLS12381.OptimizedCurve.add(q0, q1)
    clear_cofactor_g2(r)
  end

  @doc """
  Hash To Base Field for FQ2.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-5.3
  Returns a tuple of `count` FQ2 elements.
  Implicitly uses SHA256.
  """
  #
  def hash_to_field_fq2(message, count, dst, hash_function) do
    # Extension degree of FQ2
    m = 2
    len_in_bytes = count * m * Constants.hash_to_field_l()
    pseudo_random_bytes = Hash.expand_message_xmd(message, dst, len_in_bytes, hash_function)

    Enum.reduce(0..(count - 1), [], fn i, u ->
      e =
        Enum.reduce(0..(m - 1), [], fn j, e ->
          elem_offset = Constants.hash_to_field_l() * (j + i * m)
          tv = binary_part(pseudo_random_bytes, elem_offset, Constants.hash_to_field_l())
          e ++ [Integer.mod(Hash.os2ip(tv), ExEcc.OptimizedBLS12381.OptimizedCurve.field_modulus())]
        end)

      u ++ [FQ2.new(List.to_tuple(e))]
    end)
    |> List.to_tuple()
  end

  @doc """
  Map To Curve for G2

  First, convert FQ2 point to a point on the 3-Isogeny curve.
  SWU Map: https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-6.6.3

  Second, map 3-Isogeny curve to BLS12-381-G2 curve.
  3-Isogeny Map:
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#appendix-C.3
  """
  def map_to_curve_g2(u) do
    {x, y, z} = ExEcc.OptimizedBLS12381.OptimizedSWU.optimized_swu_g2(u)
    ExEcc.OptimizedBLS12381.OptimizedSWU.iso_map_g2(x, y, z)
  end

  @doc """
  Clear Cofactor via Multiplication

  Ensure a point falls in the correct sub group of the curve.
  """
  def clear_cofactor_g2(p) do
    ExEcc.OptimizedBLS12381.OptimizedClearCofactor.multiply_clear_cofactor_g2(p)
  end

  # --- Hash to G1 ---

  @doc """
  Convert a message to a point on G1 as defined here:
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-6.6.3

  The idea is to first hash into FQ and then use SSWU to map the result into G1.

  Contents and inputs follow the ciphersuite ``BLS12381G1_XMD:SHA-256_SSWU_RO_``
  defined here:
  https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-hash-to-curve-09#section-8.8.1
  """
  def hash_to_g1(message, dst, hash_function) do
    {u0, u1} = hash_to_field_fq(message, 2, dst, hash_function)
    q0 = map_to_curve_g1(u0)
    q1 = map_to_curve_g1(u1)
    r = ExEcc.OptimizedBLS12381.OptimizedCurve.add(q0, q1)
    clear_cofactor_g1(r)
  end

  @doc """
  Hash To Base Field for FQ

  Convert a message to a point in the finite field as defined here:
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-5.3
  """
  def hash_to_field_fq(message, count, dst, hash_function) do
    # Extension degree of FQ
    m = 1
    len_in_bytes = count * m * Constants.hash_to_field_l()
    pseudo_random_bytes = Hash.expand_message_xmd(message, dst, len_in_bytes, hash_function)

    Enum.reduce(0..(count - 1), [], fn i, u ->
      e =
        Enum.reduce(0..(m - 1), [], fn j, e ->
          elem_offset = Constants.hash_to_field_l() * (j + i * m)
          tv = binary_part(pseudo_random_bytes, elem_offset, Constants.hash_to_field_l())
          e ++ [Integer.mod(Hash.os2ip(tv), ExEcc.OptimizedBLS12381.OptimizedCurve.field_modulus())]
        end)

      u ++ [FQ.new(List.to_tuple(e))]
    end)
    |> List.to_tuple()
  end

  @doc """
  Map To Curve for G1

  First, convert FQ point to a point on the 11-Isogeny curve.
  SWU Map: https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-6.6.3

  Second, map 11-Isogeny curve to BLS12-381-G1 curve.
  11-Isogeny Map:
  https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-hash-to-curve-09#name-11-isogeny-map-for-bls12-38
  """
  def map_to_curve_g1(u) do
    {x, y, z} = ExEcc.OptimizedBLS12381.OptimizedSWU.optimized_swu_g1(u)
    ExEcc.OptimizedBLS12381.OptimizedSWU.iso_map_g1(x, y, z)
  end

  @doc """
  Clear Cofactor via Multiplication

  Ensure a point falls in the correct subgroup of the curve.
  """
  def clear_cofactor_g1(p) do
    ExEcc.OptimizedBLS12381.OptimizedClearCofactor.multiply_clear_cofactor_g1(p)
  end
end
