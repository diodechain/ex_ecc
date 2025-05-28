defmodule ExEcc.Bls.HashToCurve do
  alias ExEcc.Bls.Constants
  alias ExEcc.Bls.Hash
  # alias ExEcc.Bls.Typing # For G1Uncompressed, G2Uncompressed types
  # alias ExEcc.Fields.OptimizedFieldElements, as: OptFQ # For FQ, FQ2 types/structs
  # alias ExEcc.OptimizedBls12381 # For curve operations like add, iso_map, swu, clear_cofactor, field_modulus

  # The `hash_function` parameter in Python often implies a specific hash like hashlib.sha256.
  # In Elixir, we'll assume SHA-256 as it's common for BLS12-381 ciphersuites.
  # If other hash functions are needed, the functions here would need to be parameterized or duplicated.

  # --- Hash to G2 ---

  @doc """
  Convert a message to a point on G2.
  Follows BLS12381G2_XMD:SHA-256_SSWU_RO_ ciphersuite.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-8.8.2
  """
  # @spec hash_to_g2(binary, binary) :: ExEcc.Bls.Typing.g2_uncompressed()
  def hash_to_g2(message, dst) do
    # {u0, u1} = hash_to_field_fq2(message, 2, dst) # Assuming hash_function is SHA256 implicitly
    # q0 = map_to_curve_g2(u0)
    # q1 = map_to_curve_g2(u1)
    # r = ExEcc.OptimizedBls12381.add(q0, q1)
    # p = clear_cofactor_g2(r)
    # p
    :not_implemented_yet_g2
  end

  @doc """
  Hash To Base Field for FQ2.
  https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-5.3
  Returns a tuple of `count` FQ2 elements.
  Implicitly uses SHA256.
  """
  # @spec hash_to_field_fq2(binary, integer, binary) :: {ExEcc.Fields.OptimizedFieldElements.FQ2.t_fq2(), ...}
  def hash_to_field_fq2(message, count, dst) do
    m = 2 # Extension degree of FQ2
    len_in_bytes = count * m * Constants.hash_to_field_l()
    pseudo_random_bytes = Hash.expand_message_xmd(message, dst, len_in_bytes) # Assumes SHA256 from expand_message_xmd

    # field_mod = ExEcc.OptimizedBls12381.field_modulus()
    # modulus_coeffs_fq2 = ExEcc.OptimizedBls12381.Parameters.fq2_modulus_coeffs() # Or however it's defined
    # mc_tuples_fq2 = ExEcc.OptimizedBls12381.Parameters.fq2_mc_tuples() # Or however it's defined

    # Enum.map(0..(count-1), fn i ->
    #   e_coeffs = Enum.map(0..(m-1), fn j ->
    #     elem_offset = Constants.hash_to_field_l() * (j + i * m)
    #     tv = :binary.part(pseudo_random_bytes, elem_offset, Constants.hash_to_field_l())
    #     rem(Hash.os2ip(tv), field_mod)
    #   end)
    #   # Construct FQ2 element. Assumes OptFQ.FQ2.new_fq2/4 or similar constructor.
    #   # OptFQ.FQ2.new_fq2(e_coeffs, modulus_coeffs_fq2, mc_tuples_fq2, field_mod)
    # end)
    # |> List.to_tuple() # If the return spec is a tuple
    :not_implemented_yet_fq2_field_hash
  end

  @doc """
  Map To Curve for G2 (using SWU map and 3-Isogeny).
  """
  # @spec map_to_curve_g2(ExEcc.Fields.OptimizedFieldElements.FQ2.t_fq2()) :: ExEcc.Bls.Typing.g2_uncompressed()
  def map_to_curve_g2(u_fq2) do
    # {x, y, z} = ExEcc.OptimizedBls12381.optimized_swu_g2(u_fq2)
    # ExEcc.OptimizedBls12381.iso_map_g2(x, y, z)
    :not_implemented_yet_map_g2
  end

  @doc """
  Clear Cofactor for G2 points.
  """
  # @spec clear_cofactor_g2(ExEcc.Bls.Typing.g2_uncompressed()) :: ExEcc.Bls.Typing.g2_uncompressed()
  def clear_cofactor_g2(p) do
    # ExEcc.OptimizedBls12381.multiply_clear_cofactor_g2(p)
    :not_implemented_yet_clear_g2
  end

  # --- Hash to G1 ---

  @doc """
  Convert a message to a point on G1.
  Follows BLS12381G1_XMD:SHA-256_SSWU_RO_ ciphersuite.
  https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-hash-to-curve-09#section-8.8.1
  """
  # @spec hash_to_g1(binary, binary) :: ExEcc.Bls.Typing.g1_uncompressed()
  def hash_to_g1(message, dst) do
    # {u0, u1} = hash_to_field_fq(message, 2, dst) # Assuming SHA256
    # q0 = map_to_curve_g1(u0)
    # q1 = map_to_curve_g1(u1)
    # r = ExEcc.OptimizedBls12381.add(q0, q1)
    # p = clear_cofactor_g1(r)
    # p
    :not_implemented_yet_g1
  end

  @doc """
  Hash To Base Field for FQ.
  Returns a tuple of `count` FQ elements.
  Implicitly uses SHA256.
  """
  # @spec hash_to_field_fq(binary, integer, binary) :: {ExEcc.Fields.OptimizedFieldElements.FQ.t_fq(), ...}
  def hash_to_field_fq(message, count, dst) do
    m = 1 # Extension degree of FQ
    len_in_bytes = count * m * Constants.hash_to_field_l()
    pseudo_random_bytes = Hash.expand_message_xmd(message, dst, len_in_bytes)

    # field_mod = ExEcc.OptimizedBls12381.field_modulus()

    # Enum.map(0..(count-1), fn i ->
    #   elem_offset = Constants.hash_to_field_l() * (i * m) # m is 1, so just i
    #   tv = :binary.part(pseudo_random_bytes, elem_offset, Constants.hash_to_field_l())
    #   fq_val = rem(Hash.os2ip(tv), field_mod)
    #   # Construct FQ element. Assumes OptFQ.new_fq/2 or similar.
    #   # OptFQ.new_fq(fq_val, field_mod)
    # end)
    # |> List.to_tuple()
    :not_implemented_yet_fq_field_hash
  end

  @doc """
  Map To Curve for G1 (using SWU map and 11-Isogeny).
  """
  # @spec map_to_curve_g1(ExEcc.Fields.OptimizedFieldElements.FQ.t_fq()) :: ExEcc.Bls.Typing.g1_uncompressed()
  def map_to_curve_g1(u_fq) do
    # {x, y, z} = ExEcc.OptimizedBls12381.optimized_swu_g1(u_fq)
    # ExEcc.OptimizedBls12381.iso_map_g1(x, y, z)
    :not_implemented_yet_map_g1
  end

  @doc """
  Clear Cofactor for G1 points.
  """
  # @spec clear_cofactor_g1(ExEcc.Bls.Typing.g1_uncompressed()) :: ExEcc.Bls.Typing.g1_uncompressed()
  def clear_cofactor_g1(p) do
    # ExEcc.OptimizedBls12381.multiply_clear_cofactor_g1(p)
    :not_implemented_yet_clear_g1
  end
end
