defmodule ExEcc.Bls.PointCompression do
  alias ExEcc.Bls.Constants
  # alias ExEcc.Bls.Typing # G1Compressed, G1Uncompressed, G2Compressed, G2Uncompressed
  # alias ExEcc.Fields.OptimizedFieldElements, as: OptFQ # FQ, FQ2 structs and operations
  # alias ExEcc.OptimizedBls12381 # Z1, Z2, b, b2, field_modulus (q), is_inf, is_on_curve, normalize

  # Placeholder for field_modulus (q)
  # q = ExEcc.OptimizedBls12381.field_modulus()
  # b_g1 = ExEcc.OptimizedBls12381.b() # Curve parameter for G1
  # b_g2 = ExEcc.OptimizedBls12381.b2() # Curve parameter for G2 (an FQ2 element)
  # z1_point = ExEcc.OptimizedBls12381.z1() # Point at infinity for G1
  # z2_point = ExEcc.OptimizedBls12381.z2() # Point at infinity for G2

  # For optimized FQ/FQ2 elements, we assume they are integers or tuples of integers.
  # FQ.n would be the integer itself.
  # FQ2.coeffs would be [real_part, imag_part], both integers.

  @doc """
  Extracts the C, B, and A flags from a compressed G1/G2 coordinate component.
  The format: (c_flag, b_flag, a_flag, x_bits)
  """
  @spec get_flags(integer) :: {boolean, boolean, boolean}
  def get_flags(z) when is_integer(z) do
    # POW_2_381, POW_2_382, POW_2_383 are large integers
    c_flag = Bitwise.band(Bitwise.sr(z, 383), 1) == 1
    b_flag = Bitwise.band(Bitwise.sr(z, 382), 1) == 1
    a_flag = Bitwise.band(Bitwise.sr(z, 381), 1) == 1
    {c_flag, b_flag, a_flag}
  end

  @doc """
  Checks if a compressed point (z1 or z1,z2) represents the point at infinity.
  """
  @spec is_point_at_infinity(integer, integer | nil) :: boolean
  def is_point_at_infinity(z1, z2 \\ nil) do
    # rem(z1, Constants.pow_2_381()) == 0 and (is_nil(z2) or z2 == 0)
    # Actual check from py_ecc: (z1 % POW_2_381 == 0) and (z2 is None or z2 == 0)
    # However, the b_flag (infinity flag) is the primary indicator used in decompression logic.
    # This helper might be for a different context or a redundant check if b_flag is already known.
    # The `decompress_G1/G2` uses `b_flag` for this, so this function might not be directly needed there.
    # Let's rely on b_flag for decompression logic as per the Python code.
    # This is a direct port of the helper if needed elsewhere.
    (rem(z1, Constants.pow_2_381()) == 0) and (is_nil(z2) or z2 == 0)
  end

  # --- G1 Compression/Decompression ---

  # @spec compress_g1(ExEcc.Bls.Typing.g1_uncompressed()) :: ExEcc.Bls.Typing.g1_compressed()
  def compress_g1(pt) do
    # q = ExEcc.OptimizedBls12381.field_modulus()
    # if ExEcc.OptimizedBls12381.is_inf(pt) do
    #   Constants.pow_2_383() + Constants.pow_2_382()
    # else
    #   {x_fq, y_fq} = ExEcc.OptimizedBls12381.normalize(pt) # Assuming normalize returns FQ structs/maps
    #   x_n = x_fq.n # Assuming FQ struct has .n field for integer value
    #   y_n = y_fq.n
    #   a_flag = div(y_n * 2, q) # Integer division
    #   x_n + a_flag * Constants.pow_2_381() + Constants.pow_2_383()
    # end
    # |> ExEcc.Bls.Typing.g1_compressed() # If G1Compressed is a struct/tagged tuple
    :not_implemented_compress_g1
  end

  # @spec decompress_g1(ExEcc.Bls.Typing.g1_compressed()) :: ExEcc.Bls.Typing.g1_uncompressed()
  def decompress_g1(z_compressed) do
    # z = z_compressed # If G1Compressed is just an integer alias
    # {c_flag, b_flag, a_flag} = get_flags(z)
    # q_val = ExEcc.OptimizedBls12381.field_modulus()
    # b_val_fq = ExEcc.OptimizedBls12381.b() # This should be an FQ struct
    # b_val_n = b_val_fq.n

    # unless c_flag, do: raise ArgumentError, "c_flag should be 1"
    # is_inf_pt_flag = b_flag # Direct use of b_flag

    # if is_inf_pt_flag do
    #   if a_flag, do: raise ArgumentError, "a point at infinity should have a_flag == 0"
    #   ExEcc.OptimizedBls12381.z1() # Return point at infinity for G1
    # else
    #   x_n = rem(z, Constants.pow_2_381())
    #   if x_n >= q_val, do: raise ArgumentError, "Point value should be less than field modulus. Got #{x_n}"

    #   # Y^2 = X^3 + b (mod q)
    #   # y_n_sq_candidate = rem(x_n * x_n * x_n + b_val_n, q_val)
    #   # y_n = :crypto.mod_pow(y_n_sq_candidate, div(q_val + 1, 4), q_val) # Modular sqrt using Fermat's Little theorem (if q % 4 == 3)
    #   # Python uses pow(base, exp, mod)
    #   y_n_sq_rhs = rem(ModMath.pow(x_n, 3, q_val) + b_val_n, q_val)
    #   y_n = ModMath.pow(y_n_sq_rhs, div(q_val + 1, 4), q_val)

    #   unless rem(ModMath.pow(y_n, 2, q_val), q_val) == rem(y_n_sq_rhs, q_val) do
    #     raise ArgumentError, "The given point is not on G1: y**2 = x**3 + b"
    #   end

    #   y_sign_bit = div(y_n * 2, q_val)
    #   y_final_n = if y_sign_bit != Bitwise.to_integer(a_flag), do: q_val - y_n, else: y_n

    #   # Construct FQ elements for the point (x, y, 1)
    #   # x_fq = OptFQ.new_fq(x_n, q_val)
    #   # y_fq = OptFQ.new_fq(y_final_n, q_val)
    #   # one_fq = OptFQ.one(q_val)
    #   # {x_fq, y_fq, one_fq}
    #   :not_implemented_yet_non_inf_g1_decomp
    # end
    :not_implemented_decompress_g1
  end

  # --- G2 Compression/Decompression ---

  @doc """
  Modular square root in FQ2. Favors higher imaginary or real part for tie-breaking.
  This requires FQ2 arithmetic (pow, division, negation, comparison of coeffs).
  """
  # @spec modular_squareroot_in_fq2(ExEcc.Fields.OptimizedFieldElements.FQ2.t_fq2()) :: ExEcc.Fields.OptimizedFieldElements.FQ2.t_fq2() | nil
  def modular_squareroot_in_fq2(value_fq2) do
    # eighth_roots = Constants.eighth_roots_of_unity() # List of FQ2 elements
    # fq2_order_val = Constants.fq2_order()

    # # FQ2 exponentiation: value_fq2 ** ((fq2_order_val + 8) // 16)
    # candidate_sqrt = OptFQ.FQ2.pow(value_fq2, div(fq2_order_val + 8, 16))
    # check_val_fq2 = OptFQ.FQ2.divide(OptFQ.FQ2.pow(candidate_sqrt, 2), value_fq2)

    # # Check if check_val_fq2 is in EIGHTH_ROOTS_OF_UNITY[::2] (every second element)
    # # This means comparing FQ2 elements.
    # matching_root_index = Enum.find_index(0..3, fn i ->
    #   OptFQ.FQ2.equal?(check_val_fq2, Enum.at(eighth_roots, i * 2))
    # end)

    # if is_integer(matching_root_index) do
    #   root_divisor = Enum.at(eighth_roots, matching_root_index * 2)
    #   x1 = OptFQ.FQ2.divide(candidate_sqrt, root_divisor)
    #   x2 = OptFQ.FQ2.neg(x1)

    #   # Compare x1 and x2 based on coeffs [real, imag]
    #   # {x1_re, x1_im} = OptFQ.FQ2.coeffs(x1) # Assuming coeffs returns [re, im]
    #   # {x2_re, x2_im} = OptFQ.FQ2.coeffs(x2)

    #   # if x1_im > x2_im or (x1_im == x2_im and x1_re > x2_re), do: x1, else: x2
    #   :not_implemented_comparison_part
    # else
    #   nil
    # end
    :not_implemented_modular_sqrt_fq2
  end

  # @spec compress_g2(ExEcc.Bls.Typing.g2_uncompressed()) :: ExEcc.Bls.Typing.g2_compressed()
  def compress_g2(pt) do
    # q_val = ExEcc.OptimizedBls12381.field_modulus()
    # b2_val = ExEcc.OptimizedBls12381.b2() # FQ2 element
    # unless ExEcc.OptimizedBls12381.is_on_curve(pt, b2_val), do: raise ArgumentError, "Point not on twisted curve G2"

    # if ExEcc.OptimizedBls12381.is_inf(pt) do
    #   {Constants.pow_2_383() + Constants.pow_2_382(), 0}
    # else
    #   {x_fq2, y_fq2} = ExEcc.OptimizedBls12381.normalize(pt) # x_fq2, y_fq2 are FQ2 structs

    #   # Assuming FQ2 struct has .coeffs field like [real_part_int, imag_part_int]
    #   # For optimized elements, coeffs are integers.
    #   [x_re, x_im] = x_fq2.coeffs
    #   [y_re, y_im] = y_fq2.coeffs

    #   a_flag1 = if y_im > 0, do: div(y_im * 2, q_val), else: div(y_re * 2, q_val)

    #   # x_im goes to z1, x_re goes to z2
    #   z1 = x_im + a_flag1 * Constants.pow_2_381() + Constants.pow_2_383()
    #   z2 = x_re
    #   {round(z1), round(z2)} # Ensure integers if any FQ2 ops returned floats (unlikely with direct int coeffs)
    # end
    # # |> ExEcc.Bls.Typing.g2_compressed() # If G2Compressed is a struct/tagged tuple
    :not_implemented_compress_g2
  end

  # @spec decompress_g2(ExEcc.Bls.Typing.g2_compressed()) :: ExEcc.Bls.Typing.g2_uncompressed()
  def decompress_g2(p_compressed) do
    # {z1, z2} = p_compressed # If G2Compressed is a tuple {int, int}
    # {c_flag1, b_flag1, a_flag1} = get_flags(z1)
    # q_val = ExEcc.OptimizedBls12381.field_modulus()
    # b2_val_fq2 = ExEcc.OptimizedBls12381.b2()

    # unless c_flag1, do: raise ArgumentError, "c_flag1 should be 1"
    # is_inf_pt_flag = b_flag1

    # if is_inf_pt_flag do
    #   if a_flag1, do: raise ArgumentError, "a point at infinity should have a_flag1 == 0"
    #   ExEcc.OptimizedBls12381.z2() # Return point at infinity for G2
    # else
    #   x1_im_part = rem(z1, Constants.pow_2_381())
    #   if x1_im_part >= q_val, do: raise ArgumentError, "x1 (imag) value invalid"
    #   if z2 >= q_val, do: raise ArgumentError, "z2 (real) value invalid"

    #   x2_re_part = z2

    #   # x = FQ2([x2_re_part, x1_im_part]) using OptimizedFieldElements.FQ2.new_fq2/4 or similar
    #   # Need modulus_coeffs and mc_tuples for the specific FQ2 field (BLS12-381 specific)
    #   # x_fq2 = OptFQ.FQ2.new_fq2([x2_re_part, x1_im_part], bls12381_fq2_mod_coeffs, bls12381_fq2_mc_tuples, q_val)

    #   # y_fq2_sq_rhs = OptFQ.FQ2.add(OptFQ.FQ2.pow(x_fq2, 3), b2_val_fq2)
    #   # y_fq2 = modular_squareroot_in_fq2(y_fq2_sq_rhs)

    #   # if is_nil(y_fq2), do: raise ArgumentError, "Failed to find modular squareroot in FQ2"

    #   # # Sign selection for y_fq2
    #   # [y_re_int, y_im_int] = y_fq2.coeffs

    #   # y_sign_bit_source = if y_im_int > 0, do: y_im_int, else: y_re_int
    #   # chosen_sign_bit = div(y_sign_bit_source * 2, q_val)

    #   # y_final_fq2 =
    #   #   if chosen_sign_bit != Bitwise.to_integer(a_flag1) do
    #   #     OptFQ.FQ2.neg(y_fq2)
    #   #   else
    #   #     y_fq2
    #   #   end

    #   # one_fq2 = OptFQ.FQ2.new_fq2([1,0], bls12381_fq2_mod_coeffs, bls12381_fq2_mc_tuples, q_val) # FQ2(1)
    #   # candidate_point = {x_fq2, y_final_fq2, one_fq2}

    #   # unless ExEcc.OptimizedBls12381.is_on_curve(candidate_point, b2_val_fq2) do
    #   #   raise ArgumentError, "Decompressed point not on G2 curve"
    #   # end
    #   # candidate_point
    #   :not_implemented_yet_non_inf_g2_decomp
    # end
    :not_implemented_decompress_g2
  end

  # Helper for modular arithmetic if not relying on a full ModMath library yet
  # This is a simplified pow, :crypto.mod_pow is better for crypto contexts.
  # Ensure this is used carefully, or replaced with :crypto.mod_pow where appropriate.
  defmodule ModMath do
    def pow(_base, 0, _mod), do: 1
    def pow(base, 1, _mod), do: base
    def pow(base, exp, mod) when rem(exp,2) == 0 do
      half = pow(base, div(exp,2), mod)
      rem(half * half, mod)
    end
    def pow(base, exp, mod) do
      half = pow(base, div(exp,2), mod)
      rem(half * half * base, mod)
    end
  end

end
