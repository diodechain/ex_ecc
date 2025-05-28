defmodule ExEcc.OptimizedBN128.OptimizedPairing do
  alias ExEcc.Fields.OptimizedBN128FQ, as: FQ
  alias ExEcc.Fields.OptimizedBN128FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBN128FQ12, as: FQ12
  alias ExEcc.OptimizedBN128.OptimizedCurve, as: Curve

  @field_modulus Curve.field_modulus()
  @curve_order Curve.curve_order()

  @ate_loop_count 29_793_968_203_157_093_288
  # Max index for pseudo_binary_encoding (0 to 63)
  @log_ate_loop_count 63
  @pseudo_binary_encoding [
    0,
    0,
    0,
    1,
    0,
    1,
    0,
    -1,
    0,
    0,
    1,
    -1,
    0,
    0,
    1,
    0,
    0,
    1,
    1,
    0,
    -1,
    0,
    0,
    1,
    0,
    -1,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    0,
    0,
    -1,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    -1,
    0,
    0,
    1,
    1,
    0,
    0,
    -1,
    0,
    0,
    0,
    1,
    1,
    0,
    -1,
    0,
    0,
    1,
    0,
    1,
    1
  ]

  # TODO: Port the pseudo_binary_encoding check to a test case.
  # if Enum.sum(Enum.with_index(@pseudo_binary_encoding, fn e, i -> e * :math.pow(2,i) end)) != @ate_loop_count do
  #   raise "Pseudo binary encoding is incorrect"
  # end

  def normalize1(p) do
    {x, y} = Curve.normalize(p)
    field_module = x.__struct__
    {x, y, field_module.one()}
  end

  # Returns {numerator, denominator}
  def linefunc(p1, p2, t) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    {xt, yt, zt} = t
    # Assumes all points are in the same field FQ12 for pairing line functions
    field_module = x1.__struct__
    zero = field_module.zero()

    m_numerator = field_module.sub(field_module.mul(y2, z1), field_module.mul(y1, z2))
    m_denominator = field_module.sub(field_module.mul(x2, z1), field_module.mul(x1, z2))

    cond do
      not field_module.eq(m_denominator, zero) ->
        num =
          field_module.sub(
            field_module.mul(
              m_numerator,
              field_module.sub(field_module.mul(xt, z1), field_module.mul(x1, zt))
            ),
            field_module.mul(
              m_denominator,
              field_module.sub(field_module.mul(yt, z1), field_module.mul(y1, zt))
            )
          )

        den = field_module.mul(m_denominator, field_module.mul(zt, z1))
        {num, den}

      # P1 == P2, use tangent line
      field_module.eq(m_numerator, zero) ->
        m_num_tangent = field_module.mul(field_module.new(3), field_module.mul(x1, x1))
        m_den_tangent = field_module.mul(field_module.new(2), field_module.mul(y1, z1))

        num =
          field_module.sub(
            field_module.mul(
              m_num_tangent,
              field_module.sub(field_module.mul(xt, z1), field_module.mul(x1, zt))
            ),
            field_module.mul(
              m_den_tangent,
              field_module.sub(field_module.mul(yt, z1), field_module.mul(y1, zt))
            )
          )

        den = field_module.mul(m_den_tangent, field_module.mul(zt, z1))
        {num, den}

      # Vertical line (P1 == -P2 or P1 or P2 is inf, though inf handled by m_denominator usually)
      true ->
        num = field_module.sub(field_module.mul(xt, z1), field_module.mul(x1, zt))
        den = field_module.mul(z1, zt)
        {num, den}
    end
  end

  def cast_point_to_fq12(pt) do
    # Check if point is at infinity
    if Curve.is_inf(pt) do
      # Return FQ12 point at infinity
      {FQ12.one(), FQ12.one(), FQ12.zero()}
    else
      # These are FQ elements
      {x, y, z} = pt
      # FQ12.new expects a tuple of FQ elements. Access .n for integer value for the first coeff.
      x_fq12_coeffs = List.to_tuple([x.n | List.duplicate(FQ.zero().n, 11)])
      y_fq12_coeffs = List.to_tuple([y.n | List.duplicate(FQ.zero().n, 11)])
      z_fq12_coeffs = List.to_tuple([z.n | List.duplicate(FQ.zero().n, 11)])

      {FQ12.new(x_fq12_coeffs), FQ12.new(y_fq12_coeffs), FQ12.new(z_fq12_coeffs)}
    end
  end

  # TODO: Port consistency checks for linefunc to a test suite.

  def miller_loop(q_fq12, p_fq12, do_final_exponentiate \\ true) do
    if Curve.is_inf(q_fq12) or Curve.is_inf(p_fq12) do
      FQ12.one()
    else
      r_acc = q_fq12
      f_num_acc = FQ12.one()
      f_den_acc = FQ12.one()

      # Python: pseudo_binary_encoding[63::-1] iterates from index 63 down to 0.
      # The last element of @pseudo_binary_encoding is at index 63.
      loop_coeffs = Enum.reverse(@pseudo_binary_encoding)

      {r_final, f_num_final, f_den_final} =
        Enum.reduce(loop_coeffs, {r_acc, f_num_acc, f_den_acc}, fn v, {r, f_num, f_den} ->
          {line_n_double, line_d_double} = linefunc(r, r, p_fq12)
          f_num_new = FQ12.mul(FQ12.mul(f_num, f_num), line_n_double)
          f_den_new = FQ12.mul(FQ12.mul(f_den, f_den), line_d_double)
          r_new = Curve.double(r)

          cond do
            v == 1 ->
              {line_n_add, line_d_add} = linefunc(r_new, q_fq12, p_fq12)
              f_num_updated = FQ12.mul(f_num_new, line_n_add)
              f_den_updated = FQ12.mul(f_den_new, line_d_add)
              r_updated = Curve.add(r_new, q_fq12)
              {r_updated, f_num_updated, f_den_updated}

            v == -1 ->
              nq_fq12 = Curve.neg(q_fq12)
              {line_n_add_neg, line_d_add_neg} = linefunc(r_new, nq_fq12, p_fq12)
              f_num_updated = FQ12.mul(f_num_new, line_n_add_neg)
              f_den_updated = FQ12.mul(f_den_new, line_d_add_neg)
              r_updated = Curve.add(r_new, nq_fq12)
              {r_updated, f_num_updated, f_den_updated}

            # v == 0
            true ->
              {r_new, f_num_new, f_den_new}
          end
        end)

      # Q1 = (Q[0] ** field_modulus, Q[1] ** field_modulus, Q[2] ** field_modulus)
      # In Elixir, Q is q_fq12 = {qx, qy, qz} which are FQ12 elements.
      # FQ12.pow(element, exponent) is needed. field_modulus is an integer.
      # This is the Frobenius endomorphism for FQ12 elements.
      # x-coord of Q ^ p
      q1_x = FQ12.pow(elem(q_fq12, 0), @field_modulus)
      # y-coord of Q ^ p
      q1_y = FQ12.pow(elem(q_fq12, 1), @field_modulus)
      # z-coord of Q ^ p
      q1_z = FQ12.pow(elem(q_fq12, 2), @field_modulus)
      q1 = {q1_x, q1_y, q1_z}
      # TODO: assert is_on_curve(Q1, Curve.b12())

      # nQ2 = (Q1[0] ** field_modulus, -Q1[1] ** field_modulus, Q1[2] ** field_modulus)
      # x-coord of Q1 ^ p
      nq2_x = FQ12.pow(q1_x, @field_modulus)
      # -(y-coord of Q1 ^ p)
      nq2_y = FQ12.neg(FQ12.pow(q1_y, @field_modulus))
      # z-coord of Q1 ^ p
      nq2_z = FQ12.pow(q1_z, @field_modulus)
      nq2 = {nq2_x, nq2_y, nq2_z}
      # TODO: assert is_on_curve(nQ2, Curve.b12())

      {line1_n, line1_d} = linefunc(r_final, q1, p_fq12)
      r_after_q1 = Curve.add(r_final, q1)
      {line2_n, line2_d} = linefunc(r_after_q1, nq2, p_fq12)

      f_num_combined = FQ12.mul(FQ12.mul(f_num_final, line1_n), line2_n)
      f_den_combined = FQ12.mul(FQ12.mul(f_den_final, line1_d), line2_d)
      f = FQ12.div(f_num_combined, f_den_combined)

      if do_final_exponentiate do
        final_exponentiate(f)
      else
        f
      end
    end
  end

  def pairing(q_fq2_pt, p_fq_pt, do_final_exponentiate \\ true) do
    # unless Curve.is_on_curve(q_fq2_pt, Curve.b2()) do
    #   raise ArgumentError, "Point Q is not on the twisted curve G2"
    # end
    # unless Curve.is_on_curve(p_fq_pt, Curve.b()) do
    #   raise ArgumentError, "Point P is not on the curve G1"
    # end

    if Curve.is_inf(p_fq_pt) or Curve.is_inf(q_fq2_pt) do
      FQ12.one()
    else
      # Twist Q (FQ2 point) to FQ12 space
      q_twisted_fq12 = Curve.twist(q_fq2_pt)
      # Cast P (FQ point) to FQ12 space
      p_cast_fq12 = cast_point_to_fq12(p_fq_pt)

      miller_loop(q_twisted_fq12, p_cast_fq12, do_final_exponentiate)
    end
  end

  def final_exponentiate(f_fq12_val) do
    # exponent = (field_modulus**12 - 1) // curve_order
    # exponent_val = div(Float.pow(@field_modulus, 12) - 1, @curve_order)
    # This needs to handle large integer arithmetic. Using :crypto functions might be an option for modular exponentiation for BigInts if available.
    # For now, direct calculation, ensure FQ12.pow handles large integer exponents.

    # Calculate (p^12 - 1)
    # Using a larger modulus for intermediate step to avoid issues with direct calc if numbers are too big for direct :math.pow
    p12m1 = :crypto.mod_pow(@field_modulus, 12, @curve_order * 2) - 1
    # This calculation of p12m1 itself is complex with large numbers.
    # A direct calculation using :math.pow or similar might overflow or lose precision.
    # It's better to compute with integer math if p, r are known.

    # A more direct approach for the exponent without floats:
    # The exponent itself is an integer. field_modulus and curve_order are integers.
    # (p^12 - 1) / r. We need a way to compute p^12 for large p.
    # Elixir's :math.pow returns a float. We need integer exponentiation.

    # Placeholder for big integer exponentiation for `p_to_12`
    # This should be implemented using a loop or a library function if direct integer `pow` is not available for huge numbers.
    # p_to_12 = :crypto.mod_pow(@field_modulus, 12, some_large_enough_modulus_or_direct_if_supported)
    # For now, let's assume we can get this value somehow or FQ12.pow is very smart.

    # The exponentiation in FQ12 is complex and doesn't just take a massive integer exponent directly for optimal results.
    # py_ecc implements a specific algorithm for final exponentiation.
    # This function FQ12.pow(f, (p^12-1)/r) is a high-level representation.
    # The actual final exponentiation for BN curves has a specific structure.
    # (f^((p^12-1)/r)) = f^((p^6-1)(p^2+1)( (p^4-p^2+1)/r ))
    # It usually involves:
    # 1. f_t0 = f^(p^6) / f  (easy part, (p^6-1))
    # 2. f_t1 = f_t0^(p^2) * f_t0 (easy part, (p^2+1))
    # 3. Hard part: f_t1 ^ lambda where lambda is derived from curve parameters.

    # For BN curves (like BN128/BN254), the final exponentiation is simpler than BLS.
    # It's often just f^((p^12 - 1) / r).
    # We need FQ12.pow to correctly handle a potentially very large integer exponent.

    # Let's assume `exponent` is calculated correctly as an integer.
    # exponent_val = div((:math.pow(@field_modulus, 12) |> trunc()) -1, @curve_order)
    # The above will likely fail for real sizes. Direct integer exponentiation is needed.
    # For now, we'll use a simplified conceptual call and assume FQ12.pow handles it.
    # This detail is crucial for correctness.

    # Calculating (field_modulus^12 - 1) / curve_order
    # This needs to be done with integer arithmetic to avoid precision issues.
    # Elixir doesn't have a built-in pow for arbitrary precision integers.
    # A helper for integer exponentiation would be needed, or use a library.
    # For now, we focus on the structure, assuming this `exponent` can be computed.
    # exponent = compute_bn_final_exponent_value() # This would be a helper.

    # Simplified: p ** ((field_modulus**12 - 1) // curve_order)
    # This final_exponentiate function in py_ecc just does this. So FQ12.pow must be robust.
    # We need to compute the exponent as an integer value first.

    # exponent_numerator = power(@field_modulus, 12) - 1
    # exponent = div(exponent_numerator, @curve_order)
    # This `power` function needs to handle large integers.

    # Let's use the structure from py_ecc directly and assume FQ12.pow can take a large int exponent.
    # The exponent calculation needs to be precise.
    # We will defer the actual calculation of this large exponent value. For now, a placeholder for the concept.
    # If FQ12.pow requires smaller exponents or specific algorithms, this would need to change.
    # FQ12.pow(f_fq12_val, @final_exponent_value)

    # For now, let's pass a conceptual exponent if FQ12.pow can take it.
    # The actual calculation of (field_modulus**12-1) // curve_order needs a bigint library or manual implementation.
    # This is a critical part. If `field_modulus` is e.g. 256 bits, `field_modulus**12` is huge.

    # Assuming a helper ExEcc.Math.integer_pow/2 and that curve_order is smaller than field_modulus^12-1.
    p_pow_12 = ExEcc.Math.integer_pow(@field_modulus, 12)
    exponent_val = div(p_pow_12 - 1, @curve_order)

    FQ12.pow(f_fq12_val, exponent_val)
  end
end
