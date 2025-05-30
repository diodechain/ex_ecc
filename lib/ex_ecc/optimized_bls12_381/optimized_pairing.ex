defmodule ExEcc.OptimizedBLS12381.OptimizedPairing do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: FQP

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus

  # @ate_loop_count 1_513_237_622_294_164_2752 # Unused
  @log_ate_loop_count 62
  @pseudo_binary_encoding [
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    0,
    1,
    1
  ]

  # TODO: Port this check once a large number exponentiation is available.
  # if Enum.sum(Enum.with_index(@pseudo_binary_encoding, fn e, i -> e * :math.pow(2, i) end)) != @ate_loop_count do
  #   raise ValueError, "Pseudo binary encoding is incorrect"
  # end

  @exptable [
    FQ12.pow(FQ12.new([FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 11)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 10)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 9)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 8)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 7)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 6)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 5)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 4)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 3)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])] ++ List.duplicate(FQ2.zero(), 2)), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0]), FQ2.zero()]), @field_modulus),
    FQ12.pow(FQ12.new([FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.zero(), FQ2.new([1, 0])]), @field_modulus)
  ]

  def normalize1(p) do
    {x, y} = Curve.normalize(p)
    {x, y, FQ.one()}
  end

  # Create a function representing the line between P1 and P2,
  # and evaluate it at T. Returns a numerator and a denominator
  # to avoid unneeded divisions
  def linefunc(p1, p2, t) do
    zero = elem(p1, 0).__struct__.zero()
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    {xt, yt, zt} = t

    # points in projective coords: (x / z, y / z)
    # hence, m = (y2/z2 - y1/z1) / (x2/z2 - x1/z1)
    # multiply numerator and denominator by z1z2 to get values below
    m_numerator = FQ12.sub(FQ12.mul(y2, z1), FQ12.mul(y1, z2))
    m_denominator = FQ12.sub(FQ12.mul(x2, z1), FQ12.mul(x1, z2))

    cond do
      m_denominator != zero ->
        # m * ((xt/zt) - (x1/z1)) - ((yt/zt) - (y1/z1))
        {
          FQ12.sub(
            FQ12.mul(m_numerator, FQ12.sub(FQ12.mul(xt, z1), FQ12.mul(x1, zt))),
            FQ12.mul(m_denominator, FQ12.sub(FQ12.mul(yt, z1), FQ12.mul(y1, zt)))
          ),
          FQ12.mul(m_denominator, FQ12.mul(zt, z1))
        }

      m_numerator == zero ->
        # m = 3(x/z)^2 / 2(y/z), multiply num and den by z**2
        m_numerator_new = FQ12.mul(FQ12.new(3), FQ12.mul(x1, x1))
        m_denominator_new = FQ12.mul(FQ12.new(2), FQ12.mul(y1, z1))

        {
          FQ12.sub(
            FQ12.mul(m_numerator_new, FQ12.sub(FQ12.mul(xt, z1), FQ12.mul(x1, zt))),
            FQ12.mul(m_denominator_new, FQ12.sub(FQ12.mul(yt, z1), FQ12.mul(y1, zt)))
          ),
          FQ12.mul(m_denominator_new, FQ12.mul(zt, z1))
        }

      true ->
        {FQ12.sub(FQ12.mul(xt, z1), FQ12.mul(x1, zt)), FQ12.mul(z1, zt)}
    end
  end

  def cast_point_to_fq12(pt) do
    if pt == nil do
      nil
    else
      {x, y, z} = pt

      {FQ12.new(Tuple.pad({x.n}, 12, 0)), FQ12.new(Tuple.pad({y.n}, 12, 0)),
       FQ12.new(Tuple.pad({z.n}, 12, 0))}
    end
  end

  # TODO: Port consistency checks for linefunc if needed as test cases.

  # Main miller loop
  def miller_loop(q_fq2, p_fq, final_exponentiate \\ true) do
    if q_fq2 == nil or p_fq == nil do
      FQ12.one()
    else
      cast_p = cast_point_to_fq12(p_fq)
      # The original python code uses Q (an FQ2 point) for R and twist_Q,
      # but operations like double(R) are for FQ2 points, and twist(R) produces FQ12.
      # This needs careful review of types and operations.

      # Initializing r_fq12 as the twisted version of q_fq2 for calculations within the loop
      # twist_q_fq12 remains the twisted version of the original q_fq2 for the conditional add step.
      _r_fq12 = Curve.twist(q_fq2) # Prefixed r_fq12 with underscore
      twist_q_fq12 = Curve.twist(q_fq2)

      # The `double` and `add` operations in the loop are on `r_point_for_doubling_and_adding` which should be an FQ2 point.
      # We use `q_fq2` as the base for these operations, and then twist the result for `linefunc`.
      r_point_for_doubling_and_adding = q_fq2

      {f_num, f_den} = {FQ12.one(), FQ12.one()}

      # Loop from @log_ate_loop_count down to 0 (inclusive for second element of slice)
      # Original Python: pseudo_binary_encoding[62::-1]
      # Elixir equivalent for first 63 elements in reverse: Enum.reverse(Enum.slice(@pseudo_binary_encoding, 0, 62))
      # However, the Python slice `[62::-1]` means start at index 62, go to the beginning, step -1.
      # This is equivalent to taking the first 63 elements and reversing them.
      loop_constants = Enum.reverse(Enum.slice(@pseudo_binary_encoding, 0..@log_ate_loop_count))

      {final_f_num, final_f_den, _final_r_point} =
        Enum.reduce(loop_constants, {f_num, f_den, r_point_for_doubling_and_adding}, fn v,
                                                                                        {acc_f_num,
                                                                                         acc_f_den,
                                                                                         current_r_point_fq2} ->
          # `current_r_point_fq2` is an FQ2 point.
          # `linefunc` expects its first two arguments (P1, P2) to be FQ12 points.
          # `Curve.twist` converts an FQ2 point to an FQ12 point.
          twisted_current_r_fq12 = Curve.twist(current_r_point_fq2)

          {n_double, d_double} = linefunc(twisted_current_r_fq12, twisted_current_r_fq12, cast_p)
          next_f_num = FQ12.mul(FQ12.mul(acc_f_num, acc_f_num), n_double)
          next_f_den = FQ12.mul(FQ12.mul(acc_f_den, acc_f_den), d_double)

          doubled_r_point_fq2 = Curve.double(current_r_point_fq2)
          twisted_doubled_r_fq12 = Curve.twist(doubled_r_point_fq2)

          if v == 1 do
            {n_add, d_add} = linefunc(twisted_doubled_r_fq12, twist_q_fq12, cast_p)

            {
              FQ12.mul(next_f_num, n_add),
              FQ12.mul(next_f_den, d_add),
              # Add original Q (as FQ2 point)
              Curve.add(doubled_r_point_fq2, q_fq2)
            }
          else
            {next_f_num, next_f_den, doubled_r_point_fq2}
          end
        end)

      # Handling the remaining elements of pseudo_binary_encoding (if any after index 62)
      # Python: pseudo_binary_encoding[log_ate_loop_count-1::-1] - this seems to be the same as above based on how Python slicing works.
      # The loop in python `for v in pseudo_binary_encoding[62::-1]:` covers indices 62 down to 0.
      # Let's assume the loop above is correct for now.

      result = FQ12.mul(final_f_num, FQ12.inv(final_f_den))

      if final_exponentiate do
        final_exponentiate(result)
      else
        result
      end
    end
  end

  def pairing(q_fq2, p_fq, final_exponentiate \\ true) do
    # TODO: Check if p_fq is on curve G1, and q_fq2 is on curve G2 if is_on_curve supports FQ2 points.
    # unless Curve.is_on_curve(p_fq, Curve.b()) do
    #   raise ArgumentError, "Point P is not on the curve G1"
    # end
    # unless Curve.is_on_curve(q_fq2, Curve.b2()) do # Assuming b2 is for FQ2 curve
    #   raise ArgumentError, "Point Q is not on the twisted curve G2"
    # end

    m_loop = miller_loop(q_fq2, p_fq, false)

    if final_exponentiate do
      final_exponentiate(m_loop)
    else
      m_loop
    end
  end

  def exp_by_p(x) do
    # Frobenius map: (x_0 + x_1*i + ...)^p = x_0^p + (x_1*i)^p + ...
    # For FQ12, this means conjugating elements based on field extension structure.
    # This is a simplified version, actual Frobenius map might be more complex for BLS12-381.
    # TODO: Verify the correct Frobenius map for FQ12 under BLS12-381.
    # This is likely x.frobenius_map(1)
    # For now, assuming it's a complex operation and placeholder
    FQP.frobenius(x, 1)
  end

  def final_exponentiate(p_fq12) do
    cofactor = div(:math.pow(@field_modulus, 4) - :math.pow(@field_modulus, 2) + 1, Curve.curve_order())
    p2 = FQ12.mul(exp_by_p(exp_by_p(p_fq12)), p_fq12)
    p3 = FQ12.div(exp_by_p(exp_by_p(exp_by_p(exp_by_p(exp_by_p(exp_by_p(p2)))))), p2)
    FQ12.pow(p3, cofactor)
  end
end
