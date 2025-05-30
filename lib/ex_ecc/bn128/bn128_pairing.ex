defmodule ExEcc.Bn128.Pairing do
  alias ExEcc.Fields.Bn128FQ, as: FQ
  alias ExEcc.Fields.Bn128FQ2, as: FQ2
  alias ExEcc.Fields.Bn128FQ12, as: FQ12
  alias ExEcc.Bn128.Curve, as: Curve

  # field_modulus will be accessed via FQ.field_modulus() or similar
  @ate_loop_count 29_793_968_203_157_093_288
  @log_ate_loop_count 63

  # Placeholder for actual modulus access
  defp field_modulus, do: FQ.field_modulus()

  def linefunc(p1, p2, t) do
    if is_nil(p1) or is_nil(p2) or is_nil(t) do
      raise "Invalid input - no points-at-infinity allowed"
    end

    {x1, y1} = p1
    {x2, y2} = p2
    {xt, yt} = t

    # Determine field module based on point x1's type (simplified)
    FieldMath =
      cond do
        is_struct(x1, FQ) -> FQ
        # Used for G2 points directly in linefunc?
        is_struct(x1, FQ2) -> FQ2
        # Used for twisted G2 points
        is_struct(x1, FQ12) -> FQ12
        # Default, or raise error
        true -> FQ
      end

    cond do
      not FieldMath.eq(x1, x2) ->
        m = FieldMath.div(FieldMath.sub(y2, y1), FieldMath.sub(x2, x1))
        FieldMath.sub(FieldMath.mul(m, FieldMath.sub(xt, x1)), FieldMath.sub(yt, y1))

      FieldMath.eq(y1, y2) ->
        m =
          FieldMath.div(
            FieldMath.mul(FieldMath.new(3), FieldMath.pow(x1, 2)),
            FieldMath.mul(FieldMath.new(2), y1)
          )

        FieldMath.sub(FieldMath.mul(m, FieldMath.sub(xt, x1)), FieldMath.sub(yt, y1))

      true ->
        FieldMath.sub(xt, x1)
    end
  end

  def cast_point_to_fq12({x, y}) when is_struct(x, FQ) and is_struct(y, FQ) do
    x_fq12 = FQ12.new(List.duplicate(0, 12) |> List.replace_at(0, x.n))
    y_fq12 = FQ12.new(List.duplicate(0, 12) |> List.replace_at(0, y.n))
    {x_fq12, y_fq12}
  end

  def cast_point_to_fq12(nil), do: nil

  # Consistency checks (typically in tests)
  # ... (omitted for brevity, similar to bls12_381_pairing.ex)

  def miller_loop(q_fq12, p_fq12) do
    if is_nil(q_fq12) or is_nil(p_fq12) do
      FQ12.one()
    else
      {r_final, f_final} =
        Enum.reduce(@log_ate_loop_count..0//-1, {q_fq12, FQ12.one()}, fn i, {r_acc, f_acc} ->
          f_doubled = FQ12.mul(f_acc, f_acc)
          # linefunc expects FQ12 points here
          f_new = FQ12.mul(f_doubled, linefunc(r_acc, r_acc, p_fq12))
          # Curve.double must handle FQ12 points
          r_new = Curve.double(r_acc)

          if Bitwise.band(@ate_loop_count, Bitwise.bsl(1, i)) != 0 do
            f_updated = FQ12.mul(f_new, linefunc(r_new, q_fq12, p_fq12))
            # Curve.add must handle FQ12 points
            r_updated = Curve.add(r_new, q_fq12)
            {r_updated, f_updated}
          else
            {r_new, f_new}
          end
        end)

      # Additional steps from Python's bn128 miller_loop
      # Q1 = (Q[0] ** field_modulus, Q[1] ** field_modulus)
      # nQ2 = (Q1[0] ** field_modulus, -Q1[1] ** field_modulus)
      # These involve FQ12.pow(element, field_modulus_integer) and neg operation.
      # Need to ensure FQ12 supports these correctly.
      q1_x = FQ12.pow(elem(r_final, 0), field_modulus())
      q1_y = FQ12.pow(elem(r_final, 1), field_modulus())
      q1 = {q1_x, q1_y}
      # TODO: Curve.is_on_curve(q1, Curve.b12()) check if b12 is accessible

      nq2_x = FQ12.pow(q1_x, field_modulus())
      # FQ12.neg
      nq2_y = FQ12.neg(FQ12.pow(q1_y, field_modulus()))
      nq2 = {nq2_x, nq2_y}
      # TODO: Curve.is_on_curve(nq2, Curve.b12()) check

      f_after_q1 = FQ12.mul(f_final, linefunc(r_final, q1, p_fq12))
      # r_final is the R from the loop end
      r_after_q1 = Curve.add(r_final, q1)

      f_after_nq2 = FQ12.mul(f_after_q1, linefunc(r_after_q1, nq2, p_fq12))
      # R = add(R, nQ2) # Python notes this does nothing, so omitted.

      exponent = div(FQ12.pow(FQ12.new(field_modulus()), 12) - 1, Curve.curve_order())
      FQ12.pow(f_after_nq2, exponent)
    end
  end

  def pairing(q_fq2, p_fq) do
    # if not Curve.is_on_curve(q_fq2, Curve.b2()), do: raise "Q not on curve"
    # if not Curve.is_on_curve(p_fq, Curve.b()), do: raise "P not on curve"

    q_twisted_fq12 = Curve.twist(q_fq2)
    p_cast_fq12 = cast_point_to_fq12(p_fq)

    if is_nil(q_twisted_fq12) or is_nil(p_cast_fq12) do
      FQ12.one()
    else
      miller_loop(q_twisted_fq12, p_cast_fq12)
    end
  end

  def final_exponentiate(f_val) when is_struct(f_val, FQ12) do
    fm = field_modulus()
    exponent_val = div(trunc(:math.pow(fm, 12)) - 1, Curve.curve_order())
    FQ12.pow(f_val, exponent_val)
  end
end
