defmodule ExEcc.Bls12_381.Bls12381Pairing do
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ
  alias ExEcc.Fields.FQ2
  alias ExEcc.Fields.FQ12
  alias ExEcc.Bls12_381.Bls12381Curve, as: Curve

  # field_modulus = field_properties["bls12_381"]["field_modulus"]
  # This will be taken from the FQ module if needed, or assumed to be part of FQ12 operations.
  # For final_exponentiate, it might be needed explicitly if FQ12.pow doesn't handle large exponents with internal modulus.

  @ate_loop_count 1_513_237_622_294_164_275_2
  @log_ate_loop_count 62

  # Helper to get the field modulus
  defp field_modulus, do: Curve.field_modulus()

  # Create a function representing the line between P1 and P2,
  # and evaluate it at T
  def linefunc(p1, p2, t) do
    # Assuming points are {x, y} tuples of FQ elements for G1 points
    # or FQ2 elements for G2 points after twisting and casting.
    if is_nil(p1) or is_nil(p2) or is_nil(t) do
      raise "Invalid input - no points-at-infinity allowed"
    end

    {x1, y1} = p1
    {x2, y2} = p2
    {xt, yt} = t

    # Determine field type from one of the point coordinates
    field_module = elem_module(x1)

    cond do
      not field_module.eq(x1, x2) ->
        # m = (y2 - y1) / (x2 - x1)
        m = field_module.divide(field_module.sub(y2, y1), field_module.sub(x2, x1))
        # return m * (xt - x1) - (yt - y1)
        field_module.sub(field_module.mul(m, field_module.sub(xt, x1)), field_module.sub(yt, y1))

      # x1 == x2 is implied by falling through the first condition
      field_module.eq(y1, y2) ->
        # m = 3 * x1**2 / (2 * y1)
        m =
          field_module.divide(
            field_module.mul(field_module.new_fq(3, field_modulus()), field_module.pow(x1, 2)),
            field_module.mul(field_module.new_fq(2, field_modulus()), y1)
          )

        # return m * (xt - x1) - (yt - y1)
        field_module.sub(field_module.mul(m, field_module.sub(xt, x1)), field_module.sub(yt, y1))

      # x1 == x2 and y1 != y2 (P1 and P2 are inverses, line is vertical)
      true ->
        field_module.sub(xt, x1)
    end
  end

  def cast_point_to_fq12({x, y}) when is_struct(x, FQ) and is_struct(y, FQ) do
    # Create FQ12 elements with x.n and y.n as the first coefficients
    x_fq12 = FQ12.new([x.n] ++ List.duplicate(0, 5), field_modulus())
    y_fq12 = FQ12.new([y.n] ++ List.duplicate(0, 5), field_modulus())
    {x_fq12, y_fq12}
  end

  def cast_point_to_fq12(nil), do: nil

  # Consistency checks from Python - these would typically be in a test suite.
  # one = Curve.g1()
  # two = Curve.double(one)
  # three = Curve.multiply(one, 3)
  # negone = Curve.multiply(one, Curve.curve_order() - 1)
  # negtwo = Curve.multiply(one, Curve.curve_order() - 2)
  # negthree = Curve.multiply(one, Curve.curve_order() - 3)
  #
  # conditions = [
  #   FQ.eq(linefunc(one, two, one), FQ.new(0)),
  #   FQ.eq(linefunc(one, two, two), FQ.new(0)),
  #   not FQ.eq(linefunc(one, two, three), FQ.new(0)),
  #   FQ.eq(linefunc(one, two, negthree), FQ.new(0)),
  #   FQ.eq(linefunc(one, negone, one), FQ.new(0)),
  #   FQ.eq(linefunc(one, negone, negone), FQ.new(0)),
  #   not FQ.eq(linefunc(one, negone, two), FQ.new(0)),
  #   FQ.eq(linefunc(one, one, one), FQ.new(0)), # This case implies double(one)
  #   not FQ.eq(linefunc(one, one, two), FQ.new(0)),
  #   FQ.eq(linefunc(one, one, negtwo), FQ.new(0))
  # ]
  #
  # if not Enum.all?(conditions) do
  #   raise "Line function is inconsistent"
  # end

  # Main miller loop
  def miller_loop(q_fq12, p_fq12) do
    if is_nil(q_fq12) or is_nil(p_fq12) do
      FQ12.new([1] ++ List.duplicate(0, 5), field_modulus())
    else
      # R starts as Q
      # f starts as FQ12.one()
      {_final_r, final_f} =
        Enum.reduce(@log_ate_loop_count..0//-1, {q_fq12, FQ12.new([1] ++ List.duplicate(0, 5), field_modulus())}, fn i, {r_acc, f_acc} ->
          # f = f * f * linefunc(R, R, P)
          f_doubled = FQ12.mul(f_acc, f_acc)
          f_new = FQ12.mul(f_doubled, linefunc(r_acc, r_acc, p_fq12))
          # double function needs to handle FQ12 points
          r_new = Curve.double(r_acc)

          if Bitwise.band(@ate_loop_count, Bitwise.bsl(1, i)) != 0 do
            # f = f * linefunc(R, Q, P)
            f_updated = FQ12.mul(f_new, linefunc(r_new, q_fq12, p_fq12))
            # add function needs to handle FQ12 points
            r_updated = Curve.add(r_new, q_fq12)
            {r_updated, f_updated}
          else
            {r_new, f_new}
          end
        end)

      # Final exponentiation
      exponent = div(trunc(:math.pow(field_modulus(), 12)) - 1, Curve.curve_order())
      FQ12.pow(final_f, exponent)
    end
  end

  # Pairing computation
  def pairing(q_fq2, p_fq) do
    # Twisted Q point (FQ12)
    q_twisted_fq12 = Curve.twist(q_fq2)
    # P point cast to FQ12
    p_cast_fq12 = cast_point_to_fq12(p_fq)

    if is_nil(q_twisted_fq12) or is_nil(p_cast_fq12) do
      # If twist or cast results in nil (e.g. from point at infinity)
      # typically the pairing result is FQ12.one()
      FQ12.new([1] ++ List.duplicate(0, 5), field_modulus())
    else
      miller_loop(q_twisted_fq12, p_cast_fq12)
    end
  end

  # Final exponentiation (often part of the pairing but can be separate)
  def final_exponentiate(f_val) when is_struct(f_val, FQ12) do
    # exponent = (field_modulus**12 - 1) // curve_order
    # This is a very large number. FQ12.pow must handle it.
    field_modulus_val = field_modulus()
    exponent_val = div(trunc(:math.pow(field_modulus_val, 12)) - 1, Curve.curve_order())
    FQ12.pow(f_val, exponent_val)
  end

  defp elem_module(elem) do
    case elem do
      %FQ{} -> FQ
      %FQ2{} -> FQ2
      %FQ12{} -> FQ12
      _ -> raise "Invalid element type"
    end
  end
end
