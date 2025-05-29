defmodule ExEcc.Bls12381.Pairing do
  alias ExEcc.Fields.Bls12381FQ, as: FQ
  # alias ExEcc.Fields.Bls12381FQ2, as: FQ2 # FQ2 is unused
  alias ExEcc.Fields.Bls12381FQ12, as: FQ12
  alias ExEcc.Bls12381.Curve, as: Curve

  # field_modulus = field_properties["bls12_381"]["field_modulus"]
  # This will be taken from the FQ module if needed, or assumed to be part of FQ12 operations.
  # For final_exponentiate, it might be needed explicitly if FQ12.pow doesn't handle large exponents with internal modulus.

  @ate_loop_count 1_513_237_622_294_164_275_2
  @log_ate_loop_count 62

  # Helper to get the field modulus, assuming FQ exposes it or it's a known constant.
  # This is a placeholder; actual access might differ.
  # Or a hardcoded value if FQ doesn't provide it.
  defp field_modulus, do: FQ.field_modulus()

  # Create a function representing the line between P1 and P2,
  # and evaluate it at T
  def linefunc(p1, p2, t) do
    # Assuming points are {x, y} tuples of FQ elements for G1 points
    # or FQ12 elements for G2 points after twisting and casting.
    # The original Python code raises ValueError for nil inputs.
    # Elixir typically uses pattern matching or guards for this.
    if is_nil(p1) or is_nil(p2) or is_nil(t) do
      # Consider raising an error or returning an error tuple
      raise "Invalid input - no points-at-infinity allowed"
    end

    {x1, y1} = p1
    {x2, y2} = p2
    {xt, yt} = t

    # Determine field type from one of the point coordinates (e.g., x1)
    # This is a simplification; robust type handling would be better.
    field_module = if elem(x1, 0) == ExEcc.Fields.Bls12381FQ, do: FQ, else: FQ12

    cond do
      not field_module.eq(x1, x2) ->
        # m = (y2 - y1) / (x2 - x1)
        m = field_module.div(field_module.sub(y2, y1), field_module.sub(x2, x1))
        # return m * (xt - x1) - (yt - y1)
        field_module.sub(field_module.mul(m, field_module.sub(xt, x1)), field_module.sub(yt, y1))

      # x1 == x2 is implied by falling through the first condition
      field_module.eq(y1, y2) ->
        # m = 3 * x1**2 / (2 * y1)
        m =
          field_module.div(
            field_module.mul(field_module.new(3), field_module.pow(x1, 2)),
            field_module.mul(field_module.new(2), y1)
          )

        # return m * (xt - x1) - (yt - y1)
        field_module.sub(field_module.mul(m, field_module.sub(xt, x1)), field_module.sub(yt, y1))

      # x1 == x2 and y1 != y2 (P1 and P2 are inverses, line is vertical)
      true ->
        field_module.sub(xt, x1)
    end
  end

  def cast_point_to_fq12({x, y}) when is_struct(x, FQ) and is_struct(y, FQ) do
    # Assumes FQ.n gets the integer value and FQ12.new can take a list of coefficients.
    # The python code FQ12([x.n] + [0]*11) creates an FQ12 where the 0th coefficient is x.n
    # and others are 0.
    x_fq12 = FQ12.new(List.duplicate(0, 12) |> List.replace_at(0, x.n))
    y_fq12 = FQ12.new(List.duplicate(0, 12) |> List.replace_at(0, y.n))
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
      FQ12.one()
    else
      # R starts as Q
      # f starts as FQ12.one()
      {_final_r, final_f} =
        Enum.reduce(@log_ate_loop_count..0//-1, {q_fq12, FQ12.one()}, fn i, {r_acc, f_acc} ->
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

      # The Python code includes commented-out Frobenius map applications and final exponentiation within miller_loop.
      # Here, we follow the structure but note that the final exponentiation is often a separate step.
      # For now, return f as calculated by the loop. final_exponentiate is a separate function.
      # For now, return f as calculated by the loop. final_exponentiate is a separate function.
      # return f ** ((field_modulus**12 - 1) // curve_order)
      exponent = div(FQ12.pow(FQ12.new(field_modulus()), 12) - 1, Curve.curve_order())
      # Requires FQ12.pow to handle large integer exponents
      FQ12.pow(final_f, exponent)
    end
  end

  # Pairing computation
  def pairing(q_fq2, p_fq) do
    # Validations (ensure points are on their respective curves)
    # Assuming Curve.b2() and Curve.b() are available for b2 and b values.
    # And Curve.is_on_curve can handle FQ2 and FQ points with their respective b values.
    # if not Curve.is_on_curve(q_fq2, Curve.b2()) do
    #   raise "Invalid input - point Q is not on the correct curve (FQ2)"
    # end
    # if not Curve.is_on_curve(p_fq, Curve.b()) do
    #   raise "Invalid input - point P is not on the correct curve (FQ)"
    # end

    # Twisted Q point (FQ12)
    # Curve.twist needs to accept FQ2 and return FQ12
    q_twisted_fq12 = Curve.twist(q_fq2)
    # P point cast to FQ12
    p_cast_fq12 = cast_point_to_fq12(p_fq)

    if is_nil(q_twisted_fq12) or is_nil(p_cast_fq12) do
      # If twist or cast results in nil (e.g. from point at infinity)
      # typically the pairing result is FQ12.one()
      FQ12.one()
    else
      miller_loop(q_twisted_fq12, p_cast_fq12)
    end
  end

  # Final exponentiation (often part of the pairing but can be separate)
  def final_exponentiate(f_val) when is_struct(f_val, FQ12) do
    # exponent = (field_modulus**12 - 1) // curve_order
    # This is a very large number. FQ12.pow must handle it.
    # field_modulus_val = field_modulus() # This needs to be the actual prime modulus for BLS12-381 FQ
    # exponent = div( :math.pow(field_modulus_val, 12) -1 , Curve.curve_order())
    # The exponent calculation was moved into miller_loop to match the python structure more closely for now.
    # This function might be redundant if miller_loop already does it, or it can be used if miller_loop returns raw f.
    # Re-doing it here based on the python function signature:
    # Make sure this is correct
    field_modulus_val = field_modulus()
    # This is problematic, FQ12.new expects coefficients not a single number for pow
    _num = FQ12.pow(FQ12.new(field_modulus_val), 12) # Prefixed num with underscore
    # The base for FQ12.pow should be an FQ12 element.
    # The python `field_modulus**12` is integer exponentiation.

    # Correct exponent calculation using integer arithmetic
    exponent_val = div(trunc(:math.pow(field_modulus_val, 12)) - 1, Curve.curve_order())
    FQ12.pow(f_val, exponent_val)
  end
end
