defmodule ExEcc.OptimizedBLS12381.OptimizedPairing do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.Fields.FieldProperties
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.FieldMath

  @field_modulus FieldProperties.field_properties()["bls12_381"]["field_modulus"]
  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  @ate_loop_count 15_132_376_222_941_642_752
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

  # Verify pseudo binary encoding
  if Enum.sum(
       Enum.with_index(@pseudo_binary_encoding)
       |> Enum.map(fn {e, i} -> e * :math.pow(2, i) end)
     ) != @ate_loop_count do
    raise "Pseudo binary encoding is incorrect"
  end

  def normalize1(p) do
    {x, y} = Curve.normalize(p)
    {x, y, FQ.one()}
  end

  # Create a function representing the line between P1 and P2,
  # and evaluate it at T. Returns a numerator and a denominator
  # to avoid unneeded divisions
  def linefunc(p1, p2, t) do
    {x1, y1, z1} = p1
    zero = FieldMath.zero(x1)
    {x2, y2, z2} = p2
    {xt, yt, zt} = t

    # points in projective coords: (x / z, y / z)
    # hence, m = (y2/z2 - y1/z1) / (x2/z2 - x1/z1)
    # multiply numerator and denominator by z1z2 to get values below
    m_numerator = FieldMath.sub(FieldMath.mul(y2, z1), FieldMath.mul(y1, z2))
    m_denominator = FieldMath.sub(FieldMath.mul(x2, z1), FieldMath.mul(x1, z2))

    cond do
      not FieldMath.eq(m_denominator, zero) ->
        # m * ((xt/zt) - (x1/z1)) - ((yt/zt) - (y1/z1))
        {
          FieldMath.sub(
            FieldMath.mul(
              m_numerator,
              FieldMath.sub(FieldMath.mul(xt, z1), FieldMath.mul(x1, zt))
            ),
            FieldMath.mul(
              m_denominator,
              FieldMath.sub(FieldMath.mul(yt, z1), FieldMath.mul(y1, zt))
            )
          ),
          FieldMath.mul(FieldMath.mul(m_denominator, zt), z1)
        }

      FieldMath.eq(m_numerator, zero) ->
        # m = 3(x/z)^2 / 2(y/z), multiply num and den by z**2
        m_numerator = FieldMath.mul(FieldMath.mul(FQ.new(3), x1), x1)
        m_denominator = FieldMath.mul(FieldMath.mul(FQ.new(2), y1), z1)

        {
          FieldMath.sub(
            FieldMath.mul(
              m_numerator,
              FieldMath.sub(FieldMath.mul(xt, z1), FieldMath.mul(x1, zt))
            ),
            FieldMath.mul(
              m_denominator,
              FieldMath.sub(FieldMath.mul(yt, z1), FieldMath.mul(y1, zt))
            )
          ),
          FieldMath.mul(FieldMath.mul(m_denominator, zt), z1)
        }

      true ->
        {FieldMath.sub(FieldMath.mul(xt, z1), FieldMath.mul(x1, zt)), FieldMath.mul(z1, zt)}
    end
  end

  def cast_point_to_fq12(pt) do
    if is_nil(pt) do
      nil
    else
      {x, y, z} = pt

      {
        FQ12.new([x.n] ++ List.duplicate(0, 11)),
        FQ12.new([y.n] ++ List.duplicate(0, 11)),
        FQ12.new([z.n] ++ List.duplicate(0, 11))
      }
    end
  end

  # Main miller loop
  def miller_loop(q, p, final_exponentiate \\ true) do
    if is_nil(q) or is_nil(p) do
      FQ12.one()
    else
      cast_p = cast_point_to_fq12(p)
      twist_r = twist_q = Curve.twist(q)
      r = q
      f_num = f_den = FQ12.one()

      # Process pseudo binary encoding in reverse
      Enum.reduce(
        Enum.reverse(Enum.take(@pseudo_binary_encoding, 63)),
        {f_num, f_den, r, twist_r},
        fn v, {f_num, f_den, r, twist_r} ->
          {n, d} = linefunc(twist_r, twist_r, cast_p)
          f_num = FieldMath.mul(FieldMath.mul(f_num, f_num), n)
          f_den = FieldMath.mul(FieldMath.mul(f_den, f_den), d)
          r = Curve.double(r)
          twist_r = Curve.twist(r)

          if v == 1 do
            {n, d} = linefunc(twist_r, twist_q, cast_p)
            f_num = FieldMath.mul(f_num, n)
            f_den = FieldMath.mul(f_den, d)
            r = Curve.add(r, q)
            twist_r = Curve.twist(r)
            {f_num, f_den, r, twist_r}
          else
            {f_num, f_den, r, twist_r}
          end
        end
      )
      |> then(fn {f_num, f_den, _r, _twist_r} ->
        f = FieldMath.div(f_num, f_den)

        if final_exponentiate do
          FieldMath.pow(f, div(:math.pow(@field_modulus, 12) - 1, @curve_order))
        else
          f
        end
      end)
    end
  end

  # Pairing computation
  def pairing(q, p, final_exponentiate \\ true) do
    if not Curve.is_on_curve(q, Curve.b2()) do
      raise "Invalid input - point Q is not on the correct curve"
    end

    if not Curve.is_on_curve(p, Curve.b()) do
      raise "Invalid input - point P is not on the correct curves"
    end

    if Curve.is_inf(p) or Curve.is_inf(q) do
      FQ12.one()
    else
      miller_loop(q, p, final_exponentiate)
    end
  end

  # Precompute exponentiation table
  @exptable Enum.map(0..11, fn i ->
              FQ12.new(List.duplicate(0, i) ++ [1] ++ List.duplicate(0, 11 - i))
              |> FieldMath.pow(@field_modulus)
            end)

  def exp_by_p(x) do
    Enum.zip(@exptable, x.coeffs)
    |> Enum.reduce(FQ12.zero(), fn {table_entry, coeff}, acc ->
      FieldMath.add(acc, FieldMath.mul(table_entry, coeff))
    end)
  end

  def final_exponentiate(p) do
    cofactor = div(:math.pow(@field_modulus, 4) - :math.pow(@field_modulus, 2) + 1, @curve_order)
    p2 = FieldMath.mul(exp_by_p(exp_by_p(p)), p)

    p3 =
      FieldMath.div(
        exp_by_p(exp_by_p(exp_by_p(exp_by_p(exp_by_p(exp_by_p(p2)))))),
        p2
      )

    FieldMath.pow(p3, cofactor)
  end
end
