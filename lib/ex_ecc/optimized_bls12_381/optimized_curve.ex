defmodule ExEcc.OptimizedBLS12381.OptimizedCurve do
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ # Optimized FQ
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: FQP # Optimized FQP
  alias ExEcc.Fields.FieldProperties

  @field_modulus FieldProperties.field_properties()["bls12_381"].field_modulus
  @fq2_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
  @fq12_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  @b FQ.new_fq(4, @field_modulus)

  # Original @b2 FQ2.new({4, 4}) -> FQP.new_fqp([FQ.new_fq(4, @fm), FQ.new_fq(4, @fm)], @fq2_coeffs, @fm)
  @b2_coeffs [FQ.new_fq(4, @field_modulus), FQ.new_fq(4, @field_modulus)]
  # The FQP in OptimizedFieldElements stores coeffs as integers, so convert FQ structs to integers here.
  @b2_int_coeffs Enum.map(@b2_coeffs, &(&1.n))
  @b2 FQP.new_fqp(@b2_int_coeffs, @fq2_modulus_coeffs, @field_modulus)

  # Original @b12 FQ12.new(Tuple.pad({4}, 12, 0))
  # This means coeffs are [4, 0, 0, ..., 0]
  @b12_int_coeffs [4 | List.duplicate(0, 11)]
  @b12 FQP.new_fqp(@b12_int_coeffs, @fq12_modulus_coeffs, @field_modulus)

  @g1_x FQ.new_fq(
    36_854_167_537_133_870_167_810_883_151_830_777_579_616_207_957_825_464_098_945_783_786_886_075_923_783_763_188_360_549_476_763_458_215_481_041_854_645_07,
    @field_modulus
  )
  @g1_y FQ.new_fq(
    1_339_506_544_944_476_473_020_471_379_941_921_221_584_933_875_938_349_620_426_543_736_416_511_423_956_333_506_472_724_655_353_366_534_992_391_756_441_569,
    @field_modulus
  )
  @g1_z FQ.new_fq(1, @field_modulus)
  @g1 {@g1_x, @g1_y, @g1_z}

  # G2 points are FQP structs (representing FQ2 elements) with integer coeffs
  @g2_x_c0 FQ.new_fq(352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160, @field_modulus)
  @g2_x_c1 FQ.new_fq(305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_5758, @field_modulus)
  @g2_y_c0 FQ.new_fq(198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_7905, @field_modulus)
  @g2_y_c1 FQ.new_fq(927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582, @field_modulus)
  @g2_z FQP.new_fqp([1,0], @fq2_modulus_coeffs, @field_modulus) # FQ2.one() -> FQP([1,0],...)

  @g2_x FQP.new_fqp([@g2_x_c0.n, @g2_x_c1.n], @fq2_modulus_coeffs, @field_modulus)
  @g2_y FQP.new_fqp([@g2_y_c0.n, @g2_y_c1.n], @fq2_modulus_coeffs, @field_modulus)
  @g2 {@g2_x, @g2_y, @g2_z}

  @z1 {FQ.one(@field_modulus), FQ.one(@field_modulus), FQ.zero(@field_modulus)}
  # For @z2, FQP.one() and FQP.zero() are needed for optimized FQP
  @z2_one_coeffs [1 | List.duplicate(0, length(@fq2_modulus_coeffs) -1)]
  @z2_zero_coeffs List.duplicate(0, length(@fq2_modulus_coeffs))
  @z2 {FQP.new_fqp(@z2_one_coeffs, @fq2_modulus_coeffs, @field_modulus),
       FQP.new_fqp(@z2_one_coeffs, @fq2_modulus_coeffs, @field_modulus),
       FQP.new_fqp(@z2_zero_coeffs, @fq2_modulus_coeffs, @field_modulus)}

  # Helper to get the module (FQ or FQP) for point operations based on element type
  defp elem_op_module(%FQ{}), do: FQ
  defp elem_op_module(%FQP{}), do: FQP
  defp elem_op_module(%{coeffs: _coeffs, modulus_coeffs: _modulus_coeffs}), do: FQP # FQP struct duck typing
  defp elem_op_module(_), do: FQ # Default for safety, or raise error

  def is_inf(pt) do
    {_x, _y, z} = pt
    op_module = elem_op_module(z)
    op_module.equal?(z, op_module.zero(z.field_modulus, z.degree, z.modulus_coeffs)) # Compare with FQP.zero if z is FQP
  end

  def is_on_curve(pt, b_val) do # b_val is an FQ struct
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt # These are FQ structs
      # y^2 * z - x^3 == b * z^3  (projective)
      # All ops are FQ ops here because x,y,z are FQ for G1
      term1 = FQ.mul(FQ.mul(y, y), z)
      term2 = FQ.mul(FQ.mul(x, x), x)
      lhs = FQ.sub(term1, term2)
      # Ensure b_val is an FQ struct for mul; if it's integer, convert
      b_fq = FQ.ensure_fq(b_val, @field_modulus)
      rhs = FQ.mul(b_fq, FQ.mul(FQ.mul(z, z), z))
      FQ.equal?(lhs, rhs)
    end
  end

  def double(pt) do
    {x, y, z} = pt
    op_module = elem_op_module(x)
    fm = x.field_modulus

    # Constants as FQ/FQP elements
    # For Optimized FQ, direct integers are used in FQP ops if not FQP elements themselves.
    # The optimized FQP operations handle integer scalars directly.
    fq_2 = FQ.new_fq(2, fm)
    fq_3 = FQ.new_fq(3, fm)
    fq_4 = FQ.new_fq(4, fm)
    fq_8 = FQ.new_fq(8, fm)

    # For G1 (FQ points): x, y, z are FQ structs
    if op_module == FQ do
      w = op_module.mul(fq_3, op_module.mul(x, x)) # 3 * x^2
      s = op_module.mul(y, z) # y * z
      big_b_val = op_module.mul(x, op_module.mul(y, s)) # x * y * s
      h = op_module.sub(op_module.mul(w, w), op_module.mul(fq_8, big_b_val)) # w^2 - 8 * B
      s_squared = op_module.mul(s, s) # s^2
      newx = op_module.mul(fq_2, op_module.mul(h, s)) # 2 * h * s
      newy = op_module.sub(op_module.mul(w, op_module.sub(op_module.mul(fq_4, big_b_val), h)), op_module.mul(fq_8, op_module.mul(y, op_module.mul(y, s_squared)))) # w * (4B - h) - 8 * y^2 * s^2
      newz = op_module.mul(fq_8, op_module.mul(s, s_squared)) # 8 * s^3
      {newx, newy, newz}
    else # For G2 (FQP points)
      # Here x, y, z are FQP structs. Arithmetic should use FQP module methods.
      # The FQP methods in OptimizedFieldElements accept integers as scalars directly for some ops.
      w = op_module.mul(x, x) |> op_module.mul(3) # 3 * x^2
      s = op_module.mul(y, z) # y * z
      big_b_val = op_module.mul(x, op_module.mul(y, s)) # x * y * s
      h = op_module.sub(op_module.mul(w, w), op_module.mul(big_b_val, 8)) # w^2 - 8 * B
      s_squared = op_module.mul(s, s) # s^2
      newx = op_module.mul(h, s) |> op_module.mul(2) # 2 * h * s
      newy = op_module.sub(op_module.mul(w, op_module.sub(op_module.mul(big_b_val, 4), h)), op_module.mul(op_module.mul(y, op_module.mul(y, s_squared)), 8)) # w * (4B - h) - 8 * y^2 * s^2
      newz = op_module.mul(s, s_squared) |> op_module.mul(8) # 8 * s^3
      {newx, newy, newz}
    end
  end

  def add(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    op_module = elem_op_module(x1) # Assuming p1 and p2 are of the same type
    fm = x1.field_modulus

    one = op_module.one(fm, x1.degree, x1.modulus_coeffs) # FQ.one(fm) or FQP.one(...)
    zero = op_module.zero(fm, x1.degree, x1.modulus_coeffs) # FQ.zero(fm) or FQP.zero(...)

    cond do
      op_module.equal?(z1, zero) -> p2 # p1 is inf
      op_module.equal?(z2, zero) -> p1 # p2 is inf
      true ->
        u1 = op_module.mul(y2, z1)
        u2 = op_module.mul(y1, z2)
        v1 = op_module.mul(x2, z1)
        v2 = op_module.mul(x1, z2)

        cond do
          op_module.equal?(v1, v2) and op_module.equal?(u1, u2) -> double(p1)
          op_module.equal?(v1, v2) -> {one, one, zero} # Point at infinity
          true ->
            u = op_module.sub(u1, u2)
            v = op_module.sub(v1, v2)
            v_squared = op_module.mul(v, v)
            v_squared_times_v2 = op_module.mul(v_squared, v2)
            v_cubed = op_module.mul(v, v_squared)
            w_val = op_module.mul(z1, z2)
            # If op_module is FQ, integer 2 needs to be FQ(2)
            # If op_module is FQP, its mul handles integer scalars.
            two_val = if op_module == FQ, do: FQ.new_fq(2, fm), else: 2

            a_val = op_module.sub(op_module.sub(op_module.mul(op_module.mul(u, u), w_val), v_cubed), op_module.mul(v_squared_times_v2, two_val))
            newx = op_module.mul(v, a_val)
            newy = op_module.sub(op_module.mul(u, op_module.sub(v_squared_times_v2, a_val)), op_module.mul(v_cubed, u2))
            newz = op_module.mul(v_cubed, w_val)
            {newx, newy, newz}
        end
    end
  end

  def multiply(pt, n) when is_integer(n) do
    {_x, _y, z_coord} = pt
    op_module = elem_op_module(z_coord)
    fm = z_coord.field_modulus
    # Determine degree and modulus_coeffs for zero point, handling FQ case where they are not in struct
    degree = Map.get(z_coord, :degree, 1) # Default to 1 for FQ like elements
    mod_coeffs = Map.get(z_coord, :modulus_coeffs, [1]) # Default for FQ like elements

    inf_pt = {op_module.one(fm, degree, mod_coeffs),
              op_module.one(fm, degree, mod_coeffs),
              op_module.zero(fm, degree, mod_coeffs)}
    cond do
      n == 0 -> inf_pt
      n == 1 -> pt
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do # Points are {x,y,z} where coords are FQ or FQP structs
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    op_module = elem_op_module(x1) # Assuming homogeneous coordinates

    op_module.equal?(op_module.mul(x1, z2), op_module.mul(x2, z1)) and
      op_module.equal?(op_module.mul(y1, z2), op_module.mul(y2, z1))
  end

  def normalize(pt) do # Point {x,y,z} with FQ or FQP elements
    {x, y, z} = pt
    op_module = elem_op_module(x)
    inv_z = op_module.inv(z) # Requires FQP.inv to be robust
    {op_module.mul(x, inv_z), op_module.mul(y, inv_z)}
  end

  # For @w, original was FQ12.new(Tuple.pad({0, 1}, 12, 0))
  # This is FQP([0,1,0,...0], fq12_mod_coeffs, fm)
  @w_int_coeffs ([0,1] ++ List.duplicate(0,10))
  @w FQP.new_fqp(@w_int_coeffs, @fq12_modulus_coeffs, @field_modulus)

  def neg(pt) do
    {x, y, z} = pt
    op_module = elem_op_module(y)
    {x, op_module.neg(y), z}
  end

  # twist for optimized curve elements
  def twist(pt) do
    {x_fq2, y_fq2, z_fq2} = pt # pt is a G2 point {FQP, FQP, FQP}
    # Optimized FQP stores coeffs as integers.
    # x_fq2.coeffs is [re, im]
    # y_fq2.coeffs is [re, im]
    # z_fq2.coeffs is [re, im]

    # Field isomorphism from FQ2 (u^2=-1) to FQ2 (u^2 - xi = 0) is not directly applied here.
    # The py_ecc twist for optimized BLS12-381 is different and complex.
    # It involves (x * W**-2, y * W**-3), where W is an FQ12 element.
    # This requires FQP division/multiplication by FQ12 powers.
    # This current `twist` is likely from a different source or incorrect interpretation.
    # Reverting to a simpler placeholder or what py_ecc might do (mul by const FQ12 elements).

    # The original py_ecc twist for optimized does:
    # return (x * FQ12.one() / W2, y * FQ12.one() / W3, z * FQ12.one())
    # Where W2 = cls.FQ12_W**2, W3 = cls.FQ12_W**3
    # And FQ12_W is the FQ12 element [0,1,0,...0]

    # This requires FQP (representing FQ2) to be multiplied by FQ12 elements, resulting in FQ12.
    # This is not a simple coefficient shuffle as the old code implied.
    # For now, placeholder, as this needs careful implementation of FQP<->FQ12 ops.
    _ = {x_fq2, y_fq2, z_fq2}
    nil # Placeholder for actual twisted point {FQ12, FQ12, FQ12}
  end
end
