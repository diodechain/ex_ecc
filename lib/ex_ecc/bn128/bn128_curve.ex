defmodule ExEcc.Bn128.Bn128Curve do
  alias ExEcc.Fields.FieldElements, as: FQ
  alias ExEcc.Fields.FieldElements.FQP, as: FQP
  alias ExEcc.Fields.FieldProperties
  # alias ExEcc.Fields.Bn128FQP, as: FQP # Not directly used yet

  @curve_order 218_882_428_718_392_752_222_464_057_452_572_750_885_483_644_004_160_343_436_982_041_865_758_084_956_17
  @bn128_field_modulus FieldProperties.field_properties()["bn128"].field_modulus
  @bn128_fq2_modulus_coeffs FieldProperties.field_properties()["bn128"].fq2_modulus_coeffs
  @bn128_fq12_modulus_coeffs FieldProperties.field_properties()["bn128"].fq12_modulus_coeffs

  @b FQ.new_fq(3, @bn128_field_modulus)

  @b2_num_coeffs [FQ.new_fq(3, @bn128_field_modulus), FQ.new_fq(0, @bn128_field_modulus)]
  @b2_den_coeffs [FQ.new_fq(9, @bn128_field_modulus), FQ.new_fq(1, @bn128_field_modulus)]
  @b2_modulus_coeffs Enum.map(@bn128_fq2_modulus_coeffs, &FQ.new_fq(&1, @bn128_field_modulus))

  @b2_num FQP.new_fqp(@b2_num_coeffs, @b2_modulus_coeffs, @bn128_field_modulus)
  @b2_den FQP.new_fqp(@b2_den_coeffs, @b2_modulus_coeffs, @bn128_field_modulus)
  @b2 FQP.div(@b2_num, @b2_den)

  @b12_coeffs [
    FQ.new_fq(3, @bn128_field_modulus) | List.duplicate(FQ.zero(@bn128_field_modulus), 11)
  ]
  @b12_modulus_coeffs Enum.map(@bn128_fq12_modulus_coeffs, &FQ.new_fq(&1, @bn128_field_modulus))
  @b12 FQP.new_fqp(@b12_coeffs, @b12_modulus_coeffs, @bn128_field_modulus)

  @g1 {FQ.new_fq(1, @bn128_field_modulus), FQ.new_fq(2, @bn128_field_modulus)}

  @g2_x_c0 108_570_469_990_230_571_359_445_707_622_328_294_813_707_563_595_785_180_869_905_199_932_856_558_527_81
  @g2_x_c1 115_597_320_329_863_871_079_910_040_213_922_857_839_258_128_618_211_925_309_174_031_514_523_918_056_34
  @g2_y_c0 849_565_392_312_343_368_133_220_340_314_543_556_831_685_132_759_340_120_810_574_107_621_412_009_353_1
  @g2_y_c1 408_236_787_586_343_368_133_220_340_314_543_556_831_685_132_759_340_120_810_574_107_621_412_009_353_1

  @g2_x_coeffs [
    FQ.new_fq(@g2_x_c0, @bn128_field_modulus),
    FQ.new_fq(@g2_x_c1, @bn128_field_modulus)
  ]
  @g2_y_coeffs [
    FQ.new_fq(@g2_y_c0, @bn128_field_modulus),
    FQ.new_fq(@g2_y_c1, @bn128_field_modulus)
  ]

  @g2 {FQP.new_fqp(@g2_x_coeffs, @b2_modulus_coeffs, @bn128_field_modulus),
       FQP.new_fqp(@g2_y_coeffs, @b2_modulus_coeffs, @bn128_field_modulus)}

  @z1 nil
  @z2 nil

  defguard is_general_point(pt)
           when is_nil(pt) or (is_tuple(pt) and tuple_size(pt) == 2 and is_struct(elem(pt, 0)))

  def is_inf(pt) when is_general_point(pt) or is_nil(pt) do
    is_nil(pt)
  end

  defp module_for_point(%FQ{}), do: FQ
  defp module_for_point(%FQP{}), do: FQP

  defp module_for_point(_other) do
    FQ
  end

  def is_on_curve(pt, b_val) when is_general_point(pt) or is_nil(pt) do
    if is_inf(pt) do
      true
    else
      {x, y} = pt
      elem_module = module_for_point(x)
      elem_module.eq(elem_module.sub(elem_module.pow(y, 2), elem_module.pow(x, 3)), b_val)
    end
  end

  # Validations - can be moved to tests
  # if not is_on_curve(@g1, @b), do: raise "G1 is not on the curve"
  # if not is_on_curve(@g2, @b2), do: raise "G2 is not on the curve"

  def double(pt) when is_general_point(pt) or is_nil(pt) do
    if is_inf(pt) do
      pt
    else
      {x, y} = pt
      elem_module = module_for_point(x)
      # Ensure numbers like 2 and 3 are FQ elements in the correct field
      fq_2_typed =
        case x do
          %FQP{modulus_coeffs: mc, degree: deg, field_modulus: fm} ->
            FQP.new_fqp(
              [
                FQ.new_fq(2, fm) | List.duplicate(FQ.zero(fm), deg - 1)
              ],
              mc,
              fm
            )

          _ ->
            FQ.new_fq(2, x.field_modulus)
        end

      fq_3 = FQ.new_fq(3, x.field_modulus)

      fq_3_typed =
        case x do
          %FQP{modulus_coeffs: mc, degree: deg, field_modulus: fm} ->
            FQP.new_fqp(
              [
                FQ.new_fq(3, fm) | List.duplicate(FQ.zero(fm), deg - 1)
              ],
              mc,
              fm
            )

          _ ->
            fq_3
        end

      three_x_squared = elem_module.mul(fq_3_typed, elem_module.pow(x, 2))
      two_y = elem_module.mul(fq_2_typed, y)

      # If two_y is zero, the point is at infinity
      zero_typed =
        case x do
          %FQP{degree: deg, modulus_coeffs: mc, field_modulus: fm} ->
            FQP.zero(fm, deg, mc)

          _ ->
            FQ.zero(x.field_modulus)
        end

      if elem_module.equal?(two_y, zero_typed) do
        nil
      else
        m = elem_module.div(three_x_squared, two_y)
        new_x = elem_module.sub(elem_module.pow(m, 2), elem_module.mul(fq_2_typed, x))
        new_y = elem_module.sub(elem_module.mul(m, elem_module.sub(x, new_x)), y)
        {new_x, new_y}
      end
    end
  end

  def add(p1, p2)
      when (is_general_point(p1) or is_nil(p1)) and (is_general_point(p2) or is_nil(p2)) do
    cond do
      is_inf(p1) ->
        p2

      is_inf(p2) ->
        p1

      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        elem_module = module_for_point(x1)

        cond do
          elem_module.eq(x1, x2) and elem_module.eq(y1, y2) ->
            double(p1)

          elem_module.eq(x1, x2) ->
            nil

          true ->
            m_num = elem_module.sub(y2, y1)
            m_den = elem_module.sub(x2, x1)
            m = elem_module.div(m_num, m_den)
            newx = elem_module.sub(elem_module.sub(elem_module.pow(m, 2), x1), x2)
            newy = elem_module.sub(elem_module.mul(m, elem_module.sub(x1, newx)), y1)
            {newx, newy}
        end
    end
  end

  def multiply(pt, n) when (is_general_point(pt) or is_nil(pt)) and is_integer(n) do
    cond do
      n == 0 -> nil
      n == 1 -> pt
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2)
      when (is_general_point(p1) or is_nil(p1)) and (is_general_point(p2) or is_nil(p2)) do
    cond do
      is_inf(p1) and is_inf(p2) ->
        true

      is_inf(p1) or is_inf(p2) ->
        false

      true ->
        {x1, y1_coord} = p1
        {x2, y2_coord} = p2
        elem_module = module_for_point(x1)
        elem_module.eq(x1, x2) and elem_module.eq(y1_coord, y2_coord)
    end
  end

  @w_coeffs [
    FQ.zero(@bn128_field_modulus),
    FQ.one(@bn128_field_modulus) | List.duplicate(FQ.zero(@bn128_field_modulus), 10)
  ]
  @w FQP.new_fqp(@w_coeffs, @bn128_fq12_modulus_coeffs, @bn128_field_modulus)

  def neg(pt) when is_general_point(pt) or is_nil(pt) do
    if is_inf(pt) do
      nil
    else
      {x, y} = pt
      elem_module = module_for_point(x)
      {x, elem_module.neg(y)}
    end
  end

  def twist(_pt) do
    # Placeholder
    nil
  end

  # Accessors
  def g1(), do: @g1
  def g2(), do: @g2
  def z1(), do: @z1
  def z2(), do: @z2
  def b(), do: @b
  def b2(), do: @b2
  def b12(), do: @b12
  def curve_order(), do: @curve_order
  def g12(), do: twist(@g2)
  def w(), do: @w
end
