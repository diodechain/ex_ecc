defmodule ExEcc.OptimizedBN128.OptimizedCurve do
  alias ExEcc.Fields.OptimizedBN128FQ, as: FQ
  alias ExEcc.Fields.OptimizedBN128FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBN128FQ12, as: FQ12
  alias ExEcc.Fields.OptimizedFieldElements.FQP

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bn128"].field_modulus
  @curve_order 218_882_428_718_392_752_222_464_057_452_572_750_885_483_644_004_160_343_436_982_041_865_758_084_956_17

  # TODO: Port primality and factor checks if large number math is available
  # if :math.pow(2, @curve_order) |> round() |> rem(@curve_order) != 2 do
  #   raise ValueError, "Curve order is not prime"
  # end
  # if rem(:math.pow(@field_modulus, 12) - 1, @curve_order) != 0 do
  #   raise ValueError, "Curve order is not a factor of field_modulus**12 - 1"
  # end

  @b FQ.new(3)
  # FQ2.new({{3,0}}) for struct-based FQ2
  @b2 FQ2.divide(FQ2.new([3, 0]), FQ2.new([9, 1]))
  # Ensure FQ.zero() if it's a struct
  @b12 FQ12.new(List.to_tuple([3 | List.duplicate(FQ.zero(), 11)]))

  @g1 {FQ.new(1), FQ.new(2), FQ.new(1)}
  @g2 {
    FQ2.new([
      108_570_469_990_230_571_359_445_707_622_328_294_813_707_563_595_785_180_869_905_199_932_856_558_527_81,
      115_597_320_329_863_871_079_910_040_213_922_857_839_258_128_618_211_925_309_174_031_514_523_918_056_34
    ]),

    # For struct: FQ2.new({{c0, c1}})
    FQ2.new([
      # Corrected typo from python snippet (849_ -> 84_)
      84_956_539_231_234_314_176_049_732_474_892_724_384_181_905_872_636_001_487_702_806_493_069_581_019_30,
      # Corrected typo (408_ -> 40_)
      40_823_678_758_634_336_813_322_034_031_454_355_683_168_513_275_934_012_081_057_410_762_141_200_935_31
    ]),

    # For struct: FQ2.new({{c0, c1}})
    FQ2.one()
  }

  @z1 {FQ.one(), FQ.one(), FQ.zero()}
  @z2 {FQ2.one(), FQ2.one(), FQ2.zero()}

  # Helper to get the module for field operations based on the element type
  defp field_module_for_pt(pt) do
    # Assumes point elements are FQ/FQ2/FQ12 structs or nil
    # The first element of the point tuple (x-coordinate) determines the field type
    case elem(pt, 0) do
      %FQ{} -> FQ
      %FQP{degree: 2} -> FQ2
      %FQP{degree: 12} -> FQ12
      # Or raise an error for unknown type
      _ -> nil
    end
  end

  def is_inf(pt) do
    # Third element (z-coordinate) of the point tuple
    # Access the zero() function from the specific field module of z
    z_coord = elem(pt, 2)
    # Get module from struct type
    field_module = z_coord.__struct__
    field_module.eq(z_coord, field_module.zero())
  end

  def is_on_curve(pt, b_val) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      # Assuming x, y, z are of the same field type
      field_module = x.__struct__
      # y**2 * z - x**3 == b_val * z**3
      lhs =
        field_module.subtract(field_module.multiply(field_module.multiply(y, y), z), field_module.multiply(x, x, x))

      rhs = field_module.multiply(b_val, field_module.multiply(z, z, z))
      field_module.eq(lhs, rhs)
    end
  end

  # TODO: Add assertions once is_on_curve is fully tested.
  # unless is_on_curve(@g1, @b) do
  #   raise ValueError, "Generator G1 is not on curve"
  # end
  # unless is_on_curve(@g2, @b2) do
  #  raise ValueError, "Generator G2 is not on twisted curve"
  # end

  def double(pt) do
    {x, y, z} = pt
    field_module = x.__struct__

    w_val = field_module.multiply(field_module.new(3), field_module.multiply(x, x))
    s_val = field_module.multiply(y, z)
    # B in Python
    b_val_internal = field_module.multiply(x, field_module.multiply(y, s_val))

    h_val =
      field_module.subtract(
        field_module.multiply(w_val, w_val),
        field_module.multiply(field_module.new(8), b_val_internal)
      )

    s_squared = field_module.multiply(s_val, s_val)
    newx = field_module.multiply(field_module.new(2), field_module.multiply(h_val, s_val))

    newy =
      field_module.subtract(
        field_module.multiply(
          w_val,
          field_module.subtract(field_module.multiply(field_module.new(4), b_val_internal), h_val)
        ),
        field_module.multiply(field_module.new(8), field_module.multiply(y, field_module.multiply(y, s_squared)))
      )

    newz = field_module.multiply(field_module.new(8), field_module.multiply(s_val, s_squared))
    {newx, newy, newz}
  end

  def add(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    field_module = x1.__struct__
    one = field_module.one()
    zero = field_module.zero()

    cond do
      field_module.eq(z1, zero) or field_module.eq(z2, zero) ->
        if field_module.eq(z2, zero), do: p1, else: p2

      true ->
        u1 = field_module.multiply(y2, z1)
        u2 = field_module.multiply(y1, z2)
        v1 = field_module.multiply(x2, z1)
        v2 = field_module.multiply(x1, z2)

        cond do
          field_module.eq(v1, v2) and field_module.eq(u1, u2) ->
            double(p1)

          field_module.eq(v1, v2) ->
            {one, one, zero}

          true ->
            u_val = field_module.subtract(u1, u2)
            v_val = field_module.subtract(v1, v2)
            v_squared = field_module.multiply(v_val, v_val)
            v_squared_times_v2 = field_module.multiply(v_squared, v2)
            v_cubed = field_module.multiply(v_val, v_squared)
            # W in Python
            w_val = field_module.multiply(z1, z2)

            a_val =
              field_module.subtract(
                field_module.subtract(
                  field_module.multiply(field_module.multiply(u_val, u_val), w_val),
                  v_cubed
                ),
                field_module.multiply(field_module.new(2), v_squared_times_v2)
              )

            # A in Python
            newx = field_module.multiply(v_val, a_val)

            newy =
              field_module.subtract(
                field_module.multiply(u_val, field_module.subtract(v_squared_times_v2, a_val)),
                field_module.multiply(v_cubed, u2)
              )

            newz = field_module.multiply(v_cubed, w_val)
            {newx, newy, newz}
        end
    end
  end

  def multiply(pt, n) when is_integer(n) do
    field_module = elem(pt, 0).__struct__

    cond do
      n == 0 ->
        {field_module.one(), field_module.one(), field_module.zero()}

      n == 1 ->
        pt

      rem(n, 2) == 0 ->
        multiply(double(pt), div(n, 2))

      true ->
        add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    field_module = x1.__struct__

    cond do
      field_module.eq(z1, field_module.zero()) and field_module.eq(z2, field_module.zero()) ->
        true

      field_module.eq(z1, field_module.zero()) or field_module.eq(z2, field_module.zero()) ->
        false

      true ->
        field_module.eq(field_module.multiply(x1, z2), field_module.multiply(x2, z1)) and
          field_module.eq(field_module.multiply(y1, z2), field_module.multiply(y2, z1))
    end
  end

  def neg(pt) do
    {x, y, z} = pt
    field_module = x.__struct__
    {x, field_module.negate(y), z}
  end

  def negate(pt), do: neg(pt)

  def g1, do: @g1
  def g2, do: @g2
  def z1, do: @z1
  def z2, do: @z2
  def b, do: @b
  def b2, do: @b2
  def b12, do: @b12
  def field_modulus, do: @field_modulus
  def curve_order, do: @curve_order
end
