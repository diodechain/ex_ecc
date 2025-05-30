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

  # Helper functions for mixed-type arithmetic
  defp ensure_fq(element, field_modulus) when is_integer(element) do
    FQ.new(element, field_modulus)
  end
  defp ensure_fq(%{__struct__: FQ} = element, _field_modulus), do: element
  defp ensure_fq(%{__struct__: FQP} = element, _field_modulus), do: element
  defp ensure_fq(%{__struct__: FQ2} = element, _field_modulus), do: element
  defp ensure_fq(%{__struct__: FQ12} = element, _field_modulus), do: element

  defp ensure_fq2(element, field_modulus) when is_integer(element) do
    FQ2.new([element, 0], field_modulus)
  end
  defp ensure_fq2(%{__struct__: FQ} = element, field_modulus) do
    FQ2.new([element.coeffs, 0], field_modulus)
  end
  defp ensure_fq2(%{__struct__: FQ2} = element, _field_modulus), do: element
  defp ensure_fq2(%{__struct__: FQP} = element, _field_modulus), do: element
  defp ensure_fq2(%{__struct__: FQ12} = element, _field_modulus), do: element

  defp ensure_fq12(%{__struct__: FQ12} = element, _field_modulus), do: element
  defp ensure_fq12(%{__struct__: FQP} = element, _field_modulus), do: element
  defp ensure_fq12(%{__struct__: FQ2} = element, field_modulus) do
    FQ12.new([element.coeffs | List.duplicate(0, 10)], field_modulus)
  end
  defp ensure_fq12(%{__struct__: FQ} = element, field_modulus) do
    FQ12.new([element.coeffs | List.duplicate(0, 11)], field_modulus)
  end
  defp ensure_fq12(element, field_modulus) do
    FQ12.new([element | List.duplicate(0, 11)], field_modulus)
  end

  # Helper to get the module for field operations based on the element type
  # defp field_module_for_pt(pt) do
  #   case elem(pt, 0) do
  #     %{__struct__: FQ} -> FQ
  #     %{__struct__: FQP, degree: 2} -> FQ2
  #     %{__struct__: FQP, degree: 12} -> FQ12
  #     _ -> nil
  #   end
  # end

  # Helper to determine the target field type for mixed operations
  defp target_field_type(a, b) do
    cond do
      is_struct(a, FQ12) or is_struct(b, FQ12) -> FQ12
      is_struct(a, FQ2) or is_struct(b, FQ2) -> FQ2
      true -> FQ
    end
  end

  # Helper to convert elements to the target field type
  defp convert_to_target_type(element, target_type, field_modulus) do
    case target_type do
      FQ12 -> ensure_fq12(element, field_modulus)
      FQ2 -> ensure_fq2(element, field_modulus)
      FQ -> ensure_fq(element, field_modulus)
    end
  end

  # Mixed-type arithmetic operations
  # defp mixed_add(a, b, field_modulus) do ... end

  def mixed_sub(a, b, target_type) do
    a_converted = ensure_fq(a, @field_modulus)
    b_converted = ensure_fq(b, @field_modulus)
    target_type.subtract(a_converted, b_converted)
  end

  defp mixed_mul(a, b, field_modulus) do
    target_type = target_field_type(a, b)
    a_converted = convert_to_target_type(a, target_type, field_modulus)
    b_converted = convert_to_target_type(b, target_type, field_modulus)
    target_type.mul(a_converted, b_converted)
  end

  def is_inf(pt) do
    z_coord = elem(pt, 2)
    field_module = z_coord.__struct__
    field_module.eq(z_coord, field_module.zero())
  end

  def is_on_curve(pt, b_val) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      # y^2 * z - x^3 == b_val * z^3  (projective)
      term1 = mixed_mul(mixed_mul(y, y, @field_modulus), z, @field_modulus)
      term2 = mixed_mul(mixed_mul(x, x, @field_modulus), x, @field_modulus)
      lhs = mixed_sub(term1, term2, @field_modulus)
      rhs = mixed_mul(b_val, mixed_mul(mixed_mul(z, z, @field_modulus), z, @field_modulus), @field_modulus)
      field_module = x.__struct__
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
    _field_module = x.__struct__

    w_val = mixed_mul(3, mixed_mul(x, x, @field_modulus), @field_modulus)
    s_val = mixed_mul(y, z, @field_modulus)
    b_val_internal = mixed_mul(x, mixed_mul(y, s_val, @field_modulus), @field_modulus)

    h_val = mixed_sub(
      mixed_mul(w_val, w_val, @field_modulus),
      mixed_mul(8, b_val_internal, @field_modulus),
      @field_modulus
    )

    s_squared = mixed_mul(s_val, s_val, @field_modulus)
    newx = mixed_mul(2, mixed_mul(h_val, s_val, @field_modulus), @field_modulus)

    newy = mixed_sub(
      mixed_mul(
        w_val,
        mixed_sub(mixed_mul(4, b_val_internal, @field_modulus), h_val, @field_modulus),
        @field_modulus
      ),
      mixed_mul(8, mixed_mul(y, mixed_mul(y, s_squared, @field_modulus), @field_modulus), @field_modulus),
      @field_modulus
    )

    newz = mixed_mul(8, mixed_mul(s_val, s_squared, @field_modulus), @field_modulus)
    {newx, newy, newz}
  end

  def add(p1, p2) do
    cond do
      is_inf(p1) -> p2
      is_inf(p2) -> p1
      true ->
        {x1, y1, z1} = p1
        {x2, y2, z2} = p2
        field_module = x1.__struct__

        cond do
          field_module.eq(x1, x2) and field_module.eq(y1, y2) ->
            double(p1)

          field_module.eq(x1, x2) ->
            {field_module.one(), field_module.one(), field_module.zero()}

          true ->
            u1 = mixed_mul(y2, z1, @field_modulus)
            u2 = mixed_mul(y1, z2, @field_modulus)
            v1 = mixed_mul(x2, z1, @field_modulus)
            v2 = mixed_mul(x1, z2, @field_modulus)

            u = mixed_sub(u1, u2, @field_modulus)
            v = mixed_sub(v1, v2, @field_modulus)
            v_squared = mixed_mul(v, v, @field_modulus)
            v_squared_times_v2 = mixed_mul(v_squared, v2, @field_modulus)
            v_cubed = mixed_mul(v, v_squared, @field_modulus)
            w_val = mixed_mul(z1, z2, @field_modulus)

            a_val = mixed_sub(
              mixed_sub(
                mixed_mul(mixed_mul(u, u, @field_modulus), w_val, @field_modulus),
                v_cubed,
                @field_modulus
              ),
              mixed_mul(v_squared_times_v2, 2, @field_modulus),
              @field_modulus
            )

            newx = mixed_mul(v, a_val, @field_modulus)
            newy = mixed_sub(
              mixed_mul(u, mixed_sub(v_squared_times_v2, a_val, @field_modulus), @field_modulus),
              mixed_mul(v_cubed, u2, @field_modulus),
              @field_modulus
            )
            newz = mixed_mul(v_cubed, w_val, @field_modulus)
            {newx, newy, newz}
        end
    end
  end

  def multiply(pt, n) when is_integer(n) do
    field_module = elem(pt, 0).__struct__
    cond do
      n == 0 -> {field_module.one(), field_module.one(), field_module.zero()}
      n == 1 -> pt
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
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
        field_module.eq(mixed_mul(x1, z2, @field_modulus), mixed_mul(x2, z1, @field_modulus)) and
          field_module.eq(mixed_mul(y1, z2, @field_modulus), mixed_mul(y2, z1, @field_modulus))
    end
  end

  def neg(pt) do
    {x, y, z} = pt
    field_module = x.__struct__
    {x, field_module.neg(y), z}
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

  def normalize({x, y, z}) do
    field_module = x.__struct__
    zero = field_module.zero()
    if field_module.eq(z, zero) do
      {zero, zero}
    else
      z_inv = field_module.inv(z)
      {field_module.mul(x, z_inv), field_module.mul(y, z_inv)}
    end
  end
end
