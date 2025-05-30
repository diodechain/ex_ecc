defmodule ExEcc.Bls12_381.Bls12381Curve do
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ
  alias ExEcc.Fields.OptimizedBls12381FQ2, as: FQ2
  alias ExEcc.Fields.FQ12

  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB
  @curve_order 0x73EDA753299D7D483339D80809A1D80553BDA402FFFE5BFEFFFFFFFF00000001

  @b 4
  @b2 4

  @g1_x 0x17F1D3A73197D7942695638C4FA9AC0FC3688C4F9774B905A14E3A3F171BAC586C55E83FF97A1AEFFB3AF00ADB22C6BB
  @g1_y 0x08B3F481E3AAA0F1A09E30ED741D8AE4FCF5E095D5D00AF600DB18CB2C04B3EDD03CC744A2888AE40CAA232946C5E7E1
  @g2_x1 0x024AA2B2F08F0A91260805272DC51051C6E47AD4FA403B02B4510B647AE3D1770BAC0326A805BBEFD48056C8C121BDB8
  @g2_x2 0x13E02B6052719F607DACD3A088274F65596BD0D09920B61AB5DA61BBDC7F5049334CF11213945D57E5AC7D055D042B7E
  @g2_y1 0x0CE5D527727D6E118CC9CDC6DA2E351AADFD9BAA8CBDD3A76D429A695160D12C923AC9CC3BACA289E193548608B82801
  @g2_y2 0x0606C4A02EA734CC32ACD2B02BC28B99CB3E287E85A763AF267492AB572E99AB3F370D275CEC1DA1AAA9075FF05F79BE

  def field_modulus, do: @field_modulus
  def curve_order, do: @curve_order

  def b, do: @b

  def b2, do: @b2

  def g1_x, do: @g1_x

  def g1_y, do: @g1_y

  def g2_x1, do: @g2_x1

  def g2_x2, do: @g2_x2

  def g2_y1, do: @g2_y1

  def g2_y2, do: @g2_y2

  @doc """
  Returns the G1 generator point for BLS12-381 curve.
  """
  def g1 do
    {FQ.new_fq(@g1_x, @field_modulus), FQ.new_fq(@g1_y, @field_modulus)}
  end

  @doc """
  Returns the G2 generator point for BLS12-381 curve.
  """
  def g2 do
    {
      FQ2.new([@g2_x1, @g2_x2], @field_modulus),
      FQ2.new([@g2_y1, @g2_y2], @field_modulus)
    }
  end

  @doc """
  Returns the zero point (point at infinity) for G1.
  """
  def z1 do
    {FQ.zero(@field_modulus), FQ.zero(@field_modulus)}
  end

  @doc """
  Returns the zero point (point at infinity) for G2.
  """
  def z2 do
    {
      FQ2.zero(@field_modulus),
      FQ2.zero(@field_modulus)
    }
  end

  def is_on_curve(pt) do
    cond do
      is_inf(pt) ->
        true

      true ->
        {x, y} = pt
        FieldMath = elem_module(x)
        y_squared = FieldMath.mul(y, y)
        x_cubed = FieldMath.mul(FieldMath.mul(x, x), x)

        b_val =
          if FieldMath == FQ,
            do: FQ.new_fq(@b, @field_modulus),
            else: FQ2.new([@b2, 0], @field_modulus)

        FieldMath.equal?(y_squared, FieldMath.add(x_cubed, b_val))
    end
  end

  def is_inf(pt) do
    cond do
      is_nil(pt) ->
        true

      {x, y} = pt ->
        FieldMath = elem_module(x)

        FieldMath.equal?(x, FieldMath.zero(@field_modulus)) and
          FieldMath.equal?(y, FieldMath.zero(@field_modulus))
    end
  end

  def double(pt) do
    cond do
      is_inf(pt) ->
        pt

      true ->
        {x, y} = pt
        FieldMath = elem_module(x)

        fq_2 =
          if FieldMath == FQ,
            do: FQ.new_fq(2, @field_modulus),
            else: FQ2.new([2, 0], @field_modulus)

        fq_3 =
          if FieldMath == FQ,
            do: FQ.new_fq(3, @field_modulus),
            else: FQ2.new([3, 0], @field_modulus)

        three_x_squared = FieldMath.mul(fq_3, FieldMath.mul(x, x))
        two_y = FieldMath.mul(fq_2, y)
        m = FieldMath.divide(three_x_squared, two_y)

        new_x = FieldMath.sub(FieldMath.mul(m, m), FieldMath.mul(fq_2, x))
        new_y = FieldMath.sub(FieldMath.mul(m, FieldMath.sub(x, new_x)), y)
        {new_x, new_y}
    end
  end

  def add(p1, p2) do
    cond do
      is_inf(p1) ->
        p2

      is_inf(p2) ->
        p1

      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        FieldMath = elem_module(x1)

        cond do
          FieldMath.eq(x1, x2) and FieldMath.eq(y1, y2) ->
            double(p1)

          FieldMath.eq(x1, x2) ->
            if FieldMath == FQ, do: z1(), else: z2()

          true ->
            m_num = FieldMath.sub(y2, y1)
            m_den = FieldMath.sub(x2, x1)
            m = FieldMath.divide(m_num, m_den)

            new_x = FieldMath.sub(FieldMath.sub(FieldMath.mul(m, m), x1), x2)
            new_y = FieldMath.sub(FieldMath.mul(m, FieldMath.sub(x1, new_x)), y1)
            {new_x, new_y}
        end
    end
  end

  def multiply(pt, n) when is_integer(n) do
    cond do
      n == 0 ->
        z1()

      n == 1 ->
        pt

      n < 0 ->
        multiply(neg(pt), -n)

      rem(n, 2) == 0 ->
        multiply(double(pt), div(n, 2))

      true ->
        add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def neg(pt) do
    if is_inf(pt) do
      pt
    else
      {x, y} = pt
      FieldMath = elem_module(y)
      {x, FieldMath.neg(y)}
    end
  end

  def negate(pt), do: neg(pt)

  def eq(p1, p2) do
    cond do
      is_inf(p1) and is_inf(p2) ->
        true

      is_inf(p1) or is_inf(p2) ->
        false

      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        FieldMath = elem_module(x1)
        FieldMath.eq(x1, x2) and FieldMath.eq(y1, y2)
    end
  end

  def equal?(p1, p2), do: eq(p1, p2)

  defp elem_module(elem) do
    case elem do
      %FQ{} -> FQ
      %FQ2{} -> FQ2
      # Map FQP to FQ2 since FQ2 is built on FQP
      %{__struct__: FQP} -> FQ2
      _ -> raise "Invalid element type: #{inspect(elem)}"
    end
  end

  @doc """
  Twists a G2 point (FQ2) into FQ12.
  """
  def twist({x, y}) do
    # For BLS12-381, the twist is (x * w^2, y * w^3)
    # where w is the 12th root of unity
    # We need to convert the FQ2 point to FQ12
    x_fq12 = FQ12.new([x.coeffs[0], x.coeffs[1], 0, 0, 0, 0], @field_modulus)
    y_fq12 = FQ12.new([y.coeffs[0], y.coeffs[1], 0, 0, 0, 0], @field_modulus)
    {x_fq12, y_fq12}
  end

  def twist(nil), do: nil
end
