defmodule ExEcc.BLS12_381.BLS12381Curve do
  alias ExEcc.Fields.BLS12381FQ, as: FQ
  alias ExEcc.Fields.BLS12381FQ2, as: FQ2
  alias ExEcc.Fields.BLS12381FQ12, as: FQ12
  alias ExEcc.Fields.BLS12381FQP, as: FQP

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus
  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  # TODO: Port primality and factor checks if large number math is available
  # if :math.pow(2, @curve_order) |> round() |> rem(@curve_order) != 2 do
  #   raise ValueError, "Curve order is not prime"
  # end
  # if rem(:math.pow(@field_modulus, 12) - 1, @curve_order) != 0 do
  #   raise ValueError, "Curve order is not a factor of field_modulus**12 - 1"
  # end

  # Curve is y**2 = x**3 + 4
  @b FQ.new(4)
  # Twisted curve over FQ**2
  @b2 FQ2.new([4, 4])
  # Extension curve over FQ**12; same b value as over FQ
  @b12 FQ12.new(List.to_tuple([4 | List.duplicate(0, 11)]))

  # Generator for curve over FQ
  @g1 {
    FQ.new(
      368_541_675_371_338_701_678_108_831_518_307_775_796_162_079_578_254_640_989_457_837_868_860_759_237_837_631_883_605_494_767_634_582_154_810_418_546_4507
    ),
    FQ.new(
      133_950_654_494_447_647_302_047_137_994_192_122_158_493_387_593_834_962_042_654_373_641_651_142_395_633_350_647_272_465_535_336_653_499_239_175_644_1569
    )
  }

  # Generator for twisted curve over FQ2
  @g2 {
    FQ2.new([
      352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160,
      305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_5758
    ]),
    FQ2.new([
      198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_7905,
      927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582
    ])
  }

  # Point at infinity over FQ
  @z1 nil
  # Point at infinity for twisted curve over FQ2
  @z2 nil

  # Accessor functions for module attributes
  def g1, do: @g1
  def g2, do: @g2
  def z1, do: @z1
  def z2, do: @z2
  def b, do: @b
  def b2, do: @b2
  def b12, do: @b12
  def field_modulus, do: @field_modulus
  def curve_order, do: @curve_order

  def is_inf(pt) do
    pt == nil
  end

  def is_on_curve(pt, b) do
    if is_inf(pt) or pt == nil do
      true
    else
      {x, y} = pt
      y ** 2 - x ** 3 == b
    end
  end
end
