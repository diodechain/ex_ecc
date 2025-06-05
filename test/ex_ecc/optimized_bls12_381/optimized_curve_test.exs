defmodule ExEcc.OptimizedBLS12381.OptimizedCurveTest do
  @moduledoc """
  Check that the twist creates a point that is on the curve
  """
  use ExUnit.Case

  alias ExEcc.OptimizedBLS12381.OptimizedCurve
  alias ExEcc.FieldMath

  @ref_g12 [
    {0,
     1_295_966_280_564_920_301_633_669_126_993_182_664_460_442_447_255_135_566_892_593_574_072_190_111_457_594_458_882_525_666_368_533_822_152_770_435_497_189,
     0, 0, 0, 0, 0,
     3_059_144_344_244_213_709_971_259_814_753_781_636_986_470_325_476_647_558_659_373_206_291_635_324_768_958_432_433_509_563_104_347_017_837_885_763_365_758,
     0, 0, 0, 0},
    {1_057_596_936_794_959_479_820_852_555_401_133_757_543_128_784_762_062_760_834_350_686_272_367_849_371_148_153_442_919_814_535_376_419_066_060_996_128_323,
     0, 0, 0, 0, 0,
     927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582,
     0, 0, 0, 0, 0},
    {0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0}
  ]

  test "g12" do
    assert Enum.map(Tuple.to_list(OptimizedCurve.g12()), fn fqp -> FieldMath.coeffs(fqp) end) ==
             @ref_g12
  end

  test "is_on_curve" do
    assert OptimizedCurve.is_on_curve(OptimizedCurve.g12(), OptimizedCurve.b12())
  end
end
