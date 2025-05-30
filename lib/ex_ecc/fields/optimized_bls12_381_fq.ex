defmodule ExEcc.Fields.OptimizedBls12381FQ do
  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB

  def field_modulus, do: @field_modulus

  def new(n) when is_integer(n) do
    rem(rem(n, @field_modulus) + @field_modulus, @field_modulus)
  end
end
