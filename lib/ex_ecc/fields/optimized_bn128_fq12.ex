defmodule ExEcc.Fields.OptimizedBN128FQ12 do
  alias ExEcc.Fields.OptimizedFieldElements.FQP

  @field_modulus 21888242871839275222246405745257275088548364400416034343698204186575808495617
  @modulus_coeffs [0, 1, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0]

  def new(coeffs) when is_list(coeffs) do
    coeffs_mod = Enum.map(coeffs, fn
      %ExEcc.Fields.OptimizedBN128FQ{n: n} ->
        rem(rem(n, @field_modulus) + @field_modulus, @field_modulus)
      c when is_integer(c) ->
        rem(rem(c, @field_modulus) + @field_modulus, @field_modulus)
      other ->
        raise "Invalid coefficient type: #{inspect(other)}"
    end)
    FQP.new_fqp(coeffs_mod, @modulus_coeffs, @field_modulus)
  end

  def new(tuple) when is_tuple(tuple) do
    new(Tuple.to_list(tuple))
  end

  def one do
    new([1 | List.duplicate(0, 11)])
  end

  def zero do
    new(List.duplicate(0, 12))
  end
end
