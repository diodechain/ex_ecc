defmodule ExEcc.Fields.OptimizedBN128FQ do
  @field_modulus 21888242871839275222246405745257275088548364400416034343698204186575808495617

  defstruct n: 0, field_modulus: nil

  def field_modulus, do: @field_modulus

  def new(n), do: %__MODULE__{n: rem(rem(n, @field_modulus) + @field_modulus, @field_modulus), field_modulus: @field_modulus}
  def new(n, field_modulus), do: %__MODULE__{n: rem(rem(n, field_modulus) + field_modulus, field_modulus), field_modulus: field_modulus}

  def zero, do: new(0)

  def one, do: new(1)
end
