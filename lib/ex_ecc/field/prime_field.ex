defmodule ExECC.Field.PrimeField do
  @moduledoc """
  Implementation of a prime field with arithmetic operations.
  """

  @field_modulus 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

  defstruct [:value]

  @type t :: %__MODULE__{value: non_neg_integer()}

  def field_modulus, do: @field_modulus
end
