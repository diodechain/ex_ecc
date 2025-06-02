defmodule ExEcc.Utils do
  import While

  @doc """
  Extended Euclidean algorithm to find modular inverses for integers.
  Returns the modular multiplicative inverse of a modulo n.
  """
  def prime_field_inv(a, n) do
    # To address a == n edge case.
    # https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-09#section-4
    # inv0(x): This function returns the multiplicative inverse of x in
    # F, extended to all of F by fixing inv0(0) == 0.
    a = rem(a, n)

    if a == 0 do
      0
    else
      {lm, hm} = {1, 0}
      {low, high} = {rem(a, n), n}

      {lm, _low, _hm, _high} =
        reduce_while({lm, low, hm, high}, fn {lm, low, hm, high} ->
          if low > 1 do
            r = div(high, low)
            nm = hm - lm * r
            new = high - low * r
            {:cont, {nm, new, lm, low}}
          else
            {:halt, {lm, low, hm, high}}
          end
        end)

      rem(lm, n)
    end
  end

  def deg(p) do
    d = length(p) - 1

    reduce_while(d, fn d ->
      if Enum.at(p, d) == 0 and d > 0 do
        {:cont, d - 1}
      else
        {:halt, d}
      end
    end)
  end

  def poly_rounded_div(a, b) do
    dega = deg(a)
    degb = deg(b)
    temp = Enum.to_list(a)
    o = List.duplicate(0, length(a))

    {o, _temp} =
      Enum.reduce((dega - degb)..0//-1, {o, temp}, fn i, {o, temp} ->
        o =
          List.update_at(o, i, fn val -> val + div(Enum.at(temp, degb + i), Enum.at(b, degb)) end)

        temp =
          Enum.reduce(0..degb, temp, fn c, temp ->
            List.update_at(temp, c + i, fn val -> val - Enum.at(o, c) end)
          end)

        {o, temp}
      end)

    Enum.take(o, deg(o) + 1)
  end
end
