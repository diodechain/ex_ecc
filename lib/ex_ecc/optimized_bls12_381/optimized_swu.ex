defmodule ExEcc.OptimizedBLS12381.OptimizedSWU do
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.OptimizedBLS12381.Constants
  alias ExEcc.FieldMath

  # Optimized SWU Map - FQ to G1'
  # Found in Section 4 of https://eprint.iacr.org/2019/403
  def optimized_swu_g1(t) do
    t2 = FieldMath.mul(t, t)
    iso_11_z_t2 = FieldMath.mul(Constants.iso_11_z(), t2)
    temp = FieldMath.add(iso_11_z_t2, FieldMath.mul(iso_11_z_t2, iso_11_z_t2))
    # -a(Z * t^2 + Z^2 * t^4)
    denominator = FieldMath.neg(FieldMath.mul(Constants.iso_11_a(), temp))
    temp_plus_one = FieldMath.add(temp, FieldMath.type(temp).one())
    # b(Z * t^2 + Z^2 * t^4 + 1)
    numerator = FieldMath.mul(Constants.iso_11_b(), temp_plus_one)

    # Exceptional case
    denominator =
      if denominator == FieldMath.type(denominator).zero() do
        FieldMath.mul(Constants.iso_11_z(), Constants.iso_11_a())
      else
        denominator
      end

    # v = D^3
    v = FieldMath.pow(denominator, 3)
    # u = N^3 + a * N * D^2 + b* D^3
    u =
      FieldMath.add(
        FieldMath.pow(numerator, 3),
        FieldMath.add(
          FieldMath.mul(
            Constants.iso_11_a(),
            FieldMath.mul(numerator, FieldMath.pow(denominator, 2))
          ),
          FieldMath.mul(Constants.iso_11_b(), v)
        )
      )

    # Attempt y = sqrt(u / v)
    {is_root, y_intermediate} = sqrt_division_fq(u, v)
    y = y_intermediate

    y =
      if not is_root do
        FieldMath.mul(FieldMath.mul(y, FieldMath.pow(t, 3)), Constants.sqrt_minus_11_cubed())
      else
        y
      end

    numerator =
      if not is_root do
        FieldMath.mul(numerator, iso_11_z_t2)
      else
        numerator
      end

    y =
      if FieldMath.sgn0(t) != FieldMath.sgn0(y) do
        FieldMath.neg(y)
      else
        y
      end

    y_final = FieldMath.mul(y, denominator)

    {numerator, y_final, denominator}
  end

  # Optimized SWU Map - FQ2 to G2': y^2 = x^3 + 240i * x + 1012 + 1012i
  # Found in Section 4 of https://eprint.iacr.org/2019/403
  def optimized_swu_g2(t) do
    t2 = FieldMath.mul(t, t)
    iso_3_z_t2 = FieldMath.mul(Constants.iso_3_z(), t2)
    temp = FieldMath.add(iso_3_z_t2, FieldMath.mul(iso_3_z_t2, iso_3_z_t2))
    # The initial calculation for denominator_val for optimized_swu_g2
    calculated_denominator = FieldMath.neg(FieldMath.mul(Constants.iso_3_a(), temp))
    temp_plus_one = FieldMath.add(temp, FieldMath.type(temp).one())
    # b(Z * t^2 + Z^2 * t^4 + 1)
    numerator_val = FieldMath.mul(Constants.iso_3_b(), temp_plus_one)

    # Exceptional case for denominator_val
    denominator_val =
      if calculated_denominator == FQ2.zero() do
        FieldMath.mul(Constants.iso_3_z(), Constants.iso_3_a())
      else
        calculated_denominator
      end

    # v = D^3
    v = FieldMath.pow(denominator_val, 3)
    # u = N^3 + a * N * D^2 + b* D^3
    u_val =
      FieldMath.add(
        FieldMath.pow(numerator_val, 3),
        FieldMath.add(
          FieldMath.mul(
            Constants.iso_3_a(),
            FieldMath.mul(numerator_val, FieldMath.pow(denominator_val, 2))
          ),
          FieldMath.mul(Constants.iso_3_b(), v)
        )
      )

    # Attempt y = sqrt(u / v)
    {success, sqrt_candidate_initial} = sqrt_division_fq2(u_val, v)
    y = sqrt_candidate_initial

    # Handle case where (u / v) is not square
    # sqrt_candidate(x1) = sqrt_candidate(x0) * t^3
    sqrt_candidate_transformed = FieldMath.mul(sqrt_candidate_initial, FieldMath.pow(t, 3))

    # u(x1) = Z^3 * t^6 * u(x0)
    u_transformed = FieldMath.mul(FieldMath.pow(iso_3_z_t2, 3), u_val)

    # Reduce to find the correct y
    {final_y, success_2_final, final_numerator} =
      Enum.reduce_while(Constants.etas(), {y, false, numerator_val}, fn eta,
                                                                        {current_y, success_2_acc,
                                                                         current_numerator} ->
        eta_sqrt_candidate = FieldMath.mul(eta, sqrt_candidate_transformed)

        temp1 =
          FieldMath.sub(
            FieldMath.mul(FieldMath.mul(eta_sqrt_candidate, eta_sqrt_candidate), v),
            u_transformed
          )

        if temp1 == FieldMath.type(temp1).zero() and not success and not success_2_acc do
          {:halt, {eta_sqrt_candidate, true, current_numerator}}
        else
          {:cont, {current_y, success_2_acc, current_numerator}}
        end
      end)

    # This case should ideally be unreachable if the algorithm is correct.
    if not success and not success_2_final do
      raise "Hash to Curve - Optimized SWU failure"
    end

    numerator_final =
      if not success do
        FieldMath.mul(final_numerator, iso_3_z_t2)
      else
        final_numerator
      end

    y_corrected_sign =
      if FieldMath.sgn0(t) != FieldMath.sgn0(final_y) do
        FieldMath.neg(final_y)
      else
        final_y
      end

    y_overall_final = FieldMath.mul(y_corrected_sign, denominator_val)

    {numerator_final, y_overall_final, denominator_val}
  end

  def sqrt_division_fq(u, v) do
    temp = FieldMath.mul(u, v)
    # P_MINUS_3_DIV_4 needs to be defined in Constants or passed if it's dynamic.
    # For now, assuming it is available via Constants module.
    exponent = Constants.p_minus_3_div_4()

    result =
      FieldMath.mul(temp, FieldMath.pow(FieldMath.mul(temp, FieldMath.pow(v, 2)), exponent))

    is_valid_root =
      FieldMath.sub(FieldMath.mul(FieldMath.mul(result, result), v), u) ==
        FieldMath.type(u).zero()

    {is_valid_root, result}
  end

  # Square Root Division
  # Return: uv^7 * (uv^15)^((p^2 - 9) / 16) * root of unity
  # If valid square root is found return true, else false
  def sqrt_division_fq2(u, v) do
    temp1 = FieldMath.mul(u, FieldMath.pow(v, 7))
    temp2 = FieldMath.mul(temp1, FieldMath.pow(v, 8))

    # P_MINUS_9_DIV_16 needs to be available via Constants.
    exponent = Constants.p_minus_9_div_16()
    gamma = FieldMath.mul(FieldMath.pow(temp2, exponent), temp1)

    # Verify there is a valid root
    # POSITIVE_EIGHTH_ROOTS_OF_UNITY needs to be available via Constants.
    initial_accumulator = {false, gamma}

    {is_valid_root, result} =
      Enum.reduce_while(Constants.positive_eighth_roots_of_unity(), initial_accumulator, fn root,
                                                                                            {acc_is_valid,
                                                                                             acc_result} ->
        sqrt_candidate = FieldMath.mul(root, gamma)

        check_val =
          FieldMath.sub(FieldMath.mul(FieldMath.mul(sqrt_candidate, sqrt_candidate), v), u)

        if check_val == FieldMath.type(check_val).zero() and not acc_is_valid do
          {:halt, {true, sqrt_candidate}}
        else
          {:cont, {acc_is_valid, acc_result}}
        end
      end)

    {is_valid_root, result}
  end

  # Optimal Map from 3-Isogenous Curve to G2
  def iso_map_g2(x, y, z) do
    # x-numerator, x-denominator, y-numerator, y-denominator
    # Initialize with FQ2 zero values
    mapped_values = List.duplicate(FieldMath.type(z).zero(), 4)
    # z, z^2, z^3
    z_powers = [z, FieldMath.pow(z, 2), FieldMath.pow(z, 3)]

    # ISO_3_MAP_COEFFICIENTS comes from Constants
    # Horner Polynomial Evaluation
    {new_mapped_values, _} =
      Enum.reduce(
        Enum.with_index(Constants.iso_3_map_coefficients()),
        {mapped_values, z_powers},
        fn {k_i, i}, {current_mapped_values, current_z_powers} ->
          # last element
          val_i = Enum.at(k_i, -1)

          val_i_updated =
            Enum.reduce(
              Enum.with_index(Enum.reverse(Enum.slice(k_i, 0..(length(k_i) - 2)))),
              val_i,
              fn {k_i_j, j}, acc_val_i ->
                FieldMath.add(
                  FieldMath.mul(acc_val_i, x),
                  FieldMath.mul(Enum.at(current_z_powers, j), k_i_j)
                )
              end
            )

          {List.replace_at(current_mapped_values, i, val_i_updated), current_z_powers}
        end
      )

    x_num = Enum.at(new_mapped_values, 0)
    x_den = Enum.at(new_mapped_values, 1)
    y_num = Enum.at(new_mapped_values, 2)
    y_den = Enum.at(new_mapped_values, 3)

    # y-numerator * y
    y_num_updated = FieldMath.mul(y_num, y)
    # y-denominator * z
    y_den_updated = FieldMath.mul(y_den, z)

    # x-denominator * y-denominator
    z_g2 = FieldMath.mul(x_den, y_den_updated)
    # x-numerator * y-denominator
    x_g2 = FieldMath.mul(x_num, y_den_updated)
    # y-numerator * x-denominator
    y_g2 = FieldMath.mul(x_den, y_num_updated)

    {x_g2, y_g2, z_g2}
  end

  # Optimal Map from 11-Isogenous Curve to G1
  def iso_map_g1(x, y, z) do
    # Initialize with FQ zero values
    mapped_values = List.duplicate(FieldMath.type(z).zero(), 4)

    # Max power needed is 15 for z, assuming ISO_11_MAP_COEFFICIENTS structure
    # Python code has z_powers up to z**15, implying coefficients might need up to that.
    # Let's generate powers of z up to a reasonable limit needed by the coefficients.
    # The maximum length of a coefficient list (minus one for the last term) will determine max power.
    # For now, creating up to z_powers[14] which is z^15, as in Python.
    z_powers = Enum.map(1..15, &FieldMath.pow(z, &1))

    # ISO_11_MAP_COEFFICIENTS from Constants
    # Horner Polynomial Evaluation
    {new_mapped_values, _} =
      Enum.reduce(
        Enum.with_index(Constants.iso_11_map_coefficients()),
        {mapped_values, z_powers},
        fn {k_i, i}, {current_mapped_values, current_z_powers} ->
          # last element
          val_i = Enum.at(k_i, -1)

          val_i_updated =
            Enum.reduce(
              Enum.with_index(Enum.reverse(Enum.slice(k_i, 0..(length(k_i) - 2)))),
              val_i,
              fn {k_i_j, j}, acc_val_i ->
                # Ensure j is within bounds for current_z_powers
                # Python code uses z_powers[j], Elixir Enum.at(current_z_powers, j)
                FieldMath.add(
                  FieldMath.mul(acc_val_i, x),
                  FieldMath.mul(Enum.at(current_z_powers, j), k_i_j)
                )
              end
            )

          {List.replace_at(current_mapped_values, i, val_i_updated), current_z_powers}
        end
      )

    x_num = Enum.at(new_mapped_values, 0)
    x_den = Enum.at(new_mapped_values, 1)
    y_num = Enum.at(new_mapped_values, 2)
    y_den = Enum.at(new_mapped_values, 3)

    # y-numerator * y
    y_num_updated = FieldMath.mul(y_num, y)
    # y-denominator * z
    y_den_updated = FieldMath.mul(y_den, z)

    z_g1 = FieldMath.mul(x_den, y_den_updated)
    x_g1 = FieldMath.mul(x_num, y_den_updated)
    y_g1 = FieldMath.mul(x_den, y_num_updated)

    {x_g1, y_g1, z_g1}
  end
end
