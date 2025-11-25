# Forage Availability Limits in the Animal Units Calculator

## Ecological basis
- Rangeland professionals rarely expect livestock to consume 100% of standing biomass. Leaving ungrazed forage maintains soil cover, supports plant recovery, buffers wildlife habitat, and protects against erosion and compaction.
- Utilization guidelines commonly cap use between 25–50% of current forage to sustain plant vigor, with higher caps reserved for resilient, irrigated, or short-duration systems. The limit factor slider (10–100%) allows users to mirror those site-specific stocking policies.
- Incorporating an explicit limit also translates better to on-the-ground planning: when rainfall or grazing access reduces what animals can reach, the model now scales the available forage pool accordingly.

## Mathematical basis
- Each plot contributes an estimated amount of grazeable forage: `w_i * g_i * K`, where `w_i` is dry weight per square foot, `g_i` is grass proportion (as a 0–1 share), and `K` converts square-foot grams into pounds per acre.
- The application sums those contributions across `n` valid samples to obtain the total standing forage per acre. Previously, this full amount flowed into the AU estimate, but that implied 100% utilization.
- The new limit factor `L` expresses the usable share as a proportion (e.g., 40% → `L = 0.40`). Multiplying the summed forage by `L` yields the forage pool that grazing animals can access.
- Annual Animal Units are then computed as:
  
  ```
  AUs = (A * L * Σ(w_i * g_i * K)) / (2 * C * n)
  ```
  
  Where `A` is acreage, `C` is the annual forage intake per AU (lbs), and the halving factor (`2`) preserves the original model’s conservative adjustment.
- Because `L` is independent of the raw field data, changing the limit factor proportionally rescales AU output and exported results without altering the underlying sample statistics.

## Practical implications for users
- Lowering `L` (e.g., from 60% to 40%) reduces estimated AUs by the same proportion, aligning stocking guidance with conservative utilization plans.
- Raising `L` simulates more aggressive forage use, which may be appropriate for rotational grazing with ample rest periods but can risk overuse in continuous grazing settings.
- Including `L` in the exported metadata ensures downstream reviewers can see the utilization ceiling assumed in each calculation.
