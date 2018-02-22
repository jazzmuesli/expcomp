# Explainable computations in scala.

## Example 1:

> pi=3.141592653589793

> e=2.718281828459045

> pie=(5.859874482048838=3.141592653589793 + 2.718281828459045)

> roundedPie=(5.86=(5.859874482048838=3.141592653589793 + 2.718281828459045) round 3.0)

## Example 2:

> EURUSD=Rate(bid=1.2333, offer=1.2338)

> AUDUSD=Rate(bid=(0.7833=0.7843 - (0.001=0.002 * 0.5)), offer=(0.7853=0.7843 + (0.001=0.002 * 0.5)))=SpreadedRate(mid=0.7843, spread=0.002)

> EURAUD=Rate(bid=(1.5705=(1.5704826181077296=1.2333 / (0.7853=0.7843 + (0.001=0.002 * 0.5))) round 4.0), offer=(1.5751=(1.575130856632197=1.2338 / (0.7833=0.7843 - (0.001=0.002 * 0.5))) round 4.0))


## Disadvantages

As you can see in ExplainableComputationsTest::testPerformance, this approach is 60x slower than raw arithmetic operations on doubles.