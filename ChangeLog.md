# 0.5.0.0, 2018-11-25
- reformulation of Multilinear class - added Generic and NFData superclasses. This allows any Multilinear tensor to be derived generically in e.g. serialization instances and also fully evaluated for performace reasons
- added simple Sequential <=> MultiCore converters in Multilinear.Generic module

# 0.4.0.0, 2018-11-24
- added multi-core parallelism, sample benchmark is available in ./benchmark/results.zip
- more robust QuickCheck tests
- some API changes; simplified Multilinear class

# 0.3.2.0, 2018-11-18
- added filter and zipWith functions

# 0.3.1.0, 2018-11-17
- added more complex QuickCheck tests
- and therefore, some error fixes

# 0.3.0.0, 2018-11-14
- moved to Vector.Unboxed, to improve performance
- simplified error handling - removed separate Err Tensor value
- temporarily removed Infinite tensors constructors and Generic
- added memory benchmark
- added simple unit tests

# 0.2.3, 2018-11-02
- fixed conflicting dependencies
- added criterion benchmarks

# 0.2.2, 2018-11-01
## Dependencies update
- ported to LTS-12.16 resolver

# 0.2.1, 2018-10-31
Initial release
