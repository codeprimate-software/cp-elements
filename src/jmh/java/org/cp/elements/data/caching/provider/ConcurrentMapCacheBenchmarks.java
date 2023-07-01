/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cp.elements.data.caching.provider;

import java.io.IOException;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.annotation.NotNull;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

/**
 * {@link Benchmark Benchmarks} for {@link ConcurrentMapCache}.
 *
 * @author John Blum
 * @see org.openjdk.jmh.annotations.Benchmark
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @since 1.0.0
 */
@BenchmarkMode(Mode.Throughput)
@Fork(1)
@Warmup(iterations = 1, time = 3)
@Measurement(iterations = 3, time = 3)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
@SuppressWarnings("unused")
public class ConcurrentMapCacheBenchmarks {

  private static final int KEY_SET_SIZE = 1000;
  private static final int WORKLOAD_SIZE = 100_000;

  public static void main(String[] args) throws IOException {
    org.openjdk.jmh.Main.main(args);
  }

  private final Cache<Integer, Integer> cache = new ConcurrentMapCache<>();

  private final Random randomKeyGenerator = new Random(System.currentTimeMillis());

  @Benchmark
  @OperationsPerInvocation(WORKLOAD_SIZE)
  public void getAndPutBenchmark(@NotNull Blackhole blackhole) {

    for (int count = 0; count < WORKLOAD_SIZE; count++) {
      int key = this.randomKeyGenerator.nextInt(KEY_SET_SIZE);
      Integer value = this.cache.get(key);
      value = value != null ? ++value : count;
      this.cache.put(key, value);
    }
  }
}
