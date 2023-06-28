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
package org.cp.elements.beans;

import java.io.IOException;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import net.datafaker.Faker;

import org.cp.elements.security.model.User;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

/**
 * {@link Benchmark Benchmarks} for {@link AbstractBean}.
 *
 * @author John Blum
 * @see org.openjdk.jmh.annotations.Benchmark
 * @since 2.0.0
 */
@BenchmarkMode(Mode.Throughput)
@Fork(1)
@Warmup(iterations = 1, time = 3)
@Measurement(iterations = 3, time = 3)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
@SuppressWarnings("unused")
public class AbstractBeanBenchmark {

  private static final int BENCHMARK_WORK_LOAD_SIZE = 1000;
  private static final int SAMPLE_DATA_SET_SIZE = 500;

  public static void main(String[] args) throws IOException {
    org.openjdk.jmh.Main.main(args);
  }

  private final Random random = new Random(System.currentTimeMillis());

  private final String[] names = new String[SAMPLE_DATA_SET_SIZE];

  @Setup(Level.Trial)
  public void setup() {

    Faker faker = new Faker();

    for (int index = 0; index < this.names.length; index++) {
      this.names[index] = faker.name().fullName();
    }
  }

  private void setName(AbstractPerson person) {

    for (int count = 0; count < BENCHMARK_WORK_LOAD_SIZE; count++) {
      person.setName(this.names[random.nextInt(SAMPLE_DATA_SET_SIZE)]);
    }
  }

  @Benchmark
  @OperationsPerInvocation(BENCHMARK_WORK_LOAD_SIZE)
  public void setNameUsingCallbackLambda(Blackhole blackhole) {
    setName(new PersonUsingCallbackLambda());
  }

  @Benchmark
  @OperationsPerInvocation(BENCHMARK_WORK_LOAD_SIZE)
  public void setNameUsingCallbackMethodReference(Blackhole blackhole) {
    setName(new PersonUsingCallbackMethodReference());
  }

  @Benchmark
  @OperationsPerInvocation(BENCHMARK_WORK_LOAD_SIZE)
  public void setNameUsingRegisteredCallbackLambda(Blackhole blackhole) {
    setName(new PersonUsingRegisteredCallbackLambda());
  }

  @Benchmark
  @OperationsPerInvocation(BENCHMARK_WORK_LOAD_SIZE)
  public void setNamedUsingReflection(Blackhole blackhole) {
    setName(new PersonUsingReflection());
  }

  @Benchmark
  @OperationsPerInvocation(BENCHMARK_WORK_LOAD_SIZE)
  public void setNamedUsingSetter(Blackhole blackhole) {
    setName(new PersonUsingSetter());
  }

  static class AbstractPerson extends AbstractBean<Integer, User<Integer>, Object> {

    String name;

    public String getName() {
      return this.name;
    }

    public void setName(String name) {
      this.name = name;
    }
  }


  static class PersonUsingCallbackLambda extends AbstractPerson {

    @Override
    public void setName(String name) {
      processChange("name", this.name, name, newName -> this.name = newName);
    }
  }

  static class PersonUsingCallbackMethodReference extends AbstractPerson {

    @Override
    public void setName(String name) {
      processChange("name", this.name, name, super::setName);
    }
  }

  static class PersonUsingRegisteredCallbackLambda extends AbstractPerson {

    PersonUsingRegisteredCallbackLambda() {
      mapPropertyNameToStateChangeCallback("name", newName -> this.name = String.valueOf(newName));
    }

    @Override
    public void setName(String name) {
      processChange("name", this.name, name);
    }
  }

  static class PersonUsingReflection extends AbstractPerson {

    @Override
    public void setName(String name) {
      processChange("name", this.name, name);
    }
  }

  static class PersonUsingSetter extends AbstractPerson {

  }
}
