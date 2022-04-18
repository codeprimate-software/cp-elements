/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.function;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract utility class containing canned, useful {@link Function Functions}.
 *
 * @author John Blum
 * @see java.util.function.Consumer
 * @see java.util.function.Function
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FunctionUtils {

  /**
   * Adapts the given, required {@link Function} as a {@link Consumer}.
   *
   * @param <T> {@link Class type} of the {@link Function Function's} and {@link Consumer Consumer's} argument.
   * @param <R> {@link Class type} of the {@link Function Function's} return value.
   * @param function {@link Function} to adapt; must not be {@literal null}.
   * @return a {@link Consumer} implementation adapting the given, required {@link Function}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see java.util.function.Consumer
   * @see java.util.function.Function
   */
  public static @NotNull <T, R> Consumer<T> toConsumer(@NotNull Function<T, R> function) {

    Assert.notNull(function, "Function is required");

    return function::apply;
  }

  /**
   * Adapts the given, required {@link Consumer} as a {@link Function}.
   *
   * @param <T> {@link Class type} of {@link Object} processed by the {@link Consumer} and {@link Function}.
   * @param consumer {@link Consumer} to adapt; must not be {@literal null}.
   * @return a {@link Function} implementation adapting the given, required {@link Consumer}.
   * @throws IllegalArgumentException if the {@link Consumer} is {@literal null}.
   * @see java.util.function.Consumer
   * @see java.util.function.Function
   */
  public static @NotNull <T> Function<T, T> toFunction(@NotNull Consumer<T> consumer) {

    Assert.notNull(consumer, "Consumer is required");

    return target -> {
      consumer.accept(target);
      return target;
    };
  }

  /**
   * Adapts the given, required {@link Consumer} as a {@link Function}.
   *
   * @param <T> {@link Class type} of the {@link Function} argument.
   * @param <R> {@link Class type} of {@link Object} returned by the {@link Supplier} and {@link Function}.
   * @param supplier {@link Supplier} to adapt; must not be {@literal null}.
   * @return a {@link Function} implementation adapting the given, required {@link Supplier}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see java.util.function.Function
   * @see java.util.function.Supplier
   */
  public static @NotNull <T, R> Function<T, R> toFunction(@NotNull Supplier<R> supplier) {

    Assert.notNull(supplier, "Supplier is required");

    return argument -> supplier.get();
  }

  /**
   * Adapts the given, required {@link Function} as a {@link Supplier}.
   *
   * @param <T> {@link Class type} of the {@link Function Function's} argument.
   * @param <R> {@link Class type} of the {@link Function Function's} and {@link Supplier Supplier's} return value.
   * @param function {@link Function} to adapt; must not be {@literal null}.
   * @return a {@link Supplier} implementation adapting the given, required {@link Function}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see java.util.function.Supplier
   */
  public static @NotNull <T, R> Supplier<R> toSupplier(@NotNull Function<T, R> function) {

    Assert.notNull(function, "Function is required");

    return () -> function.apply(null);
  }
}
