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

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class containing canned, useful {@link Function Functions} and {@literal Adapters}
 * for {@literal java.util.function} {@link Class types}.
 *
 * @author John Blum
 * @see java.util.function.Consumer
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see java.util.function.Supplier
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FunctionUtils {

  /**
   * Composes the given array of {@link Function Functions} into a single {@link Function}
   * using the {@literal Composite Software Design Pattern}.
   *
   * @param functions array of {@link Function Functions} to compose.
   * @return a single {@literal non-null} {@link Function} composed from
   * the given array of {@link Function Functions}.
   * @see java.util.function.Function
   */
  @NullSafe
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static @NotNull Function compose(@NotNull Function... functions) {

    return Arrays.stream(ArrayUtils.nullSafeArray(functions, Function.class))
      .filter(Objects::nonNull)
      .reduce(Function::andThen)
      .orElseGet(Function::identity);
  }

  /**
   * Factory method used to construct a new instance of {@link Consumer} that does nothing.
   *
   * @param <T> {@link Class type} of {@link Object} processed by the {@link Consumer}.
   * @return a noop {@link Consumer}.
   * @see java.util.function.Consumer
   */
  public static @NotNull <T> Consumer<T> noopConsumer() {
    return argument -> {};
  }

  /**
   * Null-safe method used to guard against a {@literal null} {@link Consumer}.
   *
   * @param <T> {@link Class type} of the {@link Consumer Consumer's} {@link Object argument}.
   * @param consumer {@link Consumer} to evaluate.
   * @return the given {@link Consumer} if not {@literal null} or a noop {@link Consumer}.
   * @see java.util.function.Consumer
   */
  @NullSafe
  public static @NotNull <T> Consumer<T> nullSafeConsumer(@Nullable Consumer<T> consumer) {
    return consumer != null ? consumer : noopConsumer();
  }

  /**
   * Null-safe method used to guard against a {@literal null} {@link Predicate} reference.
   *
   * @param <T> {@link Class type} of {@link Object} tested by the {{@link Predicate}.
   * @param predicate {@link Predicate} to evaluate for {@literal null}.
   * @return the given {@link Predicate} if not {@literal null} or a new {@link Predicate}
   * with a {@link Predicate#test(Object)} that always evaluates to {@literal true}.
   * @see #nullSafePredicateMatchNone(Predicate)
   * @see java.util.function.Predicate
   */
  @NullSafe
  public static @NotNull <T> Predicate<T> nullSafePredicateMatchAll(@Nullable Predicate<T> predicate) {
    return predicate != null ? predicate : argument -> true;
  }

  /**
   * Null-safe method used to guard against a {@literal null} {@link Predicate} reference.
   *
   * @param <T> {@link Class type} of {@link Object} tested by the {{@link Predicate}.
   * @param predicate {@link Predicate} to evaluate for {@literal null}.
   * @return the given {@link Predicate} if not {@literal null} or a new {@link Predicate}
   * with a {@link Predicate#test(Object)} that always evaluates to {@literal false}.
   * @see #nullSafePredicateMatchAll(Predicate)
   * @see java.util.function.Predicate
   */
  @NullSafe
  public static @NotNull <T> Predicate<T> nullSafePredicateMatchNone(@Nullable Predicate<T> predicate) {
    return predicate != null ? predicate : argument -> false;
  }

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
   * Adapts the given, required {@link Predicate} as a {@link Function}.
   *
   * @param <T> {@link Class type} of {@link Object} processed by the {@link Predicate} and {@link Function}.
   * @param predicate {@link Predicate} to adapt; must not be {@literal null}.
   * @return a {@link Function} implementation adapting the given, required {@link Predicate}.
   * @throws IllegalArgumentException if the {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.function.Function
   */
  public static @NotNull <T> Function<T, Boolean> toFunction(@NotNull Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return predicate::test;
  }

  /**
   * Adapts the given, required {@link Consumer} as a {@link Function}.
   *
   * @param <T> {@link Class type} of the {@link Function Function's} argument.
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
   * Adapts the given, required {@link Function} as a {@link Predicate}.
   *
   * @param <T> {@link Class type} of the {@link Function Function's} argument.
   * @param function {@link Function} to adapt; must not be {@literal null}.
   * @return a {@link Predicate} implementation adapting the given, required {@link Function}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see java.util.function.Predicate
   */
  public static @NotNull <T> Predicate<T> toPredicate(@NotNull Function<T, Boolean> function) {

    Assert.notNull(function, "Function is required");

    return function::apply;
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
