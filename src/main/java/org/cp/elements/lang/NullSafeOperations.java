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
package org.cp.elements.lang;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract base class for performing {@literal null-safe} operations.
 * <p>
 * The null-safe methods in this class can be used to replace operations such as the following:
 * <p>
 * <code>
 *   if (properties != null) {
 *     map.putAll(properties);
 *   }
 * </code>
 * <p>
 * Which can be written as:
 * <p>
 * <code>
 *   NullSafeOperations.ifNotNullDo(properties, map::putAll);
 * </code>
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see java.util.function.Supplier
 * @since 1.0.0
 */
public abstract class NullSafeOperations {

  /**
   * Null-safe method used to conditionally invoke the given, required {@link Consumer} when the {@link Object target}
   * is not {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object target}.
   * @param target {@link Object} evaluated for a {@literal null} reference and conditionally passed to
   * the {@link Consumer} for processing.
   * @param consumer {@link Consumer} used to process the {@link Object target} if the {@link Object target}
   * is not {@literal null}; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Consumer} is {@literal null}.
   * @see java.util.function.Consumer
   */
  @NullSafe
  public static <T> void ifNotNullDo(@Nullable T target, @NotNull Consumer<T> consumer) {

    Assert.notNull(consumer, "The Consumer used to process the target Object is required");

    if (target != null) {
      consumer.accept(target);
    }
  }

  /**
   * Null-safe method used to conditionally invoke the given, required {@link Function} when the {@link Object target}
   * is not {@literal null} or return the value from the given, required {@link Supplier} when the {@link Object target}
   * is {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object target}.
   * @param <S> {@link Class type} of the value returned by the {@link Function} and {@link Supplier}.
   * @param target {@link Object} evaluated for a {@literal null} reference and conditionally passed to
   * the {@link Function} for processing.
   * @param function {@link Function} used to process the {@link Object target} when not {@literal null},
   * returning the computed {@link S value}; must not be {@literal null}.
   * @param supplier {@link Supplier} called for the {@link S return value} when the {@link Object target}
   * is {@literal null}; must not be {@literal null}.
   * @return the {@link S value} computed and returned from the {@link Function} after processing
   * the {@link Object target} when not {@literal null} or the {@link S value} returned from the {@link Supplier}
   * when the {@link Object target} is {@literal null}.
   * @throws IllegalArgumentException if the {@link Function} or the {@link Supplier} is {@literal null}.
   * @see java.util.function.Function
   * @see java.util.function.Supplier
   */
  @NullSafe
  public static @Nullable <T, S> S ifNotNullDoOrReturn(@Nullable T target,
      @NotNull Function<T, S> function, @NotNull Supplier<S> supplier) {

    Assert.notNull(function, "The Function used to process the target Object is required");
    Assert.notNull(supplier, "The Supplier to call when the target Object is null is required");

    return Optional.ofNullable(target)
      .map(function)
      .orElseGet(supplier);
  }
}
