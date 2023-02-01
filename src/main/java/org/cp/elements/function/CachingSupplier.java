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
package org.cp.elements.function;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Supplier} implementation that supports caching.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link CachingSupplier Supplier's} {@link Object return value}.
 * @see java.util.function.Supplier
 * @since 1.0.1
 */
public interface CachingSupplier<T> extends Supplier<T> {

  /**
   * Factory method used to construct a new instance of {@link CachingSupplier} to wrap and {@literal decorate}
   * the given, required {@link Supplier}.
   *
   * @param <T> {@link Class type} of the {@link Supplier Supplier's} {@link Object return value}.
   * @param supplier {@link Supplier} to {@literal decorate} with caching capabilities; must not be {@literal null}.
   * @return a new {@link CachingSupplier} {@literal decorating} the given, required {@link Supplier}.
   * @throws IllegalArgumentException if the given {@link Supplier} is {@literal null}.
   * @see java.util.function.Supplier
   */
  static @NotNull <T> CachingSupplier<T> from(@NotNull Supplier<T> supplier) {

    Assert.notNull(supplier, "Supplier is required");

    AtomicReference<T> cache = new AtomicReference<>(null);

    return new CachingSupplier<T>() {

      @Override
      public void clear() {
        cache.set(null);
      }

      @Override
      public @Nullable T get() {
        return cache.updateAndGet(value -> value != null ? value : supplier.get());
      }
    };
  }

  /**
   * Clears the cached {@link T value} of the {@literal decorated} {@link Supplier}.
   */
  void clear();

}
