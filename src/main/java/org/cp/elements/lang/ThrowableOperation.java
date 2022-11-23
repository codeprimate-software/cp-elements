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

import static org.cp.elements.lang.ElementsExceptionsFactory.newThrowableOperationException;

import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Operation capable of throwing a {@link Throwable} during execution.
 *
 * Implementation of the {@link Callable}, {@link Consumer}, {@link Runnable} and {@link Supplier} interfaces.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see java.lang.Runnable
 * @see java.util.concurrent.Callable
 * @see java.util.function.Consumer
 * @see java.util.function.Supplier
 * @since 1.0.0
 */
@FunctionalInterface
public interface ThrowableOperation<T> {

  @SuppressWarnings("unused")
  ThrowableOperation<Object> NO_OP = args -> null;

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link Callable}.
   *
   * @param <T> {@link Class type} of the {@link Callable Callable's} {@link Object return value}.
   * @param callable {@link Callable} to adapt/wrap as an instance of {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link Callable}.
   * @throws IllegalArgumentException if the {@link Callable} is {@literal null}.
   * @see java.util.concurrent.Callable
   */
  static @NotNull <T> ThrowableOperation<T> fromCallable(@NotNull Callable<T> callable) {
    Assert.notNull(callable, "Callable is required");
    return args -> callable.call();

  }

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link Consumer}.
   *
   * @param <T> {@link Class type} of the {@link Consumer Consumer's} {@link Object return value};
   * defaults to {@literal null}.
   * @param consumer {@link Consumer} to adapt/wrap as an instance of {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link Consumer}.
   * @throws IllegalArgumentException if the {@link Consumer} is {@literal null}.
   * @see java.util.function.Consumer
   */
  static @NotNull <T> ThrowableOperation<T> fromConsumer(@NotNull Consumer<Object> consumer) {

    Assert.notNull(consumer, "Consumer is required");

    return arguments -> {
      consumer.accept(arguments);
      return null;
    };
  }

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link Function}.
   *
   * @param <T> {@link Class type} of the {@link Function} {@link Object return value}.
   * @param function {@link Function} to adapt/wrap as an instance of {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link Function}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  static @NotNull <T> ThrowableOperation<T> fromFunction(@NotNull Function<Object, T> function) {
    Assert.notNull(function, "Function is required");
    return function::apply;
  }

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link Runnable}.
   *
   * @param <T> {@link Class type} of the {@link Runnable Runnable's} {@link Object return value};
   * defaults to {@literal null}.
   * @param runnable {@link Runnable} to adapt/wrap as an instance of {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link Runnable}.
   * @throws IllegalArgumentException if the {@link Runnable} is {@literal null}.
   * @see java.lang.Runnable
   */
  static @NotNull <T> ThrowableOperation<T> fromRunnable(@NotNull Runnable runnable) {

    Assert.notNull(runnable, "Runnable is required");

    return arguments -> {
      runnable.run();
      return null;
    };
  }

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link Supplier}.
   *
   * @param <T> {@link Class type} of the {@link Supplier Supplier's} {@link Object return value}.
   * @param supplier {@link Supplier} to adapt/wrap as an instance of {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link Supplier}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see java.util.function.Supplier
   */
  static @NotNull <T> ThrowableOperation<T> fromSupplier(@NotNull Supplier<T> supplier) {
    Assert.notNull(supplier, "Supplier is required");
    return arguments -> supplier.get();
  }

  /**
   * Factory method used to construct a new instance of {@link ThrowableOperation} initialized from
   * (adapting/wrapping) the given, required {@link VoidReturningThrowableOperation}.
   *
   * @param <T> {@link Class type} of the {@link Object return value}.
   * @param operation {@link VoidReturningThrowableOperation} to adapt/wrap as a {@link ThrowableOperation};
   * must not be {@literal null}.
   * @return a new {@link ThrowableOperation} from the given, required {@link VoidReturningThrowableOperation}
   * @throws IllegalArgumentException if the {@link VoidReturningThrowableOperation} is {@literal null}.
   * @see VoidReturningThrowableOperation
   */
  static @NotNull <T> ThrowableOperation<T> fromVoidReturning(@NotNull VoidReturningThrowableOperation operation) {

    return args -> {
      ObjectUtils.requireObject(operation, "VoidReturningThrowableOperation is required").run(args);
      return null;
    };
  }

  /**
   * Adapts (wraps) this {@link ThrowableOperation} as a {@link Callable}.
   *
   * @return a {@link Callable} object adapting/wrapping this {@link ThrowableOperation}.
   * @see java.util.concurrent.Callable
   */
  default @NotNull Callable<T> asCallable() {
    return this::safeRun;
  }

  /**
   * Adapts (wraps) this {@link ThrowableOperation} as a {@link Consumer}.
   *
   * @return a {@link Consumer} object adapting/wrapping this {@link ThrowableOperation}.
   * @see java.util.function.Consumer
   */
  default @NotNull Consumer<Object> asConsumer() {
    return this::safeRun;
  }

  /**
   * Adapts (wraps) this {@link ThrowableOperation} as a {@link Function}.
   *
   * @return a {@link Function} object adapting/wrapping this {@link ThrowableOperation}.
   * @see java.util.function.Function
   */
  default @NotNull Function<Object, T> asFunction() {
    return this::safeRun;
  }

  /**
   * Adapts (wraps) this {@link ThrowableOperation} as a {@link Runnable}.
   *
   * @return a {@link Runnable} object adapting/wrapping this {@link ThrowableOperation}.
   * @see java.lang.Runnable
   */
  default @NotNull Runnable asRunnable() {
    return this::safeRun;
  }

  /**
   * Adapts (wraps) this {@link ThrowableOperation} as a {@link Supplier}.
   *
   * @return a {@link Supplier} object adapting/wrapping this {@link ThrowableOperation}.
   * @see java.util.function.Supplier
   */
  default @NotNull Supplier<T> asSupplier() {
    return this::safeRun;
  }

  /**
   * Performs an operation on the given, optional arguments.
   *
   * @param args optional array of {@link Object arguments} to process.
   * @return the {@link Object result} of this operation.
   * @throws Throwable if the operation fails.
   */
  T run(Object... args) throws Throwable;

  /**
   * Safely {@link #run(Object...) runs} this {@link ThrowableOperation} by handling any {@link Throwable object}
   * thrown during the execution of this {@link ThrowableOperation}.
   *
   * @param args optional array of {@link Object arguments} to process.
   * @return the {@link Object result} of this operation.
   * @throws ThrowableOperationException wrapping any {@link Throwable} thrown by this {@link ThrowableOperation}.
   * @see #run(Object...)
   */
  default T safeRun(Object... args) {

    try {
      return run(args);
    }
    catch (Throwable cause) {
      throw newThrowableOperationException(cause, "Failed to run ThrowableOperation [%s]", this);
    }
  }

  /**
   * Builder method used to compose this {@link ThrowableOperation} with the given {@link ThrowableOperation}.
   *
   * @param operation {@link ThrowableOperation} to compose with this {@link ThrowableOperation}.
   * @return a composed {@link ThrowableOperation} consisting of this {@link ThrowableOperation}
   * and the given {@link ThrowableOperation}. If the given {@link ThrowableOperation} is {@literal null},
   * then {@literal this} {@link ThrowableOperation} is returned.
   * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Composite Software Design Pattern</a>
   */
  @NullSafe
  default @NotNull ThrowableOperation<T> andThen(@Nullable ThrowableOperation<T> operation) {

    return operation != null
      ? arguments -> operation.run(this.run(arguments))
      : this;
  }

  /**
   * Interface defining a contract for implementing {@link Object Objects} that perform an operation
   * capable of throwing a {@link Throwable}.
   */
  interface VoidReturningThrowableOperation {
    void run(Object... args) throws Throwable;
  }
}
