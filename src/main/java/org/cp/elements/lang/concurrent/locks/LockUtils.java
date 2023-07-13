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
package org.cp.elements.lang.concurrent.locks;

import java.util.concurrent.locks.Lock;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract utility class for using {@link Lock Locks}.
 *
 * @author John Blum
 * @see java.util.concurrent.locks.Lock
 * @since 2.0.0
 */
public abstract class LockUtils {

  /**
   * Attempts to execute the given {@link Runnable code} while holding the given, required {@link Lock}.
   *
   * @param lock {@link Lock} to acquire while running the {@link Runnable code}; must not be {@literal null}.
   * @param runner {@link Runnable code} to run while holding the given {@link Lock}; must not be {@literal null}.
   * @throws IllegalArgumentException if either the {@link Lock} or {@link Runnable} are {@literal null}.
   * @see java.util.concurrent.locks.Lock
   * @see java.lang.Runnable
   */
  public static void doWithLock(@NotNull Lock lock, @NotNull Runnable runner) {

    Assert.notNull(lock, "Lock is required");
    Assert.notNull(runner, "Code to run with Lock is required");

    try {
      lock.lockInterruptibly();
      runner.run();
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
    }
    finally {
      lock.unlock();
    }
  }

  /**
   * Attempts to execute the given {@link Supplier code} while holding the given, required {@link Lock}.
   *
   * @param <T> {@link Class type} of {@link Object result} returned from the execution
   * of the given {@link Supplier code}.
   * @param lock {@link Lock} to acquire while executing the {@link Runnable code}; must not be {@literal null}.
   * @param supplier {@link Supplier code} to execute while holding the given {@link Lock}; must not be {@literal null}.
   * @return the result of executing the given {@link Supplier code}.
   * @throws IllegalArgumentException if either the {@link Lock} or {@link Runnable} are {@literal null}.
   * @see java.util.concurrent.locks.Lock
   * @see java.lang.Runnable
   */
  public static <T> T doWithLock(@NotNull Lock lock, @NotNull Supplier<T> supplier) {

    Assert.notNull(lock, "Lock is required");
    Assert.notNull(supplier, "Code to run with Lock is required");

    try {
      lock.lockInterruptibly();
      return supplier.get();
    }
    catch (InterruptedException cause) {
      Thread.currentThread().interrupt();
      String message = String.format("Thread was interrupted while trying to obtain lock [%s]", lock);
      throw (IllegalMonitorStateException) new IllegalMonitorStateException(message).initCause(cause);
    }
    finally {
      lock.unlock();
    }
  }
}
