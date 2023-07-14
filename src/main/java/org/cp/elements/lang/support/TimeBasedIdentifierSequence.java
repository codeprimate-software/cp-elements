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
package org.cp.elements.lang.support;

import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * Implementation of the {@link IdentifierSequence} interface generating unique, {@link Long Long-typed} identifiers
 * based on {@link System#nanoTime() time}.
 * <p>
 * In particular, this implementation uses the {@link System} clock and is {@link ThreadSafe Thread-safe}.
 * <p>
 * While this maybe useful for testing purposes, it should not be used in distributed systems
 * where multiple application or system components may generate ID's using different instances
 * of this class in separate JVMs. This is particularly important with respect to clock skew.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see java.lang.System
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class TimeBasedIdentifierSequence implements IdentifierSequence<Long> {

  private static long currentIdentifier = -1L;

  /**
   * Generates the next unique identifier (ID) in sequence in a global context, across all instances of this class
   * in the same JVM (or rather, {@link ClassLoader}).
   *
   * @return the next globally unique {@link Long} identifier (ID) in the sequence.
   * @see java.lang.System#nanoTime()
   */
  protected static synchronized long nextGlobalId() {

    long newIdentifier = System.nanoTime();

    while (newIdentifier <= currentIdentifier) {
      newIdentifier = System.nanoTime();
    }

    currentIdentifier = newIdentifier;

    return newIdentifier;
  }

  /**
   * Sets the global identifier to the current value.
   *
   * @param id {@link Long} value to use as the base identifier.
   */
  static synchronized void setGlobalId(long id) {
    currentIdentifier = id;
  }

  /**
   * Generates the next unique ID in sequence.
   *
   * @return the next unique Long ID in the sequence.
   * @see #nextGlobalId()
   */
  @Override
  public synchronized @NotNull Long nextId() {
    return nextGlobalId();
  }
}
