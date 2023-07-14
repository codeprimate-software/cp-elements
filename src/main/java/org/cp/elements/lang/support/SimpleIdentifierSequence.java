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

import java.util.concurrent.atomic.AtomicLong;

import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link IdentifierSequence} interface generating unique, {@link Long} typed identifiers.
 * <p>
 * This implementation uses the Thread-safe {@link AtomicLong} class, making this class Thread-safe.
 * <p>
 * While this maybe useful for testing purposes, it should not be used in distributed systems where multiple application
 * or system components may generate ID's using different instances of this class in separate JVMs.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see java.util.concurrent.atomic.AtomicLong
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@ThreadSafe
public class SimpleIdentifierSequence implements IdentifierSequence<Long> {

  private static final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

  /**
   * Generates the next unique ID in sequence.
   *
   * @return the next unique Long ID in the sequence.
   * @see java.util.concurrent.atomic.AtomicLong#incrementAndGet()
   */
  @Override
  public Long nextId() {
    return ID_SEQUENCE.incrementAndGet();
  }
}
