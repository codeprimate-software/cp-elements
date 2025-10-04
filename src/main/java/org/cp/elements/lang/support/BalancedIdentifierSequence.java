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
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link IdentifierSequence} implementation using {@link Long} values.
 *
 * @author John Blum
 * @see IdentifierSequence
 * @see ThreadSafe
 * @since 3.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class BalancedIdentifierSequence implements IdentifierSequence<Long> {

  public static final long STARTING_VALUE = Long.MAX_VALUE / 2L;

  private boolean flip = false;

  private long decrementSequence = STARTING_VALUE - 1;
  private long incrementSequence = STARTING_VALUE;

  @Override
  public synchronized Long nextId() {
    long nextId = this.flip ? this.decrementSequence-- : this.incrementSequence++;
    this.flip = !this.flip;
    return nextId;
  }
}
