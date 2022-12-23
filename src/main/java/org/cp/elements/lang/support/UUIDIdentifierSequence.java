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

import java.util.UUID;

import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Implementation of the {@link IdentifierSequence} interface generating unique, {@link UUID} identifiers in sequence
 * using the {@link java.util.UUID} class.
 *
 * @author John J. Blum
 * @see java.util.UUID
 * @see org.cp.elements.lang.IdentifierSequence
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class UUIDIdentifierSequence implements IdentifierSequence<UUID> {

  /**
   * Generates the next unique ID in sequence.
   *
   * @return the next unique String ID in the sequence.
   * @see java.util.UUID#randomUUID()
   */
  @Override
  public @NotNull UUID nextId() {
    return UUID.randomUUID();
  }
}
