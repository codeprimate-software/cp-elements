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
package org.cp.elements.beans.event;

import java.time.Instant;
import java.util.EventObject;

import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link EventObject} implementation used as the record for the change in state to the source {@link Object}.
 *
 * @author John J. Blum
 * @see java.time.Instant
 * @see java.util.EventObject
 * @since 1.0.0
 */
public class ChangeEvent extends EventObject {

  private final Instant changeDateTime = Instant.now();

  /**
   * Constructs a new instance of {@link ChangeEvent} initialized with the given, required {@link Object} used as
   * the source for change events.
   *
   * @param source {@link Object} reference used as the source of the change events; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Object source} is {@literal null}.
   */
  public ChangeEvent(@NotNull Object source) {
    super(source);
  }

  /**
   * Gets the {@link Instant date and time} when the change to the source {@link Object} occurred.
   *
   * @return a {@link Instant} indicating the {@literal date and time} of {@literal this} change event,
   * i.e. when the source {@link Object} was changed.
   * @see java.time.Instant
   */
  public @NotNull Instant getChangeDateTime() {
    return Instant.from(this.changeDateTime);
  }
}
