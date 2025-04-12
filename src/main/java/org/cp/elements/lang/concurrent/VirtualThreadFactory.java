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
package org.cp.elements.lang.concurrent;

import java.util.UUID;
import java.util.concurrent.ThreadFactory;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Overload;
import org.cp.elements.lang.support.UUIDIdentifierSequence;

/**
 * The VirtualThreadFactory class...
 *
 * @author John Blum
 * @see java.util.concurrent.ThreadFactory
 * @since 3.0.0
 */
@SuppressWarnings("unused")
public class VirtualThreadFactory implements ThreadFactory {

  protected static final String THREAD_NAME_FORMAT = "%1$s.THREAD-%2$s";

  /**
   * Factory method used to construct a new {@link VirtualThreadFactory}.
   *
   * @return a new {@link VirtualThreadFactory}
   */
  public static VirtualThreadFactory newThreadFactory() {
    return new VirtualThreadFactory();
  }

  private final IdentifierSequence<UUID> threadIdGenerator = new UUIDIdentifierSequence();

  /**
   * Generates a {@link String unique identifier (ID)} for the new {@link Thread}.
   *
   * @return a {@link String} containing a {@literal unique identifier (ID)} for the new {@link Thread}.
   * @see #generateThreadName()
   * @see IdentifierSequence
   */
  protected @NotNull String generateThreadId() {
    return this.threadIdGenerator.nextId().toString();
  }

  /**
   * Generates a {@link String unique name} for the new {@link Thread}.
   *
   * @return a {@link String} containing a {@literal unique name} for the new {@link Thread}.
   * @see #generateThreadId()
   * @see Thread#getName()
   */
  protected @NotNull String generateThreadName() {
    return THREAD_NAME_FORMAT.formatted(VirtualThreadFactory.class.getName(), generateThreadId());
  }

  @Override
  @SuppressWarnings("all")
  public Thread newThread(@NotNull Runnable runnable) {

    Assert.notNull(runnable, "Runnable is required");

    return Thread.ofVirtual()
      .name(generateThreadName())
      .unstarted(runnable);
  }

  /**
   * Constructs a new {@literal Virtual} {@link Thread} with the given {@link String name}
   * and {@link Runnable} to execute.
   *
   * @param name {@link String} containing the {@literal name} of the new {@literal Virtual} {@link Thread}; required.
   * @param runnable {@link Runnable} to execute; required.
   * @return a new {@literal Virtual} {@link Thread}.
   * @throws IllegalArgumentException if {@link String name} is {@literal null} or {@literal empty},
   * or the {@link Runnable} object is {@literal null}.
   * @see Runnable
   * @see Thread
   */
  @Overload
  public Thread newThread(@NotNull String name, @NotNull Runnable runnable) {

    Assert.hasText(name, "Name [%s] is required", name);
    Assert.notNull(runnable, "Runnable is required");

    return Thread.ofVirtual().name(name).unstarted(runnable);
  }
}
