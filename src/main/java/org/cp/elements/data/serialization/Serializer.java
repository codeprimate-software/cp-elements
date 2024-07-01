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
package org.cp.elements.data.serialization;

import java.io.Serializable;
import java.nio.ByteBuffer;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.nio.BufferUtils;

/**
 * Interface defining a contract for serializing and deserializing {@link Object Objects}
 * to and from a sequence of bytes.
 *
 * @author John Blum
 * @see java.io.Serializable
 * @see java.nio.ByteBuffer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Serializer {

  /**
   * Determines whether the given {@link Object} is {@link #serialize(Object) serializable}, or rather, whether
   * the given {@link Object} can be serialized into a sequence of bytes by this {@link Serializer}.
   * <p>
   * The default implementation simply checks whether the given {@link Object} is an instance of {@link Serializable}.
   * However, an {@link Object} does not necessarily need to be {@link Serializable} to be
   * {@link #serialize(Object) serialized} by a specific {@link Serializer} implementation; the ability to serialize
   * a {@link Object value} is entirely dependent on the {@link Serializer} implementation.
   * <p>
   * It is assumed that if a {@link Serializer} can {@link #serialize(Object)} a given {@link Object} that it should
   * also subsequently be capable of {@link #deserialize(ByteBuffer)  deserializing} a byte sequence from
   * the given {@link Object}.
   *
   * @param target {@link Object} to evaluate whether it can be serialized by this {@link Serializer}.
   * @return a boolean value indicating whether the given {@link Object} can be {@link #serialize(Object) serialized}
   * by this {@link Serializer}.
   * @see java.io.Serializable
   */
  @NullSafe
  default boolean isSerializable(@Nullable Object target) {
    return target instanceof Serializable;
  }

  /**
   * Serializes the given {@link Object} directly into a byte array.
   *
   * @param target {@link Object} to serialize.
   * @return an array of bytes from serializing the given, target {@link Object}.
   * @throws IllegalArgumentException if the {@link Object} to serialize is {@literal null}.
   * @see java.nio.ByteBuffer#array()
   * @see #serialize(Object)
   */
  default @NotNull byte[] toByteArray(@NotNull Object target) {

    Assert.notNull(target, "Object to serialize is required");

    ByteBuffer buffer = serialize(target);

    return buffer.isReadOnly()
      ? BufferUtils.getByteArray(buffer)
      : buffer.array();
  }

  /**
   * Serializes the given, target {@link Object} into a sequence of bytes encapsulated by
   * the returned {@link ByteBuffer}.
   *
   * @param target {@link Object} to serialize.
   * @return an instance of {@link ByteBuffer} encapsulating the sequence of bytes
   * from serializing the target {@link Object}.
   * @see java.nio.ByteBuffer
   */
  ByteBuffer serialize(Object target);

  /**
   * Deserializes the given sequence of bytes encapsulated by the given {@link ByteBuffer} into an {@link Object}
   * of the declared {@link T type}.
   *
   * @param <T> {@link Class type} of {@link Object} reconstructed from the sequence of bytes.
   * @param bytes {@link ByteBuffer} encapsulating the sequence of bytes from
   * the {@link #serialize(Object) serialized} {@link Object}.
   * @return the deserialized {@link Object} of the given {@link T type}.
   * @see java.nio.ByteBuffer
   */
  <T> T deserialize(ByteBuffer bytes);

}
