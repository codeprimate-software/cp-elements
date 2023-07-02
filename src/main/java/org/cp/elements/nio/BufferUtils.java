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
package org.cp.elements.nio;

import java.nio.Buffer;
import java.nio.ByteBuffer;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class for processing {@link Buffer Buffers}.
 *
 * @author John Blum
 * @see java.nio.Buffer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class BufferUtils {

  public static final int ONE_KILOBYTE_BUFFER_SIZE = 1024;
  public static final int TWO_KILOBYTE_BUFFER_SIZE = 2048;
  public static final int FOUR_KILOBYTE_BUFFER_SIZE = 4096;
  public static final int EIGHT_KILOBYTE_BUFFER_SIZE = 8192;
  public static final int SIXTEEN_KILOBYTE_BUFFER_SIZE = 8192;
  public static final int THIRTY_TWO_KILOBYTE_BUFFER_SIZE = 32768;
  public static final int SIXTY_FOUR_KILOBYTE_BUFFER_SIZE = 64536;

  public static final byte[] EMPTY_BYTE_ARRAY = new byte[0];

  /**
   * Computes the {@literal load factor} of the given, required {@link Buffer}.
   * <p>
   * The {@literal load factor} is a function of the {@link Buffer Buffer's} {@link Buffer#position()}
   * and {@link Buffer#capacity()}, irrespective of the {@link Buffer Buffer's} {@link Buffer#limit()},
   * if set.
   * <p>
   * Just because a {@link Buffer} has reached its {@link Buffer#limit()} does not mean it is at
   * or nearing {@link Buffer#capacity()}. This computation is often used in the determination
   * for whether to {@link #copy(ByteBuffer, int)} reallocate the {@link Buffer}.
   *
   * @param buffer {@link Buffer} used to compute the {@literal load factor}; must not be {@literal null}.
   * @return the computed {@literal load factor} of the given {@link Buffer}.
   * @throws IllegalArgumentException if the {@link Buffer} is {@literal null}.
   * @see java.nio.Buffer
   */
  public static float computeLoadFactor(@NotNull Buffer buffer) {

    Assert.notNull(buffer, "A Buffer is required to compute load factor");

    int bufferPosition = buffer.position();

    return bufferPosition > 0
      ? (float) bufferPosition / (float) buffer.capacity()
      : 0.0f;
  }

  /**
   * Copies the given, required {@link ByteBuffer} into a new {@link ByteBuffer} with
   * additional {@link ByteBuffer#capacity()}.
   * <p>
   * The {@link ByteBuffer#position()} will be set to immediately after the last byte copied from
   * the given {@link ByteBuffer}, which corresponds to either the given {@link ByteBuffer ByteBuffer's}
   * {@link ByteBuffer#limit()} or its {@link ByteBuffer#capacity()}, dependent on the existing data
   * in the {@link ByteBuffer}.
   * <p>
   * The {@link ByteBuffer#limit()} of the new {@link ByteBuffer} (copy) will match its {@link ByteBuffer#capacity()}.
   * <p>
   * The {@link ByteBuffer#capacity()} of the new {@link ByteBuffer} (copy) will be the {@link ByteBuffer#capacity()}
   * of the given {@link ByteBuffer} plus the {@code additionalCapacity}.
   *
   * @param buffer {@link ByteBuffer} to copy; must not be {@literal null}.
   * @param additionalCapacity {@link Integer} specifying the capacity to add to the new {@link ByteBuffer};
   * must be greater than equal to {@literal 0}.
   * @return a new {@link ByteBuffer} with additional {@link ByteBuffer#capacity()} and the contents from the existing,
   * given {@link ByteBuffer}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null} or {@code additionalCapacity}
   * is less than {@literal 0}.
   * @throws IllegalStateException if the given {@link ByteBuffer} to copy has no {@link ByteBuffer#capacity()}
   * or could not be {@link ByteBuffer#rewind() rewound} for the {@code copy} operation.
   * @see java.nio.ByteBuffer
   */
  public static @NotNull ByteBuffer copy(@NotNull ByteBuffer buffer, int additionalCapacity) {

    Assert.notNull(buffer, "ByteBuffer to copy is required");

    Assert.state(buffer.capacity() > 0, "ByteBuffer to copy has no capacity");

    Assert.isTrue(additionalCapacity > -1,
      "Additional capacity [%d] must be greater than equal to 0", additionalCapacity);

    ByteBuffer bufferToCopy = prepareByteBufferToCopy(buffer);

    Assert.state(bufferToCopy.position() == 0, "Failed to rewind the given ByteBuffer");
    Assert.state(bufferToCopy.remaining() > 0, "The given ByteBuffer has no remaining content to copy");

    return bufferToCopy.isDirect()
      ? allocateDirect(bufferToCopy, additionalCapacity)
      : allocateNonDirect(bufferToCopy, additionalCapacity);
  }

  private static @NotNull ByteBuffer allocateDirect(@NotNull ByteBuffer existingBuffer, int additionalCapacity) {
    return ByteBuffer.allocateDirect(existingBuffer.capacity() + additionalCapacity).put(existingBuffer);
  }

  private static @NotNull ByteBuffer allocateNonDirect(@NotNull ByteBuffer existingBuffer, int additionalCapacity) {
    return ByteBuffer.allocate(existingBuffer.capacity() + additionalCapacity).put(existingBuffer);
  }

  private static @NotNull ByteBuffer prepareByteBufferToCopy(@NotNull ByteBuffer buffer) {

    return (ByteBuffer) (buffer.position() > 0
      ? buffer.limit(buffer.position()).rewind()
      : buffer.limit(buffer.capacity()));
  }

  /**
   * Gets an array of bytes containing the contents of the given, required {@link ByteBuffer}.
   * <p>
   * This method handles {@link ByteBuffer#isReadOnly() read-only} {@link ByteBuffer ByteBuffers}.
   *
   * @param buffer {@link ByteBuffer} from which to extract an array of bytes
   * containing the entire contents of the buffer.
   * @return an array of bytes containing the contents of the given, required {@link ByteBuffer}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @see java.nio.ByteBuffer
   */
  public static byte[] getByteArray(@NotNull ByteBuffer buffer) {
    Assert.notNull(buffer, "ByteBuffer is required");
    byte[] array = new byte[buffer.capacity()];
    buffer.get(array);
    return array;
  }

  /**
   * Converts the primitive byte array into a big, wrapper {@link Byte} array.
   *
   * @param array primitive byte array to convert.
   * @return a big {@link Byte} array containing the elements of the primitive byte array.
   * @see #toPrimitiveByteArray(Byte[])
   */
  @NullSafe
  public static Byte[] toBigByteArray(byte[] array) {

    byte[] nullSafeArray = nullSafeArray(array);

    Byte[] bigByteArray = new Byte[nullSafeArray.length];

    int index = 0;

    for (byte element : nullSafeArray) {
      bigByteArray[index++] = element;
    }

    return bigByteArray;
  }

  @NullSafe
  private static byte[] nullSafeArray(byte[] array) {
    return array != null ? array : EMPTY_BYTE_ARRAY;
  }

  /**
   * Converts the big, wrapper {@link Byte} array into a primitive byte array.
   *
   * @param array big, wrapper {@link Byte} array to convert.
   * @return a primitive byte array containing the element of the big {@link Byte} array.
   * @see #toBigByteArray(byte[])
   */
  @NullSafe
  public static byte[] toPrimitiveByteArray(Byte[] array) {

    Byte[] nullSafeArray = ArrayUtils.nullSafeArray(array, Byte.class);

    byte[] primitiveByteArray = new byte[nullSafeArray.length];
    int index = 0;

    for (Byte element : nullSafeArray) {
      primitiveByteArray[index++] = NumberUtils.byteValue(element);
    }

    return primitiveByteArray;
  }
}
