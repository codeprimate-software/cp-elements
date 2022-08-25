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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNullPointerException;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * An {@link InputStream} implementation that reads from a Java {@link ByteBuffer}.
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.nio.ByteBuffer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ByteBufferInputStream extends InputStream {

  /**
   * Factory method used to construct a new instance of {@link ByteBufferInputStream} backed by
   * the given, required {@link ByteBuffer}.
   *
   * @param byteBuffer {@link ByteBuffer} used to back the {@link InputStream}; must not be {@literal null}.
   * @return a new {@link ByteBufferInputStream} initialized with the given, required {@link ByteBuffer}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @see #ByteBufferInputStream(ByteBuffer)
   * @see java.nio.ByteBuffer
   */
  public static @NotNull ByteBufferInputStream from(@NotNull ByteBuffer byteBuffer) {
    return new ByteBufferInputStream(byteBuffer);
  }

  private final ByteBuffer byteBuffer;

  /**
   * Constructs a new instance of {@link ByteBufferInputStream} backed by the given, required {@link ByteBuffer}.
   *
   * @param byteBuffer {@link ByteBuffer} used to back the {@link InputStream}; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @see java.nio.ByteBuffer
   */
  public ByteBufferInputStream(@NotNull ByteBuffer byteBuffer) {
    this.byteBuffer = prepareForReading(ObjectUtils.requireObject(byteBuffer, "ByteBuffer is required"));
  }

  private @NotNull ByteBuffer prepareForReading(@NotNull ByteBuffer byteBuffer) {

    return (ByteBuffer) (byteBuffer.position() > 0
      ? byteBuffer.limit(byteBuffer.position()).rewind().mark()
      : byteBuffer.limit(byteBuffer.capacity())).mark();
  }

  /**
   * Gets the {@link ByteBuffer} used to back this {@link InputStream}.
   *
   * @return the {@link ByteBuffer} used to back this {@link InputStream}.
   * @see java.nio.ByteBuffer
   */
  protected @NotNull ByteBuffer getByteBuffer() {
    return this.byteBuffer;
  }

  /**
   * Determines the number of bytes still available to be read from the backing {@link ByteBuffer}
   * using this {@link InputStream}.
   *
   * @return the number of bytes still available to be read.
   * @see java.nio.ByteBuffer#remaining()
   */
  @Override
  public int available() {
    return getByteBuffer().remaining();
  }

  /**
   * Marks this {@link InputStream} to the current {@link ByteBuffer#position()}.
   *
   * @param readLimit ignored. This {@link InputStream} is backed by a {@link ByteBuffer} and therefore can recall
   * any number of bytes read before {@link #reset()} is called and the {@code readLimit} is reached.
   * @see java.nio.ByteBuffer#mark()
   */
  @Override
  public synchronized void mark(int readLimit) {
    getByteBuffer().mark();
  }

  /**
   * Return {@literal true} to indicate {@link #mark(int)} is supported.
   *
   * @return {@literal true}.
   */
  @Override
  public boolean markSupported() {
    return true;
  }

  /**
   * Read a single byte from the {@link ByteBuffer} using this {@link InputStream}.
   *
   * @return an {@link Integer} value containing the byte read from the backing {@link ByteBuffer}.
   * @throws IOException if an error occurs reading from the backing {@link ByteBuffer}.
   */
  @Override
  public int read() throws IOException {
    return available() > 0 ? getByteBuffer().get() : -1;
  }

  /**
   * Reads at most {@code length} bytes from the backing {@link ByteBuffer} using this {@link InputStream}
   * into the given, required byte array starting at the given offset.
   *
   * @param buffer byte array written from the bytes read from this {@link InputStream}; must not be {@literal null}.
   * @param offset {@link Integer} referring to the index in the byte array to start writing bytes.
   * @param length {@link Integer} specifying the number of bytes to write into the byte array
   * from this {@link InputStream}.
   * @return an {@link Integer} indicating the number of bytes read from this {@link InputStream} into the byte array.
   * Returns {@literal 0} if the length of the byte array is {@literal 0}. Returns {@literal -1} if there are no
   * {@link #available()} bytes in this {@link InputStream}.
   * @throws NullPointerException if the byte array is {@literal null}.
   * @throws IndexOutOfBoundsException if {@code offset} is less than {@literal 0} or greater than equal to
   * the byte array length, or the length is less than {@literal 0} or greater than
   * the byte array length - {@code offset}.
   */
  @Override
  public int read(@NotNull byte[] buffer, int offset, int length) throws IOException {

    Assert.notNull(buffer, newNullPointerException("Byte array cannot be null"));

    if (buffer.length > 0) {

      assertThat(offset)
        .throwing(newIndexOutOfBoundsException("Offset [%d] must be greater than equal to 0 and less than [%d]",
          offset, buffer.length))
        .isGreaterThanEqualToAndLessThan(0, buffer.length);

      assertThat(length)
        .throwing(newIndexOutOfBoundsException("Length [%d] must be greater than equal to 0 and less than equal to [%d]",
          length, buffer.length - offset))
        .isGreaterThanEqualToAndLessThanEqualTo(0, buffer.length - offset);

      int numberOfBytesToRead = Math.min(available(), length);

      if (numberOfBytesToRead > 0) {
        getByteBuffer().get(buffer, offset, numberOfBytesToRead);
        return numberOfBytesToRead;
      }

      return -1;
    }

    return 0;
  }

  /**
   * Resets the underlying, backing {@link ByteBuffer} to the last {@link ByteBuffer#mark() marked position}.
   *
   * @see java.nio.ByteBuffer#reset()
   */
  @Override
  public synchronized void reset() {
    getByteBuffer().reset();
  }

  /**
   * Skips the given number of bytes read from this {@link InputStream}.
   *
   * Effectively moves the {@link ByteBuffer#position()} ahead by the number of available or given number of bytes,
   * which ever is smaller.
   *
   * @param numberOfBytes {@link Long} specifying the number of bytes to skip in this {@link InputStream}.
   * @return an {@link Integer} indicating the actual number of bytes skipped.
   * @see java.nio.ByteBuffer#position(int)
   * @see #available()
   */
  @Override
  public long skip(long numberOfBytes) {

    if (numberOfBytes > 0) {
      int actualNumberOfBytes = (int) Math.min(available(), numberOfBytes);
      ByteBuffer byteBuffer = getByteBuffer();
      byteBuffer.position(byteBuffer.position() + actualNumberOfBytes);
      return actualNumberOfBytes;
    }

    return 0;
  }
}
