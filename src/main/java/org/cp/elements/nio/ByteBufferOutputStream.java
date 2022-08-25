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

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * An {@link OutputStream} implementation that writes to a Java {@link ByteBuffer}.
 *
 * @author John Blum
 * @see java.io.OutputStream
 * @see java.nio.ByteBuffer
 * @since 1.0.0
 */
public class ByteBufferOutputStream extends OutputStream {

  protected static final int CAPACITY_INCREMENT = BufferUtils.EIGHT_KILOBYTE_BUFFER_SIZE;

  protected static final float LOAD_FACTOR = 0.85f;

  /**
   * Factory method used to construct a new instance of {@link ByteBufferOutputStream} to write data into the given,
   * required {@link ByteBuffer}.
   *
   * @param byteBuffer {@link ByteBuffer} into which this {@link OutputStream} will write data;
   * must not be {@literal null} and must be {@link ByteBuffer#isReadOnly() writable}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @throws IllegalStateException if the {@link ByteBuffer} is {@link ByteBuffer#isReadOnly() read-only}.
   * @see #ByteBufferOutputStream(ByteBuffer)
   * @see java.nio.ByteBuffer
   */
  public static @NotNull ByteBufferOutputStream into(@NotNull ByteBuffer byteBuffer) {
    return new ByteBufferOutputStream(byteBuffer);
  }

  private volatile boolean closed = false;

  private ByteBuffer byteBuffer;

  /**
   * Constructs a new instance of {@link ByteBufferOutputStream} initialized with
   * the given, required {@link ByteBuffer}.
   *
   * @param byteBuffer {@link ByteBuffer} into which this {@link OutputStream} will write data;
   * must not be {@literal null} and must be {@link ByteBuffer#isReadOnly() writable}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @throws IllegalStateException if the {@link ByteBuffer} is {@link ByteBuffer#isReadOnly() read-only}.
   * @see java.nio.ByteBuffer
   */
  public ByteBufferOutputStream(@NotNull ByteBuffer byteBuffer) {
    setByteBuffer(byteBuffer);
  }

  /**
   * Configures the {@link ByteBuffer} used by this {@link OutputStream} to write data into.
   *
   * @param byteBuffer {@link ByteBuffer} into which this {@link OutputStream} will write data;
   * must not be {@literal null} and must be {@link ByteBuffer#isReadOnly() writable}.
   * @throws IllegalArgumentException if the {@link ByteBuffer} is {@literal null}.
   * @throws IllegalStateException if the {@link ByteBuffer} is {@link ByteBuffer#isReadOnly() read-only}.
   * @see java.nio.ByteBuffer
   */
  private void setByteBuffer(@NotNull ByteBuffer byteBuffer) {

    Assert.notNull(byteBuffer, "ByteBuffer is required");
    Assert.state(!byteBuffer.isReadOnly(), "ByteBuffer must not be read-only");

    this.byteBuffer = byteBuffer;
  }

  /**
   * Closes the {@link OutputStream} and returns the backing {@link ByteBuffer} into which this {@link OutputStream}
   * wrote data.
   *
   * @return the backing {@link ByteBuffer}.
   * @see java.nio.ByteBuffer
   * @see #close()
   */
  public @NotNull ByteBuffer getByteBuffer() {
    close();
    return this.byteBuffer;
  }

  /**
   * Determines whether this {@link OutputStream} has been closed.
   *
   * @return a boolean value indicating whether this {@link OutputStream} has been closed.
   * @see #isOpen()
   */
  public boolean isClosed() {
    return this.closed;
  }

  /**
   * Determines whether this {@link OutputStream} has not been closed yet.
   *
   * @return a boolean value indicating whether this {@link OutputStream} has not been closed yet.
   * @see #isClosed()
   */
  public boolean isOpen() {
    return !isClosed();
  }

  /**
   * Asserts that this {@link OutputStream} has not been closed yet.
   *
   * @throws IllegalStateException if this {@link OutputStream} has been closed.
   * @see #isOpen()
   * @see #isClosed()
   */
  protected void assertNotClosed() {
    Assert.state(isOpen(), "The ByteBufferOutputStream was closed");
  }

  /**
   * Closes this {@link OutputStream} preventing any further I/O operations (e.g. {@literal writes})
   * from occurring on the backing {@link ByteBuffer}.
   */
  @Override
  public void close() {
    this.closed = true;
  }

  protected byte convertToByte(int value) {
    //return Integer.valueOf(value).byteValue();
    return (byte) (value & 0x000000FF);
  }

  protected void reallocateBuffer() {

    assertNotClosed();

    ByteBuffer byteBuffer = this.byteBuffer;

    if (BufferUtils.computeLoadFactor(byteBuffer) > LOAD_FACTOR) {
      setByteBuffer(BufferUtils.copy(byteBuffer, CAPACITY_INCREMENT));
    }
  }

  @Override
  public void write(int byteToWrite) throws IOException {

    assertNotClosed();
    reallocateBuffer();
    this.byteBuffer.put(convertToByte(byteToWrite));
  }
}
