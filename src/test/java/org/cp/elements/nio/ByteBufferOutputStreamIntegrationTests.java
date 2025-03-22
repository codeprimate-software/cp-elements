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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayUtils;

/**
 * Integration Tests for {@link ByteBufferOutputStream}.
 *
 * @author John Blum
 * @see java.io.OutputStream
 * @see java.nio.ByteBuffer
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.nio.ByteBufferOutputStream
 * @since 1.0.0
 */
public class ByteBufferOutputStreamIntegrationTests {

  private static final int CAPACITY = 1;

  private @NotNull ByteBuffer allocateHeapByteBuffer() {

    ByteBuffer byteBuffer = ByteBuffer.allocate(CAPACITY);

    assertThat(byteBuffer).isNotNull();
    assertThat(byteBuffer.capacity()).isEqualTo(CAPACITY);
    assertThat(byteBuffer.limit()).isEqualTo(CAPACITY);
    assertThat(byteBuffer.position()).isZero();
    assertThat(byteBuffer.remaining()).isEqualTo(CAPACITY);

    return byteBuffer;
  }

  private @NotNull ByteBuffer mockByteBuffer() {
    return spy(allocateHeapByteBuffer());
  }

  @Test
  public void constructNewByteBufferOutputStreamWithNonNullByteBuffer() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    try (ByteBufferOutputStream outputStream = new ByteBufferOutputStream(mockByteBuffer)) {
      assertThat(outputStream.getByteBuffer()).isEqualTo(mockByteBuffer);
    }

    verify(mockByteBuffer, times(1)).isReadOnly();
  }

  @Test
  @SuppressWarnings("all")
  public void constructNewByteBufferOutputStreamWithNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ByteBufferOutputStream(null))
      .withMessage("ByteBuffer is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void constructNewByteBufferOutputStreamWithReadOnlyByteBuffer() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(true).when(mockByteBuffer).isReadOnly();

    assertThatIllegalStateException()
      .isThrownBy(() -> new ByteBufferOutputStream(mockByteBuffer))
      .withMessage("ByteBuffer must not be read-only")
      .withNoCause();

    verify(mockByteBuffer, times(1)).isReadOnly();
    verifyNoMoreInteractions(mockByteBuffer);
  }

  @Test
  public void intoIsCorrect() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(mockByteBuffer);

    assertThat(outputStream).isNotNull();
    assertThat(outputStream.getByteBuffer()).isEqualTo(mockByteBuffer);

    verify(mockByteBuffer, times(1)).isReadOnly();
  }

  @Test
  public void closeClosedTheOutputStream() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(mockByteBuffer);

    assertThat(outputStream).isNotNull();
    assertThat(outputStream.isClosed()).isFalse();
    assertThat(outputStream.isOpen()).isTrue();

    outputStream.close();

    assertThat(outputStream.isClosed()).isTrue();
    assertThat(outputStream.isOpen()).isFalse();

    verify(mockByteBuffer, times(1)).isReadOnly();
    verifyNoMoreInteractions(mockByteBuffer);
  }

  @Test
  public void closePreventsOutputStreamWriteOperations() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(mockByteBuffer);

    assertThat(outputStream).isNotNull();

    outputStream.close();

    assertThat(outputStream.isClosed()).isTrue();
    assertThat(outputStream.isOpen()).isFalse();

    assertThatIllegalStateException()
      .isThrownBy(() -> outputStream.write(0x0A))
      .withMessage("The ByteBufferOutputStream was closed")
      .withNoCause();

    verify(mockByteBuffer, times(1)).isReadOnly();
    verifyNoMoreInteractions(mockByteBuffer);
  }

  @Test
  public void gettingByteBufferClosesOutputStream() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(mockByteBuffer);

    assertThat(outputStream).isNotNull();
    assertThat(outputStream.isClosed()).isFalse();
    assertThat(outputStream.isOpen()).isTrue();
    assertThat(outputStream.getByteBuffer()).isEqualTo(mockByteBuffer);
    assertThat(outputStream.isClosed()).isTrue();
    assertThat(outputStream.isOpen()).isFalse();

    verify(mockByteBuffer, times(1)).isReadOnly();
  }

  @Test
  public void convertToByteIsCorrect() {

    ByteBuffer mockByteBuffer = mockByteBuffer();

    doReturn(false).when(mockByteBuffer).isReadOnly();

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(mockByteBuffer);

    assertThat(outputStream).isNotNull();
    assertThat(outputStream.convertToByte(0x00)).isEqualTo(Integer.valueOf(0x00).byteValue());
    assertThat(outputStream.convertToByte(0x01)).isEqualTo(Integer.valueOf(0x01).byteValue());
    assertThat(outputStream.convertToByte(0x08)).isEqualTo(Integer.valueOf(0x08).byteValue());
    assertThat(outputStream.convertToByte(0xFF)).isEqualTo(Integer.valueOf(0xFF).byteValue());
    assertThat(outputStream.convertToByte(0xB00BFAB)).isEqualTo(Integer.valueOf(0xAB).byteValue());
    assertThat(outputStream.convertToByte(0xB00BCAFE)).isEqualTo(Integer.valueOf(0xFE).byteValue());
  }

  @Test
  public void writeReallocatesByteBuffer() throws IOException {

    byte[] array = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F };

    ByteBuffer byteBuffer = ByteBuffer.allocate(2);

    assertThat(byteBuffer).isNotNull();
    assertThat(byteBuffer.isDirect()).isFalse();
    assertThat(byteBuffer.isReadOnly()).isFalse();
    assertThat(byteBuffer.capacity()).isEqualTo(2);

    byteBuffer.put((byte) 0x21);
    byteBuffer.put((byte) 0x42);

    ByteBufferOutputStream outputStream = ByteBufferOutputStream.into(byteBuffer);

    assertThat(outputStream).isNotNull();
    assertThat(outputStream.isOpen()).isTrue();

    outputStream.write(array);
    outputStream.flush();

    ByteBuffer buffer = outputStream.getByteBuffer();

    assertThat(buffer).isNotNull();
    assertThat(buffer).isNotSameAs(byteBuffer);
    assertThat(buffer.isDirect()).isFalse();
    assertThat(buffer.isReadOnly()).isFalse();
    assertThat(buffer.capacity()).isGreaterThanOrEqualTo(array.length);
    assertThat(outputStream.isClosed()).isTrue();

    buffer.limit(buffer.position()).rewind();

    byte[] copy = buffer.array();

    assertThat(ArrayUtils.subArray(BufferUtils.toBigByteArray(copy), 0, 2))
      .containsExactly(BufferUtils.toBigByteArray(new byte[] { 0x21, 0x42 }));
    assertThat(ArrayUtils.subArray(BufferUtils.toBigByteArray(copy), 2, 15))
      .containsExactly(BufferUtils.toBigByteArray(array));
  }
}
