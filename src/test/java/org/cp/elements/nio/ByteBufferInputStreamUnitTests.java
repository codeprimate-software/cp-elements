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
import static org.assertj.core.api.Assertions.assertThatNullPointerException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.junit.jupiter.api.Test;

import org.cp.elements.function.FunctionException;
import org.cp.elements.function.ThrowableFunction;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.TestException;
import org.mockito.InOrder;

/**
 * Unit Tests for {@link ByteBufferInputStream}.
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.nio.ByteBuffer
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.nio.ByteBufferInputStream
 * @since 1.0.0
 */
public class ByteBufferInputStreamUnitTests {

  private static final int CAPACITY = 1;

  private @NotNull ByteBuffer allocateHeapByteBuffer(int capacity) {

    ByteBuffer buffer = ByteBuffer.allocate(capacity);

    assertThat(buffer).isNotNull();
    assertThat(buffer.capacity()).isEqualTo(capacity);
    assertThat(buffer.limit()).isEqualTo(capacity);
    assertThat(buffer.position()).isZero();
    assertThat(buffer.remaining()).isEqualTo(capacity);

    return spy(buffer);
  }

  private @NotNull ByteBuffer emptyByteBuffer() {
    return allocateHeapByteBuffer(0);
  }

  // NOTE: ByteBuffer from Java 19+ is a Sealed Class and cannot be mocked.
  private @NotNull ByteBuffer singleByteByteBuffer() {
    return allocateHeapByteBuffer(1);
  }

  private @NotNull ByteBuffer sizedByteBuffer(int capacity) {
    return allocateHeapByteBuffer(capacity);
  }

  private <T> T tryWithResource(@NotNull ByteBuffer buffer,
      @NotNull ThrowableFunction<ByteBufferInputStream, T> operation) throws Throwable {

    try (ByteBufferInputStream in = ByteBufferInputStream.from(buffer)) {
      return operation.apply(in);
    }
    catch (FunctionException cause) {
      throw cause.getCause();
    }
    catch (IOException cause) {
      throw new TestException("Failed to close the ByteBufferInputStream", cause);
    }
  }

  @SuppressWarnings("all")
  private void verifyPositionedByteBufferInteractions(@NotNull ByteBuffer mockByteBuffer, int expectedLimit) {

    InOrder order = inOrder(mockByteBuffer);

    order.verify(mockByteBuffer, times(2)).position();
    order.verify(mockByteBuffer, times(1)).limit(eq(expectedLimit));
    order.verify(mockByteBuffer, times(1)).rewind();
    order.verify(mockByteBuffer, times(2)).mark();

    verifyNoMoreInteractions(mockByteBuffer);
  }

  @SuppressWarnings("all")
  private void verifyZeroPositionedByteBufferInteractions(@NotNull ByteBuffer mockByteBuffer) {
    verifyZeroPositionedByteBufferInteractions(mockByteBuffer, CAPACITY);
  }

  @SuppressWarnings("all")
  private void verifyZeroPositionedByteBufferInteractions(@NotNull ByteBuffer mockByteBuffer, int expectedLimit) {

    InOrder order = inOrder(mockByteBuffer);

    order.verify(mockByteBuffer, times(1)).position();
    order.verify(mockByteBuffer, times(1)).capacity();
    order.verify(mockByteBuffer, times(1)).limit(eq(expectedLimit));
    order.verify(mockByteBuffer, times(1)).mark();

    verifyNoMoreInteractions(mockByteBuffer);
  }

  @Test
  public void constructNewByteBufferInputStreamWithNonNullByteBuffer() throws IOException {

    ByteBuffer byteBuffer = singleByteByteBuffer();

    try (ByteBufferInputStream inputStream = new ByteBufferInputStream(byteBuffer)) {
      assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    }

    verifyZeroPositionedByteBufferInteractions(byteBuffer);
  }

  @Test
  public void constructNewByteBufferInputStreamWithNonPositionedByteBuffer() throws IOException {

    ByteBuffer byteBuffer = sizedByteBuffer(16);

    try (ByteBufferInputStream inputStream = new ByteBufferInputStream(byteBuffer)) {
      assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    }

    verifyZeroPositionedByteBufferInteractions(byteBuffer, 16);
  }

  @Test
  @SuppressWarnings("all")
  public void constructNewByteBufferInputStreamWithNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ByteBufferInputStream(null))
      .withMessage("ByteBuffer is required")
      .withNoCause();
  }

  @Test
  public void constructNewByteBufferInputStreamWithPositionedByteBuffer() throws IOException {

    ByteBuffer mockByteBuffer = sizedByteBuffer(5);

    doReturn(5).when(mockByteBuffer).position();

    try (ByteBufferInputStream inputStream = new ByteBufferInputStream(mockByteBuffer)) {
      assertThat(inputStream.getByteBuffer()).isEqualTo(mockByteBuffer);
    }

    verifyPositionedByteBufferInteractions(mockByteBuffer, 5);
  }

  @Test
  public void fromByteBufferIsSuccessful() {

    ByteBuffer mockByteBuffer = emptyByteBuffer();

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(mockByteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(mockByteBuffer);

    verifyZeroPositionedByteBufferInteractions(mockByteBuffer, 0);
  }

  @Test
  public void availableIsCorrect() throws IOException {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    assertThat(inputStream.available()).isEqualTo(array.length);

    int count = 0;

    while (inputStream.read() != -1) {
      assertThat(inputStream.available()).isEqualTo(array.length - ++count);
    }

    assertThat(count).isEqualTo(array.length);
    assertThat(inputStream.available()).isZero();
  }

  @Test
  public void markIsCorrect() throws IOException {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    assertThat(inputStream.available()).isEqualTo(array.length);

    int count = 0;

    for (byte data : array) {
      assertThat(inputStream.read()).isEqualTo(data);
      if (++count == 3) {
        inputStream.mark(0);
      }
    }

    assertThat(count).isEqualTo(array.length);
    assertThat(inputStream.read()).isEqualTo(-1);
    assertThat(inputStream.available()).isZero();

    inputStream.reset();

    assertThat(inputStream.available()).isEqualTo(6);

    count = 0;

    for (int index = 3; index < array.length; index++) {
      assertThat(inputStream.read()).isEqualTo(array[index]);
      if (++count == 3) {
        inputStream.mark(10);
      }
    }

    assertThat(count).isEqualTo(array.length - 3);
    assertThat(inputStream.read()).isEqualTo(-1);
    assertThat(inputStream.available()).isZero();

    inputStream.reset();

    assertThat(inputStream.available()).isEqualTo(3);
    assertThat(inputStream.read()).isEqualTo(7);
  }

  @Test
  public void markSupportedReturnsTrue() throws Throwable {
    assertThat(tryWithResource(singleByteByteBuffer(), ByteBufferInputStream::markSupported)).isTrue();
  }

  @Test
  public void readsAllBytesFromByteBuffer() throws IOException {

    String data = "This is data for the ByteBufferInputStream.read() test case!";

    ByteBuffer byteBuffer = ByteBuffer.wrap(data.getBytes());

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);

    byte[] buffer = new byte[byteBuffer.capacity()];

    for (int index = 0, byteValue = inputStream.read(); byteValue != -1; index++, byteValue = inputStream.read()) {
      buffer[index] = (byte) byteValue;
    }

    assertThat(new String(buffer)).isEqualTo(data);
    assertThat(inputStream.read()).isEqualTo(-1);
  }

  @Test
  public void readIntoByteArrayWithAvailableBytesReturnsActualNumberOfBytesRead() throws IOException {

    byte[] data = { 1, 2, 3 };
    byte[] buffer = new byte[data.length * 10];

    ByteBuffer byteBuffer = ByteBuffer.wrap(data);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    assertThat(inputStream.available()).isEqualTo(data.length);
    assertThat(inputStream.read(buffer)).isEqualTo(data.length);
    assertThat(inputStream.available()).isZero();

    for (int index = 0; index < buffer.length; index++) {
      if (index < data.length) {
        assertThat(buffer[index]).isEqualTo(data[index]);
      }
      else {
        assertThat(buffer[index]).isZero();
      }
    }
  }

  @Test
  public void readIntoByteArrayWithAvailableBytesReturnsGivenLengthInBytesRead() throws IOException {

    byte[] data = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF };
    byte[] buffer = new byte[data.length * 2];

    ByteBuffer byteBuffer = ByteBuffer.wrap(data);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    assertThat(inputStream.read(buffer, 5, 5)).isEqualTo(5);

    int dataIndex = 0;

    for (int index = 0; index < buffer.length; index++) {
      if (index < 5 || index > 9) {
        assertThat(buffer[index]).isZero();
      }
      else {
        assertThat(buffer[index]).isEqualTo(data[dataIndex++]);
      }
    }
  }

  @Test
  public void readIntoByteArrayWithUnavailableBytesReturnsMinusOne() throws IOException {

    ByteBuffer mockByteBuffer = emptyByteBuffer();

    doReturn(0).when(mockByteBuffer).remaining();

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(mockByteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(mockByteBuffer);
    assertThat(inputStream.read(new byte[10], 0, 5)).isEqualTo(-1);

    verify(mockByteBuffer, times(1)).remaining();
    verifyZeroPositionedByteBufferInteractions(mockByteBuffer, 0);
  }

  @Test
  public void readIntoZeroLengthByteArrayWithAvailableBytesReturnsZero() throws IOException {

    ByteBuffer mockByteBuffer = singleByteByteBuffer();

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(mockByteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(mockByteBuffer);
    assertThat(inputStream.read(new byte[0], 0, 10)).isZero();

    verifyZeroPositionedByteBufferInteractions(mockByteBuffer);
  }

  @Test
  public void readIntoByteArrayWithInvalidOffset() {

    ThrowableAssertions.assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> tryWithResource(singleByteByteBuffer(), in -> in.read(new byte[5], 7, 5)))
      .havingMessage("Offset [7] must be greater than equal to 0 and less than [5]")
      .withNoCause();
  }

  @Test
  public void readIntoByteArrayWithNegativeOffset() {

    ThrowableAssertions.assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> tryWithResource(singleByteByteBuffer(), in -> in.read(new byte[10], -5, 5)))
      .havingMessage("Offset [-5] must be greater than equal to 0 and less than [10]")
      .withNoCause();
  }

  @Test
  public void readIntoByteArrayWithInvalidLength() {

    ThrowableAssertions.assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> tryWithResource(singleByteByteBuffer(), in -> in.read(new byte[10], 6, 5)))
      .havingMessage("Length [5] must be greater than equal to 0 and less than equal to [4]")
      .withNoCause();
  }

  @Test
  public void readIntoByteArrayWithNegativeLength() {

    ThrowableAssertions.assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> tryWithResource(singleByteByteBuffer(), in -> in.read(new byte[10], 5, -5)))
      .havingMessage("Length [-5] must be greater than equal to 0 and less than equal to [5]")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void readIntoNullByteArrayThrowsNullPointerException() {

    assertThatNullPointerException()
      .isThrownBy(() -> tryWithResource(singleByteByteBuffer(), in -> in.read(null, 0, 10)))
      .withMessage("Byte array cannot be null")
      .withNoCause();
  }

  @Test
  public void resetIsCorrect() throws IOException {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);

    inputStream.reset();

    for (int index = 0; index < 5; index++) {
      assertThat(inputStream.read()).isEqualTo(array[index]);
    }

    assertThat(inputStream.read()).isEqualTo(array[5]);

    inputStream.reset();

    for (byte data : array) {
      assertThat(inputStream.read()).isEqualTo(data);
    }

    assertThat(inputStream.available()).isZero();

    inputStream.reset();

    assertThat(inputStream.available()).isEqualTo(array.length);
  }

  @Test
  public void skipIsCorrect() throws IOException {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    ByteBufferInputStream inputStream = ByteBufferInputStream.from(byteBuffer);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getByteBuffer()).isEqualTo(byteBuffer);
    assertThat(inputStream.skip(3)).isEqualTo(3);

    for (int index = 3; index < 6; index++) {
      assertThat(inputStream.read()).isEqualTo(array[index]);
    }

    assertThat(inputStream.available()).isEqualTo(4);
    assertThat(inputStream.skip(1)).isEqualTo(1);
    assertThat(inputStream.available()).isEqualTo(3);
    assertThat(inputStream.read()).isEqualTo(8);
    assertThat(inputStream.available()).isEqualTo(2);
    assertThat(inputStream.skip(3)).isEqualTo(2);
    assertThat(inputStream.available()).isZero();
  }
}
