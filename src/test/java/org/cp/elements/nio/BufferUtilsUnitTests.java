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

import java.nio.Buffer;
import java.nio.ByteBuffer;

import org.cp.elements.lang.NumberUtils;

import org.junit.Test;

/**
 * Unit Tests for {@link BufferUtils}.
 *
 * @author John Blum
 * @see java.nio.ByteBuffer
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.nio.BufferUtils
 * @since 1.0.0
 */
public class BufferUtilsUnitTests {

  private <T extends Buffer> T reposition(T buffer, int newPosition) {
    assertThat(buffer.position(newPosition).position()).isEqualTo(newPosition);
    return buffer;
  }

  @Test
  public void computeLoadFactorIsCorrect() {

    ByteBuffer byteBuffer = ByteBuffer.allocate(10);

    assertThat(byteBuffer).isNotNull();
    assertThat(byteBuffer.capacity()).isEqualTo(10);
    assertThat(byteBuffer.position()).isZero();
    assertThat(BufferUtils.computeLoadFactor(byteBuffer)).isEqualTo(0.0f); // 0%
    assertThat(BufferUtils.computeLoadFactor(reposition(byteBuffer, 2))).isEqualTo(0.2f); // 20%
    assertThat(BufferUtils.computeLoadFactor(reposition(byteBuffer, 5))).isEqualTo(0.5f); // 50%
    assertThat(BufferUtils.computeLoadFactor(reposition(byteBuffer, 8))).isEqualTo(0.8f); // 80%
    assertThat(BufferUtils.computeLoadFactor(reposition(byteBuffer, 10))).isEqualTo(1.0f); // 100%
  }

  @Test
  public void computeLoadFactorWithNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.computeLoadFactor(null))
      .withMessage("A Buffer is required to compute load factor")
      .withNoCause();
  }

  @Test
  public void copyByteBufferIsSuccessful() {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    assertThat(byteBuffer).isNotNull();
    assertThat(byteBuffer.position()).isZero();
    assertThat(byteBuffer.limit()).isEqualTo(array.length);
    assertThat(byteBuffer.capacity()).isEqualTo(array.length);

    ByteBuffer byteBufferCopy = BufferUtils.copy(byteBuffer, array.length * 2);

    assertThat(byteBufferCopy).isNotNull();
    assertThat(byteBufferCopy).isNotSameAs(byteBuffer);
    assertThat(byteBufferCopy.position()).isEqualTo(array.length);
    assertThat(byteBufferCopy.limit()).isEqualTo(byteBufferCopy.capacity());
    assertThat(byteBufferCopy.capacity())
      .describedAs("Buffer copy capacity is [%d]", byteBufferCopy.capacity())
      .isEqualTo(array.length + array.length * 2);

    // Must rewind the copy to prepare for reading from the beginning of the buffer all the data in the original buffer
    // plus any additional data added to the copy
    assertThat(byteBufferCopy.limit(byteBufferCopy.capacity()).rewind()).isEqualTo(byteBufferCopy);
    assertThat(byteBufferCopy.position()).isZero();
    assertThat(byteBufferCopy.limit()).isEqualTo(byteBufferCopy.capacity());
    assertThat(byteBufferCopy.capacity()).isEqualTo(array.length + array.length * 2);

    int count = 0;

    for (byte element : array) {
      assertThat(byteBufferCopy.get()).isEqualTo(element);
      count++;
    }

    assertThat(count).isEqualTo(array.length);
    assertThat(byteBufferCopy.position()).isEqualTo(15);
    assertThat(byteBufferCopy.remaining()).isEqualTo(byteBufferCopy.capacity() - array.length);
  }

  @Test
  public void copyByteBufferWithNonZeroPositionAndLimitLessThanCapacity() {

    ByteBuffer source = ByteBuffer.allocate(6);

    assertThat(source).isNotNull();

    source.put(NumberUtils.byteValue(0x0C));
    source.put(NumberUtils.byteValue(0x0B));
    source.put(NumberUtils.byteValue(0x0A));

    assertThat(source.position()).isEqualTo(3);
    assertThat(source.limit()).isEqualTo(source.capacity());
    assertThat(source.capacity()).isEqualTo(6);

    ByteBuffer copy = BufferUtils.copy(source, source.capacity());

    assertThat(copy).isNotNull();
    assertThat(copy.position()).isEqualTo(3);
    assertThat(copy.limit()).isEqualTo(copy.capacity());
    assertThat(copy.capacity()).isEqualTo(source.capacity() * 2);

    for (int number = 9; number > 0; number--) {
      copy.put(NumberUtils.byteValue(number));
    }

    assertThat(copy.remaining()).isZero();
    assertThat(copy.array())
      .containsExactly(0x0C, 0x0B, 0x0A, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01);
  }

  @Test
  public void copyByteBufferWithNegativeAdditionalCapacity() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.copy(ByteBuffer.allocate(1), -1))
      .withMessage("Additional capacity [-1] must be greater than equal to 0")
      .withNoCause();
  }

  @Test
  public void copyByteBufferWithNoCapacity() {

    assertThatIllegalStateException()
      .isThrownBy(() -> BufferUtils.copy(ByteBuffer.allocate(0), 0))
      .withMessage("ByteBuffer to copy has no capacity")
      .withNoCause();
  }

  @Test
  public void copyNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.copy(null, 10))
      .withMessage("ByteBuffer to copy is required")
      .withNoCause();
  }

  @Test
  public void getByteArrayFromByteBuffer() {

    ByteBuffer buffer = ByteBuffer.wrap("TEST".getBytes());

    byte[] array = BufferUtils.getByteArray(buffer);

    assertThat(array).isNotNull();
    assertThat(array).isNotEmpty();
    assertThat(new String(array)).isEqualTo("TEST");
  }

  @Test
  public void getByteArrayFromNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.getByteArray(null))
      .withMessage("ByteBuffer is required")
      .withNoCause();
  }

  @Test
  public void toBigByteArrayFromPrimitiveByteArray() {

    byte[] primitiveByteArray =
      { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F };

    Byte[] bigByteArray = BufferUtils.toBigByteArray(primitiveByteArray);

    assertThat(bigByteArray).isNotNull();
    assertThat(bigByteArray).hasSize(primitiveByteArray.length);

    int index = 0;

    for (Byte element : bigByteArray) {
      assertThat(element.byteValue()).isEqualTo(primitiveByteArray[index++]);
    }
  }

  @Test
  public void toBigByteArrayFromNullIsNullSafeReturnsEmptyArray() {

    Byte[] array = BufferUtils.toBigByteArray(null);

    assertThat(array).isNotNull();
    assertThat(array).isEmpty();
  }

  @Test
  public void toPrimitiveByteArrayFromBigByteArray() {

    Byte[] bigByteArray = { 0x01, 0x02, null, 0x04, null, null, null, 0x08, null };

    byte[] primitiveByteArray = BufferUtils.toPrimitiveByteArray(bigByteArray);

    assertThat(primitiveByteArray).isNotNull();
    assertThat(primitiveByteArray).hasSize(bigByteArray.length);
    assertThat(primitiveByteArray).containsExactly(0x01, 0x02, 0x00, 0x04, 0x00, 0x00, 0x00, 0x08, 0x00);
  }

  @Test
  public void toPrimitiveByteArrayFromNullIsNullSafeReturnEmptyArray() {

    byte[] array = BufferUtils.toPrimitiveByteArray(null);

    assertThat(array).isNotNull();
    assertThat(array).isEmpty();
  }
}
