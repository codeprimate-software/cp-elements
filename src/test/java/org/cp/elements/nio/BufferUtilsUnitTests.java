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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.nio.ByteBuffer;

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

  @Test
  public void computeLoadFactorIsCorrect() {

    ByteBuffer mockByteBuffer = mock(ByteBuffer.class);

    doReturn(0).doReturn(5).doReturn(8).when(mockByteBuffer).position();
    doReturn(10).when(mockByteBuffer).capacity();

    assertThat(BufferUtils.computeLoadFactor(mockByteBuffer)).isEqualTo(1.0f);
    assertThat(BufferUtils.computeLoadFactor(mockByteBuffer)).isEqualTo(0.5f);
    assertThat(BufferUtils.computeLoadFactor(mockByteBuffer)).isEqualTo(0.8f);
  }

  @Test
  public void computeLoadFactorWithNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.computeLoadFactor(null))
      .withMessage("ByteBuffer is required to compute load factor")
      .withNoCause();
  }

  @Test
  public void copyByteBufferIsSuccessful() {

    byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF };

    ByteBuffer byteBuffer = ByteBuffer.wrap(array);

    ByteBuffer copy = BufferUtils.copy(byteBuffer, array.length * 2);

    assertThat(copy).isNotNull();
    assertThat(copy.capacity()).isGreaterThanOrEqualTo(array.length * 2);

    while (byteBuffer.position() < byteBuffer.capacity()) {
      assertThat(copy.get()).isEqualTo(byteBuffer.get());
    }

    assertThat(copy.remaining()).isEqualTo(copy.capacity() - array.length);
  }

  @Test
  public void copyByteBufferWithNegativeAdditionalCapacity() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.copy(mock(ByteBuffer.class), -1))
      .withMessage("Additional capacity [-1] must be greater than equal to 0")
      .withNoCause();
  }

  @Test
  public void copyNullByteBuffer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BufferUtils.copy(null, 10))
      .withMessage("ByteBuffer is required to copy")
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
}
