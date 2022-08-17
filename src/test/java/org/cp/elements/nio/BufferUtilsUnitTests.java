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

import java.nio.ByteBuffer;

import org.junit.Test;

/**
 * Unit Tests for {@link BufferUtils}.
 *
 * @author John Blum
 * @see java.nio.ByteBuffer
 * @see org.junit.Test
 * @see org.cp.elements.nio.BufferUtils
 * @since 1.0.0
 */
public class BufferUtilsUnitTests {

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
