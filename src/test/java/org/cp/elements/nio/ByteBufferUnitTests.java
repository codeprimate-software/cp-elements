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

import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;

import org.junit.Test;

/**
 * Unit Tests for Java's {@link ByteBuffer}.
 *
 * @author John Blum
 * @see java.nio.ByteBuffer
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ByteBufferUnitTests {

  @Test
  public void allocateIsCorrect() {

    int capacity = 10;

    ByteBuffer buffer = ByteBuffer.allocate(capacity);

    assertThat(buffer).isNotNull();
    assertThat(buffer.position()).isZero();
    assertThat(buffer.limit()).isEqualTo(capacity);
    assertThat(buffer.capacity()).isEqualTo(capacity);
  }

  @Test(expected = BufferOverflowException.class)
  public void writeToByteBufferOverCapacityThrowsBufferOverflowException() {

    ByteBuffer buffer = ByteBuffer.allocate(10);

    for (int index = 0; index < 10; index++) {
      buffer.putInt(index);
    }
  }
}