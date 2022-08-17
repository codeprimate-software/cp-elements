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
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract utility class for processing {@link Buffer Buffers}.
 *
 * @author John Blum
 * @see java.nio.Buffer
 * @since 1.0.0
 */
public abstract class BufferUtils {

  /**
   * Gets an array of bytes containing the contents of the given, required {@link ByteBuffer}.
   *
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
}
