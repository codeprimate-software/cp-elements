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
package org.cp.elements.data.compression;

/**
 * Interface defining a contract for compressing and decompressing {@link Object Objects}
 * serialized as an array of bytes.
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Compressor {

  /**
   * Compresses the given array of {@link Byte#TYPE bytes}.
   *
   * @param array array of {@link Byte#TYPE bytes} to compress.
   * @return an array of compressed {@link Byte#TYPE bytes}
   * from the array of decompressed {@link Byte#TYPE bytes}.
   * @see #decompress(byte[])
   */
  byte[] compress(byte[] array);

  /**
   * Decompresses the given array of {@link Byte#TYPE bytes}.
   *
   * @param array array of {@link Byte#TYPE bytes} to decompress.
   * @return an array of decompressed {@link Byte#TYPE bytes}
   * from the given array of compressed {@link Byte#TYPE bytes}.
   * @see #compress(byte[])
   */
  byte[] decompress(byte[] array);

}
