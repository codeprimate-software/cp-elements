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
package org.cp.elements.data.compression.provider;

import static org.cp.elements.lang.ElementsExceptionsFactory.newCompressionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newDecompressionException;

import java.util.zip.Deflater;
import java.util.zip.Inflater;

import org.cp.elements.data.compression.Compressor;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Compressor} implementation that uses the Java {@link Deflater} and {@link Inflater}.
 *
 * @author John Blum
 * @see java.util.zip.Deflater
 * @see java.util.zip.Inflater
 * @see org.cp.elements.data.compression.Compressor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaDeflaterInflaterCompressor implements Compressor {

  protected static final int DEFAULT_DATA_BUFFER_SIZE = 4096; // bytes
  protected static final int TEMP_BUFFER_SIZE = 512; // bytes

  /**
   * Factory method used to construct a new instance of {@link JavaDeflaterInflaterCompressor} initialized
   * for the {@link Deflater#BEST_COMPRESSION}.
   *
   * @return a new {@link JavaDeflaterInflaterCompressor}.
   */
  public static @NotNull JavaDeflaterInflaterCompressor bestCompression() {
    return new JavaDeflaterInflaterCompressor(Deflater.BEST_COMPRESSION);
  }

  /**
   * Factory method used to construct a new instance of {@link JavaDeflaterInflaterCompressor} initialized
   * for the {@link Deflater#BEST_SPEED}.
   *
   * @return a new {@link JavaDeflaterInflaterCompressor}.
   */
  public static @NotNull JavaDeflaterInflaterCompressor bestSpeed() {
    return new JavaDeflaterInflaterCompressor(Deflater.BEST_SPEED);
  }

  private final int compressionLevel;

  /**
   * Constructs a new instance of {@link JavaDeflaterInflaterCompressor} initialized with
   * the given {@link Deflater#setLevel(int) compression level}.
   *
   * @param compressionLevel {@link Integer} value specifying the {@link Deflater#setLevel(int) compression level}.
   */
  protected JavaDeflaterInflaterCompressor(int compressionLevel) {
    this.compressionLevel = compressionLevel;
  }

  /**
   * Gets the {@link Deflater#setLevel(int) level of compression} used to compress data.
   *
   * @return the {@link Deflater#setLevel(int) level of compression} used to compress data.
   */
  protected int getCompressionLevel() {
    return this.compressionLevel;
  }

  @Override
  public @NotNull byte[] compress(@NotNull byte[] input) {

    Assert.notNull(input, "An array of bytes to compress is required");

    try {
      Deflater compressor = newDeflater(input);

      byte[] output = new byte[input.length];

      int compressedDataLength = deflate(compressor, output);

      return toCompressedBytes(output, compressedDataLength);
    }
    catch (Exception cause) {
      throw newCompressionException(cause, "Failed to compress input");
    }
  }

  private @NotNull Deflater newDeflater(@NotNull byte[] input) {

    Deflater deflater = new Deflater(getCompressionLevel());

    deflater.setInput(input);
    deflater.setStrategy(Deflater.DEFAULT_STRATEGY);
    deflater.finish();

    return deflater;
  }

  private int deflate(@NotNull Deflater compressor, @NotNull byte[] output) {

    int compressedDataLength = compressor.deflate(output);

    compressor.end();

    return compressedDataLength;
  }

  private @NotNull byte[] toCompressedBytes(@NotNull byte[] output, int compressedDataLength) {

    byte[] compressedOutput = new byte[compressedDataLength];

    System.arraycopy(output, 0, compressedOutput, 0, compressedDataLength);

    return compressedOutput;
  }

  @Override
  public @NotNull byte[] decompress(@NotNull byte[] input) {

    Assert.notNull(input, "An array of bytes to decompress is required");

    try {
      Inflater decompressor = newInflater(input);

      byte[] decompressedBytes = new byte[DEFAULT_DATA_BUFFER_SIZE];
      byte[] buffer = new byte[TEMP_BUFFER_SIZE];

      int writeIndex = 0;

      while (decompressor.getRemaining() > 0) {

        int numberOfDecompressedBytes = decompressor.inflate(buffer);

        if (writeIndex + numberOfDecompressedBytes > decompressedBytes.length) {
          decompressedBytes = reallocateBuffer(decompressedBytes);
        }

        System.arraycopy(buffer, 0, decompressedBytes, writeIndex, numberOfDecompressedBytes);
        writeIndex += numberOfDecompressedBytes;
      }

      byte[] result = new byte[decompressor.getTotalOut()];

      decompressor.end();

      System.arraycopy(decompressedBytes, 0, result, 0, result.length);

      return result;
    }
    catch (Exception cause) {
      throw newDecompressionException(cause, "Failed to decompress input");
    }
  }

  private @NotNull Inflater newInflater(byte[] input) {
    Inflater inflater = new Inflater();
    inflater.setInput(input);
    return inflater;
  }

  private byte[] reallocateBuffer(byte[] source) {
    byte[] target = new byte[source.length + DEFAULT_DATA_BUFFER_SIZE];
    System.arraycopy(source, 0, target, 0, source.length);
    return target;
  }
}
