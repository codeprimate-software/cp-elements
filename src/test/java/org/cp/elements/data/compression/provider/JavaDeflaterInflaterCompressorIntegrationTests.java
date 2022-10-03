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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.zip.Deflater;

import org.junit.Test;

/**
 * Integration Tests for {@link JavaDeflaterInflaterCompressor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.compression.provider.JavaDeflaterInflaterCompressor
 * @since 1.0.0
 */
public class JavaDeflaterInflaterCompressorIntegrationTests {

  @Test
  public void compressAndDecompressIsSuccessful() {

    String data = "The is the test data used for testing the compression and decompression of the Compressor"
      + " implementation, in this case, the JavaDeflaterInflaterCompressor! This test expects that the output of"
      + " the compressed input should be significantly smaller in size than the original uncompressed input."
      + " If that is not the case, then this test will fail since it would be assumed that the compression /"
      + " decompression did not function correctly. Under-the-hood the Compressor implementation uses"
      + " the java.util.zip.Deflater and java.util.zip.Inflater classes of the Java Development Kit (JDK)."
      + " This String of data needs to be significantly long to test the compression/decompression algorithm.";

    JavaDeflaterInflaterCompressor compressor = JavaDeflaterInflaterCompressor.bestCompression();

    assertThat(compressor).isNotNull();
    assertThat(compressor.getCompressionLevel()).isEqualTo(Deflater.BEST_COMPRESSION);

    byte[] input = data.getBytes();
    byte[] compressedInput = compressor.compress(input);

    assertThat(input).hasSizeGreaterThan(512);
    assertThat(compressedInput).isNotNull();
    assertThat(compressedInput).hasSizeLessThan(input.length);

    byte[] decompressedInput = compressor.decompress(compressedInput);

    assertThat(decompressedInput).isNotNull();
    assertThat(decompressedInput).hasSize(input.length);
    assertThat(decompressedInput).hasSizeGreaterThan(compressedInput.length);

    String decompressedData = new String(decompressedInput);

    assertThat(decompressedData).isEqualTo(data);
  }

  @Test
  public void withBestSpeedIsCorrect() {

    JavaDeflaterInflaterCompressor compressor = JavaDeflaterInflaterCompressor.bestSpeed();

    assertThat(compressor).isNotNull();
    assertThat(compressor.getCompressionLevel()).isEqualTo(Deflater.BEST_SPEED);
  }
}
