/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.data.struct;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Random;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link BloomFilter} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.BloomFilter
 * @since 1.0.0
 */
public class BloomFilterTests {

  private static final int NUMBER_COUNT = 101000;

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void constructDefaultBloomFilter() {

    BloomFilter<Integer> bloomFilter = new BloomFilter<>();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE);
    assertThat(bloomFilter.getBound(null)).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(0)).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(1)).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(2)).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);

    int index;

    int[] bitArray = bloomFilter.getBitArray();

    for (index = 0; index < bitArray.length; index++) {
      assertThat(bitArray[index]).isZero();
    }

    assertThat(index).isEqualTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE);
  }

  @Test
  public void constructCustomBloomFilter() {

    BloomFilter<Integer> bloomFilter = new BloomFilter<>(4);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(4);
    assertThat(bloomFilter.getBound(null)).isEqualTo(4 * 32);
    assertThat(bloomFilter.getBound(0)).isEqualTo(4 * 32);
    assertThat(bloomFilter.getBound(1)).isEqualTo(4 * 32);
    assertThat(bloomFilter.getBound(2)).isEqualTo(4 * 32);

    int index;

    int[] bitArray = bloomFilter.getBitArray();

    for (index = 0; index < bitArray.length; index++) {
      assertThat(bitArray[index]).isZero();
    }

    assertThat(index).isEqualTo(4);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructBloomFilterWithIllegalSize() {
    try {
      new BloomFilter<>(-10);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Size [-10] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void bitCountForNumberType() {

    BloomFilter<Integer> bloomFilter = new BloomFilter<>();

    assertThat(bloomFilter.getBitCount(Byte.MIN_VALUE)).isEqualTo(8);
    assertThat(bloomFilter.getBitCount(Byte.MAX_VALUE)).isEqualTo(8);
    assertThat(bloomFilter.getBitCount(Short.MIN_VALUE)).isEqualTo(16);
    assertThat(bloomFilter.getBitCount(Short.MAX_VALUE)).isEqualTo(16);
    assertThat(bloomFilter.getBitCount(Integer.MIN_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount(Integer.MAX_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount(Float.MIN_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount(Float.MAX_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount(Long.MIN_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getBitCount(Long.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getBitCount(Double.MIN_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getBitCount(Double.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getBitCount(null)).isEqualTo(32);
  }

  @Test
  public void bitCountForNumberValue() {

    BloomFilter<Long> bloomFilter = new BloomFilter<>();

    assertThat(bloomFilter.getBitCount(1L)).isEqualTo(8);
    assertThat(bloomFilter.getBitCount(16384)).isEqualTo(16);
    assertThat(bloomFilter.getBitCount(128000)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount((long) Integer.MAX_VALUE + Integer.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getBitCount(Math.PI)).isEqualTo(32);
    assertThat(bloomFilter.getBitCount((double) Float.MAX_VALUE + Float.MAX_VALUE)).isEqualTo(64);
  }

  @Test
  public void bitMasksBinaryStringsAreCorrect() {

    String expectedBinaryString = "1";

    for (int index = 0; index < BloomFilter.BIT_MASKS.length; index++, expectedBinaryString += "0") {

      String actualBinaryString = Integer.toBinaryString(BloomFilter.BIT_MASKS[index]);

      assertThat(actualBinaryString).isEqualTo(expectedBinaryString);
    }
  }

  @Test
  @SuppressWarnings("all")
  public void randomNumberSetIsAccepted() {

    BloomFilter<Integer> bloomFilter = new BloomFilter<>(32768);

    Random random = new Random(System.currentTimeMillis());

    synchronized (bloomFilter) {
      for (int count = 0; count < NUMBER_COUNT; count++) {

        int number = Math.abs(random.nextInt());

        bloomFilter.add(number);

        assertThat(bloomFilter.accept(number)).describedAs("Number [%1$d] at count [%2$d] was not acceted",
          number, count).isTrue();
      }
    }
  }

  @Test
  public void bloomFilterIsNotSaturated() {

    BloomFilter<Integer> bloomFilter = new BloomFilter<>();

    Random random = new Random(System.currentTimeMillis());

    for (int count = 0; count < NUMBER_COUNT; count++) {
      bloomFilter.add(random.nextInt());
    }

    // guilty until proven innocent
    boolean saturated = true;

    int[] bitArray = bloomFilter.getBitArray();

    for (int index = 0; index < bitArray.length; index++) {

      int valueAtBitArrayIndex = bitArray[index];

      saturated &= (valueAtBitArrayIndex == 0xFFFFFFFF);
    }

    assertThat(saturated).isFalse();
  }
}
