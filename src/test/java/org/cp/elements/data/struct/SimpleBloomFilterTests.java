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
import static org.cp.elements.lang.NumberUtils.isShort;

import java.util.Random;

import org.cp.elements.lang.NumberUtils;
import org.junit.Test;

/**
 * Unit tests for {@link SimpleBloomFilter}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.SimpleBloomFilter
 * @since 1.0.0
 */
public class SimpleBloomFilterTests {

  private static final int NUMBER_BOUND = 50000;
  private static final int NUMBER_COUNT = 256000;

  @Test
  public void constructDefaultBloomFilter() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE);
    assertThat(bloomFilter.getBound(null)).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(0)).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(1)).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);
    assertThat(bloomFilter.getBound(2)).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32);

    int index;

    int[] bitArray = bloomFilter.getBitArray();

    for (index = 0; index < bitArray.length; index++) {
      assertThat(bitArray[index]).isZero();
    }

    assertThat(index).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_SIZE);
  }

  @Test
  public void constructCustomBloomFilter() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>(4);

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
      new SimpleBloomFilter<>(-10);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Size [-10] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void bitMasksBinaryStringsAreCorrect() {

    String expectedBinaryString = "1";

    for (int index = 0; index < SimpleBloomFilter.BIT_MASKS.length; index++, expectedBinaryString += "0") {

      String actualBinaryString = Integer.toBinaryString(SimpleBloomFilter.BIT_MASKS[index]);

      assertThat(actualBinaryString).isEqualTo(expectedBinaryString);
    }
  }

  @Test
  public void hashFunctionCountForNumberType() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>();

    assertThat(bloomFilter.getHashFunctionCount(Byte.MIN_VALUE)).isEqualTo(8);
    assertThat(bloomFilter.getHashFunctionCount(Byte.MAX_VALUE)).isEqualTo(8);
    assertThat(bloomFilter.getHashFunctionCount(Short.MIN_VALUE)).isEqualTo(16);
    assertThat(bloomFilter.getHashFunctionCount(Short.MAX_VALUE)).isEqualTo(16);
    assertThat(bloomFilter.getHashFunctionCount(Integer.MIN_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount(Integer.MAX_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount(Float.MIN_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount(Float.MAX_VALUE)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount(Long.MIN_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(Long.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(Double.MIN_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(Double.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(null)).isEqualTo(SimpleBloomFilter.DEFAULT_HASH_FUNCTION_COUNT);
  }

  @Test
  public void hashFunctionCountForNumberValue() {

    SimpleBloomFilter<Long> bloomFilter = new SimpleBloomFilter<>();

    assertThat(bloomFilter.getHashFunctionCount(1L)).isEqualTo(8);
    assertThat(bloomFilter.getHashFunctionCount(16384)).isEqualTo(16);
    assertThat(bloomFilter.getHashFunctionCount(128000)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount((long) Integer.MAX_VALUE + Integer.MAX_VALUE)).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(Math.PI)).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount((double) Float.MAX_VALUE + Float.MAX_VALUE)).isEqualTo(64);
  }

  @Test
  @SuppressWarnings("all")
  public void randomNumberSetIsAccepted() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>();

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
  public void evenNumbersAreAcceptedOddNumbersAreRejected() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>();

    for (int number = 0; number < 100; number++) {
      if (NumberUtils.isEven(number)) {
        bloomFilter.add(number);
      }
    }

    for (int number = 0; number < 100; number++) {
      assertThat(bloomFilter.accept(number)).isEqualTo(NumberUtils.isEven(number));
    }
  }

  @Test
  public void bloomFilterIsNotSaturated() {

    SimpleBloomFilter<Integer> bloomFilter = new SimpleBloomFilter<>();

    Random random = new Random(System.currentTimeMillis());

    int numbersLessThanShort = 0;

    for (int count = 0; count < NUMBER_COUNT; count++) {

      int number = Math.abs(random.nextInt(NUMBER_BOUND));

      numbersLessThanShort += isShort(number) ? 1 : 0;

      bloomFilter.add(random.nextInt());
    }

    // guilty until proven innocent
    boolean saturated = true;

    //int saturatedCount = 0;
    //int unsaturatedCount = 0;

    int[] bitArray = bloomFilter.getBitArray();

    for (int valueAtBitArrayIndex : bitArray) {

      saturated &= (valueAtBitArrayIndex == 0xFFFFFFFF);

      /*
      if (valueAtBitArrayIndex == 0xFFFFFFFF) {
        saturatedCount++;
      }
      else {
        unsaturatedCount++;
      }
      */
    }

    /*
    System.out.printf("%nNumbers less than short [%d]%n", numbersLessThanShort);

    System.out.printf("Saturated Count [%1$d]; Unsaturated Count [%2$d]; Saturation Ratio [%3$s percent]%n",
      saturatedCount, unsaturatedCount,
        Double.valueOf((double) saturatedCount / (double) bitArray.length * 100d).toString());
    */

    assertThat(saturated).isFalse();
  }
}
