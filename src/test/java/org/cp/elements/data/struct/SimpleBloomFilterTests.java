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
package org.cp.elements.data.struct;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.util.Random;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.NumberUtils;

/**
 * Unit Tests for {@link SimpleBloomFilter}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.struct.SimpleBloomFilter
 * @since 1.0.0
 */
public class SimpleBloomFilterTests {

  private static final boolean LOG_ENABLED = false;

  private static final int NUMBER_BOUND = 100000;

  private static final int NUMBER_COUNT =
    SystemPropertyValue.newSystemPropertyValue("cp.elements.data.struct.SimpleBloomFilter.number-count")
      .getValueAs(Integer.class, 10000); // 500000;

  private void log(String message, Object... args) {

    if (LOG_ENABLED) {
      System.err.printf(message, args);
      System.err.flush();
    }
  }

  @Test
  public void constructDefaultBloomFilter() {

    SimpleBloomFilter<Object> bloomFilter = new SimpleBloomFilter<>();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_LENGTH);
    assertThat(bloomFilter.getFalsePositiveRate()).isEqualTo(0.0f);
    assertThat(bloomFilter.getFilterSize()).isEqualTo(SimpleBloomFilter.DEFAULT_NUMBER_OF_BITS);
    assertThat(bloomFilter.getHashFunctionCount(null)).isEqualTo(SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
    assertThat(bloomFilter.size()).isZero();

    int count = 0;

    for (int bucket : bloomFilter.getBitArray()) {
      assertThat(bucket).isZero();
      count++;
    }

    assertThat(count).isEqualTo(SimpleBloomFilter.DEFAULT_BIT_ARRAY_LENGTH);
  }

  @Test
  public void constructCustomBloomFilter() {

    SimpleBloomFilter<Object> bloomFilter = new SimpleBloomFilter<>(64, 13);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(2);
    assertThat(bloomFilter.getFalsePositiveRate()).isEqualTo(0.0f);
    assertThat(bloomFilter.getFilterSize()).isEqualTo(64);
    assertThat(bloomFilter.getHashFunctionCount(null)).isEqualTo(13);
    assertThat(bloomFilter.size()).isZero();

    int count = 0;

    for (int bucket : bloomFilter.getBitArray()) {
      assertThat(bucket).isZero();
      count++;
    }

    assertThat(count).isEqualTo(2);
  }

  @Test
  public void constructBloomFilterWithIllegalNumberOfBits() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SimpleBloomFilter<>(-1, SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS))
      .withMessage("Number of bits [-1] must be greater than 0")
      .withNoCause();
  }

  @Test
  public void constructBloomFilterWithIllegalNumberOfHashFunctions() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SimpleBloomFilter<>(SimpleBloomFilter.DEFAULT_NUMBER_OF_BITS, -1))
      .withMessage("Number of hash functions [-1] must be greater than 0")
      .withNoCause();
  }

  @Test
  public void simpleBloomFilterOfApproximateNumberOfElements() {

    SimpleBloomFilter<Object> bloomFilter = SimpleBloomFilter.of(100);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isGreaterThan(0);
    assertThat(bloomFilter.getBitArray().length).isLessThan(SimpleBloomFilter.DEFAULT_BIT_ARRAY_LENGTH);
    assertThat(bloomFilter.getFalsePositiveRate()).isEqualTo(SimpleBloomFilter.DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE);
    assertThat(bloomFilter.getFilterSize()).isEqualTo(bloomFilter.getBitArray().length * 32);
    assertThat(bloomFilter.getHashFunctionCount(null)).isGreaterThan(0);
    assertThat(bloomFilter.getHashFunctionCount(null)).isLessThan(SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
    assertThat(bloomFilter.size()).isZero();
  }

  @Test
  public void simpleBloomFilterOfApproximateNumberOfElementsAndAcceptableFalsePositiveRate() {

    SimpleBloomFilter<Object> bloomFilter =
      SimpleBloomFilter.of(100, 0.05f);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isGreaterThan(0);
    assertThat(bloomFilter.getBitArray().length).isLessThan(SimpleBloomFilter.DEFAULT_BIT_ARRAY_LENGTH);
    assertThat(bloomFilter.getFalsePositiveRate()).isEqualTo(0.05f);
    assertThat(bloomFilter.getFilterSize()).isEqualTo(bloomFilter.getBitArray().length * 32);
    assertThat(bloomFilter.getHashFunctionCount(null)).isGreaterThan(0);
    assertThat(bloomFilter.getHashFunctionCount(null)).isLessThan(SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
    assertThat(bloomFilter.size()).isZero();
  }

  @Test
  public void simpleBloomFilterOfOneAndDefaultAcceptableFalsePositiveRate() {

    SimpleBloomFilter<Object> bloomFilter = SimpleBloomFilter.ofOne();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getBitArray()).isNotNull();
    assertThat(bloomFilter.getBitArray().length).isEqualTo(1);
    assertThat(bloomFilter.getFalsePositiveRate()).isEqualTo(SimpleBloomFilter.DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE);
    assertThat(bloomFilter.getFilterSize()).isEqualTo(32);
    assertThat(bloomFilter.getHashFunctionCount(null)).isLessThan(SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
    assertThat(bloomFilter.size()).isZero();
  }

  @Test
  public void simpleBloomFilterOfZeroNumberOfElements() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, 0))
      .withMessage("The approximate number of elements [0] to add to the filter must be greater than 0")
      .withNoCause();

  }

  @Test
  public void simpleBloomFilterOfMinusOneHundredNumberOfElements() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, -100))
      .withMessage("The approximate number of elements [-100] to add to the filter must be greater than 0")
      .withNoCause();
  }

  @Test
  public void simpleBloomFilterOfMinusFiftyPercentAcceptableFalsePositiveRate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, -0.5f))
      .withMessage("The acceptable false positive rate [-0.5] must be greater than 0.0 and less than 1.0")
      .withNoCause();
  }

  @Test
  public void simpleBloomFilterOfZeroPercentAcceptableFalsePositiveRate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, 0.0f))
      .withMessage("The acceptable false positive rate [0.0] must be greater than 0.0 and less than 1.0")
      .withNoCause();
  }

  @Test
  public void simpleBloomFilterOfOneHundredPercentAcceptableFalsePositiveRate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, 1.0f))
      .withMessage("The acceptable false positive rate [1.0] must be greater than 0.0 and less than 1.0")
      .withNoCause();
  }

  @Test
  public void simpleBloomFilterOfFiveHundredPercentAcceptableFalsePositiveRate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleBloomFilter.of(1, 5.0f))
      .withMessage("The acceptable false positive rate [5.0] must be greater than 0.0 and less than 1.0")
      .withNoCause();
  }

  @Test
  public void bitArrayLengthForMultipleOfThirtyTwoBitsIsCorrect() {
    assertThat(SimpleBloomFilter.ofOne().getBitArrayLength(64)).isEqualTo(2);
  }

  @Test
  public void bitArrayLengthForOffNumberOfBitsIsCorrect() {
    assertThat(SimpleBloomFilter.ofOne().getBitArrayLength(200)).isEqualTo(7);
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
  public void computeOptimalNumberOfHashFunctionsIsCorrect() {

    int optimalNumberOfHashFunctions =
      SimpleBloomFilter.computeOptimalNumberOfHashFunctions(100000, 479253);

    assertThat(optimalNumberOfHashFunctions).isEqualTo(4);

    optimalNumberOfHashFunctions =
      SimpleBloomFilter.computeOptimalNumberOfHashFunctions(100000, 144270);

    assertThat(optimalNumberOfHashFunctions).isEqualTo(2);

    optimalNumberOfHashFunctions =
      SimpleBloomFilter.computeOptimalNumberOfHashFunctions(100, 1103);

    assertThat(optimalNumberOfHashFunctions).isEqualTo(8);

    optimalNumberOfHashFunctions =
      SimpleBloomFilter.computeOptimalNumberOfHashFunctions(10000, 512);

    assertThat(optimalNumberOfHashFunctions).isEqualTo(1);
  }

  @Test
  public void computeRequiredNumberOfBitsIsCorrect() {

    int requiredNumberOfBits =
      SimpleBloomFilter.computeRequiredNumberOfBits(100000, 0.1f);

    assertThat(requiredNumberOfBits).isEqualTo(479253);

    requiredNumberOfBits =
      SimpleBloomFilter.computeRequiredNumberOfBits(100000, 0.5f);

    assertThat(requiredNumberOfBits).isEqualTo(144270);

    requiredNumberOfBits =
      SimpleBloomFilter.computeRequiredNumberOfBits(100, 0.1f);

    assertThat(requiredNumberOfBits).isEqualTo(480);

    requiredNumberOfBits =
      SimpleBloomFilter.computeRequiredNumberOfBits(100, 0.005f);

    assertThat(requiredNumberOfBits).isEqualTo(1103);
  }

  @Test
  @SuppressWarnings("all")
  public void evenNumbersAreAcceptedOddNumbersRejected() {

    SimpleBloomFilter<Integer> bloomFilter = SimpleBloomFilter.of(100);

    synchronized (bloomFilter) {

      for (int number = 0; number < 100; number += 2) {
        bloomFilter.add(number);
      }

      for (int number = 0; number < 100; number++) {

        boolean even = NumberUtils.isEven(number);

        String evenOddNumberDescriptor = (even ? "Even" : "Odd");
        String acceptedRejectedDescriptor = (even ? "rejected" : "accepted");

        assertThat(bloomFilter.accept(number))
          .describedAs("%s number [%d] was %s", evenOddNumberDescriptor, number, acceptedRejectedDescriptor)
          .isEqualTo(even);
      }

      assertThat(bloomFilter.size()).isGreaterThanOrEqualTo(49);
      assertThat(bloomFilter.size()).isLessThanOrEqualTo(51);
    }
  }

  @Test
  @SuppressWarnings("all")
  public void oddNumbersAreAcceptedEvenNumbersRejected() {

    SimpleBloomFilter<Integer> bloomFilter = SimpleBloomFilter.of(100);

    synchronized (bloomFilter) {

      for (int number = 1; number < 100; number += 2) {
        bloomFilter.add(number);
      }

      for (int number = 1; number < 100; number++) {

        boolean odd = NumberUtils.isOdd(number);

        String evenOddNumberDescriptor = (odd ? "Odd" : "Even");
        String acceptedRejectedDescriptor = (odd ? "rejected" : "accepted");

        assertThat(bloomFilter.accept(number))
          .describedAs("%s number [%d] was %s", evenOddNumberDescriptor, number, acceptedRejectedDescriptor)
          .isEqualTo(odd);
      }

      assertThat(bloomFilter.size()).isGreaterThanOrEqualTo(49);
      assertThat(bloomFilter.size()).isLessThanOrEqualTo(51);
    }
  }

  @Test
  @SuppressWarnings("all")
  public void randomNumbersAreAccepted() {

    SimpleBloomFilter<Integer> bloomFilter = SimpleBloomFilter.of(100);

    Random random = new Random(System.currentTimeMillis());

    synchronized (bloomFilter) {

      for (int count = 0; count < 100; count++) {

        int number = Math.abs(random.nextInt(NUMBER_BOUND));

        bloomFilter.add(number);

        assertThat(bloomFilter.accept(number))
          .describedAs("Number [%1$d] at count [%2$d] was not accepted", number, count)
          .isTrue();
      }

      assertThat(bloomFilter.size()).isGreaterThan(90);
      assertThat(bloomFilter.size()).isLessThan(110);
    }
  }

  @Test
  public void bloomFilterHasAcceptableSaturation() {

    SimpleBloomFilter<Integer> bloomFilter = SimpleBloomFilter.of(NUMBER_COUNT);

    Random random = new Random(System.currentTimeMillis());

    for (int count = 0; count < NUMBER_COUNT; count++) {
      bloomFilter.add(Math.abs(random.nextInt(NUMBER_BOUND)));
    }

    int saturatedBucketCount = 0;
    int unsaturatedBucketCount = 0;

    int[] bitArray = bloomFilter.getBitArray();

    for (int bits : bitArray) {
      if (bits == 0xFFFFFFFF) {
        saturatedBucketCount++;
      }
      else {
        unsaturatedBucketCount++;
      }
    }

    assertThat(unsaturatedBucketCount).isNotZero();

    double saturationRatio = (double) saturatedBucketCount / (double) bitArray.length;

    log("%nSimpleBloomFilter Saturated Bucket Count [%1$d]; Unsaturated Bucket Count [%2$d]; Saturation Ratio [%3$s percent]%n",
      saturatedBucketCount, unsaturatedBucketCount, String.valueOf(saturationRatio * 100.0d));

    //assertThat(saturated).isFalse();

    assertThat(saturationRatio)
      .describedAs("Saturation ratio [%s percent] was not less than 1 percent", saturationRatio)
      .isLessThan(bloomFilter.getFalsePositiveRate());
  }
}
