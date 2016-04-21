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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.Random;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The BloomFilterTests class is a test suite of test cases testing the contract and functionality
 * of the {@link BloomFilter} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.BloomFilter
 * @since 1.0.0
 */
public class BloomFilterTests {

  protected static final int NUMBER_COUNT = 50000;

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void constructDefaultBloomFilter() {
    BloomFilter<Integer> bloomFilter = new BloomFilter<>();

    assertThat(bloomFilter, is(notNullValue()));
    assertThat(bloomFilter.bitArray(), is(notNullValue()));
    assertThat(bloomFilter.getBound(null), is(equalTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE * 32)));

    int index;

    for (index = 0; index < bloomFilter.bitArray().length; index++) {
      assertThat(bloomFilter.bitArray()[index], is(equalTo(0)));
    }

    assertThat(index, is(equalTo(BloomFilter.DEFAULT_BIT_ARRAY_SIZE)));
  }

  @Test
  public void constructCustomBloomFilter() {
    BloomFilter<Integer> bloomFilter = new BloomFilter<>(4);

    assertThat(bloomFilter, is(notNullValue()));
    assertThat(bloomFilter.bitArray(), is(notNullValue()));
    assertThat(bloomFilter.getBound(null), is(equalTo(4 * 32)));

    int index;

    for (index = 0; index < bloomFilter.bitArray().length; index++) {
      assertThat(bloomFilter.bitArray()[index], is(equalTo(0)));
    }

    assertThat(index, is(equalTo(4)));
  }

  @Test
  public void constructBloomFilterWithIllegalSize() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("size [-10] must be greater than 0");

    new BloomFilter<>(-10);
  }

  @Test
  public void bitCountForNumberType() {
    BloomFilter<Integer> bloomFilter = new BloomFilter<>();

    assertThat(bloomFilter.getBitCount((byte) 1), is(equalTo(8)));
    assertThat(bloomFilter.getBitCount((short) 1), is(equalTo(16)));
    assertThat(bloomFilter.getBitCount(1), is(equalTo(32)));
    assertThat(bloomFilter.getBitCount(1f), is(equalTo(32)));
    assertThat(bloomFilter.getBitCount(1l), is(equalTo(64)));
    assertThat(bloomFilter.getBitCount(1d), is(equalTo(64)));
    assertThat(bloomFilter.getBitCount(null), is(equalTo(32)));
  }

  @Test
  public void bitMasksBinaryStringsAreCorrect() {
    String expectedBinaryString = "1";

    for (int index = 0; index < BloomFilter.BIT_MASKS.length; index++) {
      String actualBinaryString = Integer.toBinaryString(BloomFilter.BIT_MASKS[index]);
      assertThat(actualBinaryString, is(equalTo(expectedBinaryString)));
      expectedBinaryString += "0";
    }
  }

  @Test
  @SuppressWarnings("all")
  public void randomNumberSetIsAccepted() {
    BloomFilter<Integer> bloomFilter = new BloomFilter<>(32768);

    Random random = new Random(System.currentTimeMillis());

    synchronized (bloomFilter) {
      for (int count = 0; count < NUMBER_COUNT; count++) {
        int number = random.nextInt();

        bloomFilter.add(number);

        assertThat(bloomFilter.accept(number), is(true));
      }

      int count = 0;

      for (int index = 0; index < bloomFilter.bitArray().length; index++) {
        int elementAtBitArrayIndex = bloomFilter.bitArray()[index];
        assertThat(String.format("bitArray[%1$d] == %2$s", index, Integer.toBinaryString(elementAtBitArrayIndex)),
          0xFFFFFFFF & elementAtBitArrayIndex, is(not(equalTo(0xFFFFFFFF))));
      }
    }
  }

}