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

import java.util.Arrays;
import java.util.Random;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The BloomFilter class is a probabilistic data structure to test whether an data element is a member of a set.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see <a href="https://en.wikipedia.org/wiki/Bloom_filter">Bloom Filter</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BloomFilter<T extends Number> implements Filter<T> {

  protected static final int DEFAULT_BIT_ARRAY_SIZE = 4096;

  protected static final int[] BIT_MASKS = new int[32];

  static {
    BIT_MASKS[0] = 0x00000001;
    BIT_MASKS[1] = 0x00000002;
    BIT_MASKS[2] = 0x00000004;
    BIT_MASKS[3] = 0x00000008;
    BIT_MASKS[4] = 0x00000010;
    BIT_MASKS[5] = 0x00000020;
    BIT_MASKS[6] = 0x00000040;
    BIT_MASKS[7] = 0x00000080;
    BIT_MASKS[8] = 0x00000100;
    BIT_MASKS[9] = 0x00000200;
    BIT_MASKS[10] = 0x00000400;
    BIT_MASKS[11] = 0x00000800;
    BIT_MASKS[12] = 0x00001000;
    BIT_MASKS[13] = 0x00002000;
    BIT_MASKS[14] = 0x00004000;
    BIT_MASKS[15] = 0x00008000;
    BIT_MASKS[16] = 0x00010000;
    BIT_MASKS[17] = 0x00020000;
    BIT_MASKS[18] = 0x00040000;
    BIT_MASKS[19] = 0x00080000;
    BIT_MASKS[20] = 0x00100000;
    BIT_MASKS[21] = 0x00200000;
    BIT_MASKS[22] = 0x00400000;
    BIT_MASKS[23] = 0x00800000;
    BIT_MASKS[24] = 0x01000000;
    BIT_MASKS[25] = 0x02000000;
    BIT_MASKS[26] = 0x04000000;
    BIT_MASKS[27] = 0x08000000;
    BIT_MASKS[28] = 0x10000000;
    BIT_MASKS[29] = 0x20000000;
    BIT_MASKS[30] = 0x40000000;
    BIT_MASKS[31] = 0x80000000;
  }

  private final int[] bitArray;

  private final Random random = new Random();

  public BloomFilter() {
    this(DEFAULT_BIT_ARRAY_SIZE);
  }

  public BloomFilter(int size) {
    Assert.isTrue(size > 0, "size [%1$d] must be greater than 0", size);

    bitArray = new int[size];
    Arrays.fill(bitArray, 0);
  }

  protected int getBound() {
    return (bitArray.length * 32);
  }

  public synchronized void add(T number) {
    random.setSeed(number.longValue());

    int bound = getBound();
    int bitCount = random.nextInt(bound);

    for (int count = 0; count < bitCount; count++) {
      int bitIndex = random.nextInt(bound);
      bitArray[bitIndex / 32] |= BIT_MASKS[bitIndex % 32];
    }
  }

  @Override
  public synchronized boolean accept(T number) {
    boolean accepted = true;

    random.setSeed(number.longValue());

    int bound = getBound();
    int bitCount = random.nextInt(bound);

    for (int count = 0; accepted && count < bitCount; count++) {
      int bitIndex = random.nextInt(bound);
      accepted = ((bitArray[bitIndex / 32] & BIT_MASKS[bitIndex % 32]) != 0);
    }

    return accepted;
  }

}
