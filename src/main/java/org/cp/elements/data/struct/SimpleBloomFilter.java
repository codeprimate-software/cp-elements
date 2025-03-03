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

import static java.util.Arrays.stream;

import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Integers;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * A probabilistic data structure testing whether a given data element is a member of the set maintained by the filter.
 *
 * @author John J. Blum
 * @param <T> {@link Number type} of the elements contained by this {@link BloomFilter}.
 * @see java.util.Random
 * @see org.cp.elements.data.struct.BloomFilter
 * @see org.cp.elements.data.struct.ScalableBloomFilter
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @see <a href="https://en.wikipedia.org/wiki/Bloom_filter">Bloom Filter</a>
 * @see <a href="https://stackoverflow.com/questions/658439/how-many-hash-functions-does-my-bloom-filter-need">How many hash functions does my bloom filter need?</a>
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class SimpleBloomFilter<T> implements BloomFilter<T> {

  protected static final float DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE = 0.01f; // 1%

  protected static final int THIRTY_TWO_BITS = Integers.THIRTY_TWO;
  protected static final int DEFAULT_BIT_ARRAY_LENGTH = 16384;
  protected static final int DEFAULT_NUMBER_OF_BITS = DEFAULT_BIT_ARRAY_LENGTH * THIRTY_TWO_BITS; // 64 KB filter
  protected static final int DEFAULT_NUMBER_OF_HASH_FUNCTIONS = 11;

  static final int[] BIT_MASKS = new int[32];

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

  private volatile float falsePositiveRate;

  private final int hashFunctionCount;

  private final int[] bitArray;

  private final Random random = new Random();

  /**
   * Factory method used to construct an instance of the {@link SimpleBloomFilter} class initialized with a size of
   * 1 element expected to be added to the {@link BloomFilter}.
   * <p>
   * The acceptable, false positive rate defaults to 1%.
   *
   * @param <T> {@link Class type} of elements added/evaluated by this Bloom Filter.
   * @return a new instance of the {@link SimpleBloomFilter}.
   * @see org.cp.elements.data.struct.SimpleBloomFilter
   * @see #of(int, float)
   */
  public static <T> SimpleBloomFilter<T> ofOne() {
    return of(1, DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE);
  }

  /**
   * Factory method used to construct an instance of the {@link SimpleBloomFilter} class initialized with
   * an approximate, estimated number of elements that the caller expects will be added to the {@link BloomFilter}.
   * <p>
   * The acceptable, false positive rate defaults to 1%.
   *
   * @param <T> {@link Class type} of elements added/evaluated by this Bloom Filter.
   * @param approximateNumberOfElements integer value indicating the approximate, estimated number of elements
   * the user expects will be added to the returned {@link SimpleBloomFilter} instance.
   * @return a new instance of {@link SimpleBloomFilter}.
   * @throws IllegalArgumentException if the approximate number of elements is less than equal to 0.
   * @see org.cp.elements.data.struct.SimpleBloomFilter
   * @see #of(int, float)
   */
  public static <T> SimpleBloomFilter<T> of(int approximateNumberOfElements) {
    return of(approximateNumberOfElements, DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE);
  }

  /**
   * Factory method used to construct an instance of the {@link SimpleBloomFilter} class initialized with
   * an approximate, estimated number of elements that the user expects will be added to the {@link BloomFilter}
   * along with the desired, acceptable false positive rate.
   *
   * @param <T> {@link Class type} of elements added/evaluated by this Bloom Filter.
   * @param approximateNumberOfElements integer value indicating the approximate, estimated number of elements
   * the user expects will be added to the returned {@link SimpleBloomFilter} instance.
   * @param acceptableFalsePositiveRate a floating point value indicating the acceptable percentage of false positives
   * returned by the constructed {@link SimpleBloomFilter}.
   * @return a new instance of {@link SimpleBloomFilter}.
   * @throws IllegalArgumentException if the approximate number of elements is less than equal to 0 or the acceptable,
   * false positive rate is less than equal to 0.0 or greater than equal to 1.0.
   * @see #computeRequiredNumberOfBits(double, double)
   * @see #computeOptimalNumberOfHashFunctions(double, double)
   * @see org.cp.elements.data.struct.SimpleBloomFilter
   */
  public static <T> SimpleBloomFilter<T> of(int approximateNumberOfElements, float acceptableFalsePositiveRate) {

    Assert.isTrue(approximateNumberOfElements > 0,
      "The approximate number of elements [%d] to add to the filter must be greater than 0",
      approximateNumberOfElements);

    Assert.isTrue(acceptableFalsePositiveRate > 0.0f && acceptableFalsePositiveRate < 1.0f,
      "The acceptable false positive rate [%s] must be greater than 0.0 and less than 1.0",
        String.valueOf(acceptableFalsePositiveRate));

    int requiredNumberOfBits = computeRequiredNumberOfBits(approximateNumberOfElements, acceptableFalsePositiveRate);

    int optimalNumberOfHashFunctions = computeOptimalNumberOfHashFunctions(approximateNumberOfElements,
      requiredNumberOfBits);

    SimpleBloomFilter<T> bloomFilter = new SimpleBloomFilter<>(requiredNumberOfBits, optimalNumberOfHashFunctions);

    bloomFilter.falsePositiveRate = acceptableFalsePositiveRate;

    return bloomFilter;
  }

  /**
   * Computes the required number of bits needed by the Bloom Filter as a factor of the approximate number of elements
   * to be added to the filter along with the desired, acceptable false positive rate (probability).
   * <p>
   * {@literal m = n * (log p) / (log 2) ^ 2}
   * <p>
   * m is the required number of bits needed for the Bloom Filter.
   * n is the approximate number of elements to be added to the bloom filter
   * p is the acceptable false positive rate between 0.0 and 1.0 exclusive
   *
   * @param approximateNumberOfElements integer value indicating the approximate, estimated number of elements
   * the user expects will be added to the Bloom Filter.
   * @param acceptableFalsePositiveRate a floating point value indicating the acceptable percentage of false positives
   * returned by the Bloom Filter.
   * @return the required number of bits needed by the Bloom Filter.
   */
  protected static int computeRequiredNumberOfBits(double approximateNumberOfElements,
      double acceptableFalsePositiveRate) {

    double numberOfBits = Math.abs((approximateNumberOfElements * Math.log(acceptableFalsePositiveRate))
      / Math.pow(Math.log(2.0d), 2.0d));

    return Double.valueOf(Math.ceil(numberOfBits)).intValue();
  }

  /**
   * Computes the optimal number of hash functions to apply to each element added to the Bloom Filter as a factor
   * of the approximate (estimated) number of elements that will be added to the filter along with
   * the required number of bits needed by the filter, which was computed from the probability of false positives.
   * <p>
   * {@literal k = m/n * log 2}
   * <p>
   * m is the required number of bits needed for the bloom filter
   * n is the approximate number of elements to be added to the bloom filter
   * k is the optimal number of hash functions to apply to the element added to the bloom filter
   *
   * @param approximateNumberOfElements integer value indicating the approximate, estimated number of elements
   * the user expects will be added to the Bloom Filter.
   * @param requiredNumberOfBits the required number of bits needed by the Bloom Filter to satisfy the probability
   * of false positives.
   * @return the optimal number of hash functions used by the Bloom Filter.
   */
  protected static int computeOptimalNumberOfHashFunctions(double approximateNumberOfElements,
      double requiredNumberOfBits) {

    double numberOfHashFunctions = (requiredNumberOfBits / approximateNumberOfElements) * Math.log(2.0d);

    return Double.valueOf(Math.ceil(numberOfHashFunctions)).intValue();
  }

  /**
   * Constructs a new {@link SimpleBloomFilter} with the default number of bits and default number of hash functions.
   *
   * @see #SimpleBloomFilter(int, int)
   */
  public SimpleBloomFilter() {
    this(DEFAULT_NUMBER_OF_BITS, DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
  }

  /**
   * Constructs a new {@link SimpleBloomFilter} initialized with the required number of bits
   * and optimal number of hash functions.
   *
   * @param numberOfBits the number of bits needed by this filter.
   * @param numberOfHashFunctions the number of hash functions applied to each element when added to the set
   * or evaluated against the filter.
   * @throws IllegalArgumentException if either {@code numberOfBits} or {@code numberOfHashFunctions}
   * is less than equal to 0.
   */
  public SimpleBloomFilter(int numberOfBits, int numberOfHashFunctions) {

    Assert.isTrue(numberOfBits > 0, "Number of bits [%d] must be greater than 0", numberOfBits);

    Assert.isTrue(numberOfHashFunctions > 0,
      "Number of hash functions [%d] must be greater than 0", numberOfHashFunctions);

    this.bitArray = new int[getBitArrayLength(numberOfBits)];
    this.hashFunctionCount = numberOfHashFunctions;

    Arrays.fill(this.bitArray, 0);
  }

  /**
   * Returns a reference to the bit array used as the filter in this {@link BloomFilter}.
   *
   * @return a reference to the bit array used as the filter in this {@link BloomFilter}.
   */
  int[] getBitArray() {
    return Arrays.copyOf(this.bitArray, this.bitArray.length);
  }

  /**
   * Determines the length of the bit array used as a filter in this {@link BloomFilter}, which is based on
   * the given number of bits required by this {@link BloomFilter}.
   * <p>
   * WARNING: This method may be overridden in subclasses but should be careful not to rely on any state
   * in this {@link BloomFilter} class since this method is called from the constructor.
   *
   * @param requiredNumberOfBits integer value indicating the number of bits needed by this {@link BloomFilter}.
   * @return an integer value with the length of the bit array required to adequately contain the number of bits
   * needed by this {@link BloomFilter}.
   * @see #getFilterSize()
   */
  protected int getBitArrayLength(int requiredNumberOfBits) {

    int remainder = requiredNumberOfBits % THIRTY_TWO_BITS;
    int additionalNumberOfBits = (remainder != 0 ? THIRTY_TWO_BITS - remainder : 0);

    return (requiredNumberOfBits + additionalNumberOfBits) / THIRTY_TWO_BITS;
  }

  /**
   * Returns the acceptable and probable, false positive rate allowed by this {@link BloomFilter}.
   *
   * @return a float value with the acceptable and probably false positive rate allowed by this {@link BloomFilter}.
   */
  public float getFalsePositiveRate() {
    return this.falsePositiveRate;
  }

  /**
   * Returns the number of bits used in this {@link BloomFilter}.
   *
   * @return an integer value with the number of bits used this {@link BloomFilter}.
   * @see #getBitArrayLength(int)
   * @see #getBitArray()
   */
  protected int getFilterSize() {
    return getBitArray().length * THIRTY_TWO_BITS;
  }

  /**
   * Determines the number of bits to set in this {@link BloomFilter} given the specified {@link Object element} to add.
   *
   * @param element {@link Object element} used as a basis for the number of bits to set in this {@link BloomFilter}.
   * @return an integer value with the number of bits to set for the given {@link Object element} being added to
   * this {@link BloomFilter}.
   */
  protected int getHashFunctionCount(T element) {
    return this.hashFunctionCount;
  }

  /**
   * Determines whether the given element is a member of the set contained by this {@link BloomFilter}.
   *
   * @param element {@link Object element} to evaluate.
   * @return a boolean value indicating whether the given element is a member of the set
   * contained by this {@link BloomFilter}.
   * @see java.lang.Object#hashCode()
   * @see #getHashFunctionCount(Object)
   * @see #add(Object)
   */
  @Override
  public synchronized boolean accept(T element) {

    boolean accepted = (element != null);

    if (accepted) {

      int filterSize = getFilterSize();
      int hashFunctionCount = getHashFunctionCount(element);

      this.random.setSeed(element.hashCode());

      for (int count = 0; accepted && count < hashFunctionCount; count++) {
        int bitIndex = this.random.nextInt(filterSize);
        accepted = ((this.bitArray[bitIndex / THIRTY_TWO_BITS] & BIT_MASKS[bitIndex % THIRTY_TWO_BITS]) != 0);
      }
    }

    return accepted;
  }

  /**
   * Adds the given element to the set managed by this {@link BloomFilter}.
   *
   * @param element {@link Object element} to add to this {@link BloomFilter}.
   * @see #accept(Object)
   */
  public synchronized void add(T element) {

    Assert.notNull(element, "Element cannot be null");

    int filterSize = getFilterSize();
    int hashFunctionCount = getHashFunctionCount(element);

    this.random.setSeed(element.hashCode());

    for (int count = 0; count < hashFunctionCount; count++) {
      int bitIndex = this.random.nextInt(filterSize);
      this.bitArray[bitIndex / THIRTY_TWO_BITS] |= BIT_MASKS[bitIndex % THIRTY_TWO_BITS];
    }
  }

  /**
   * Determines the approximate, estimated size of this {@link BloomFilter}.
   * <p>
   * The estimated size is approximately the number of elements that have been added to this {@link BloomFilter},
   * calculated as...
   * <p>
   * {@literal n* = - m/n * log(1 - X/m)}
   * <p>
   * n* is an estimate of the number of elements in this filter
   * m is the length (size) of this filter
   * k is the number of hash functions
   * X is the number of bits set to one
   *
   * @return an integer value with the size of this {@link BloomFilter} indicated as the estimated number of elements
   * that have possibly been added to this {@link BloomFilter}.
   */
  public int size() {

    double filterSize = getFilterSize(); // m
    double numberOfBitsSetToOne = countNumberOfBitsSetToOne(); // X
    double numberOfHashFunctions = getHashFunctionCount(null); // k
    double estimatedSize = (filterSize / numberOfHashFunctions) * Math.log(1 - (numberOfBitsSetToOne / filterSize));

    return Double.valueOf(Math.abs(Math.round(estimatedSize))).intValue();
  }

  private int countNumberOfBitsSetToOne() {

    AtomicInteger numberOfBitsSetToOne = new AtomicInteger(0);

    stream(getBitArray()).forEach(bucket -> {
      for (int bitMask : BIT_MASKS) {
        if ((bucket & bitMask) != 0) {
          numberOfBitsSetToOne.incrementAndGet();
        }
      }
    });

    return numberOfBitsSetToOne.get();
  }
}
