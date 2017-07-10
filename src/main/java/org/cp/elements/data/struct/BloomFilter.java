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

import static org.cp.elements.lang.NumberUtils.isByte;
import static org.cp.elements.lang.NumberUtils.isDecimal;
import static org.cp.elements.lang.NumberUtils.isFloat;
import static org.cp.elements.lang.NumberUtils.isInteger;
import static org.cp.elements.lang.NumberUtils.isShort;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Random;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.annotation.Experimental;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The {@link BloomFilter} class is a probabilistic data structure to test whether a data element is a member of a set.
 *
 * @author John J. Blum
 * @see java.lang.Number
 * @see java.util.Random
 * @see org.cp.elements.lang.Filter
 * @see <a href="https://en.wikipedia.org/wiki/Bloom_filter">Bloom Filter</a>
 * @since 1.0.0
 */
@ThreadSafe
@Experimental
@SuppressWarnings("unused")
public class BloomFilter<T extends Number> implements Filter<T> {

  protected static final int DEFAULT_BIT_ARRAY_SIZE = 16384;
  protected static final int DEFAULT_BIT_COUNT = 32;

  protected static final int[] BIT_MASKS = new int[32];

  protected static final Map<Class, Integer> BIT_COUNT_MAP;

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

  static {

    Map<Class, Integer> classToBitSize = new HashMap<>();

    classToBitSize.put(Double.class, 64);
    classToBitSize.put(Long.class, 64);
    classToBitSize.put(Float.class, 32);
    classToBitSize.put(Integer.class, 32);
    classToBitSize.put(Short.class, 16);
    classToBitSize.put(Byte.class, 8);
    classToBitSize.put(null, DEFAULT_BIT_COUNT);

    BIT_COUNT_MAP = Collections.unmodifiableMap(classToBitSize);
  }

  private final int[] bitArray;

  private final Random random = new Random();

  private final TypeResolver typeResolver = new SmartTypeResolver();

  /**
   * Constructs an instance of the {@link BloomFilter} class with the default bit array size.
   */
  public BloomFilter() {
    this(DEFAULT_BIT_ARRAY_SIZE);
  }

  /**
   * Constructs an instance of the {@link BloomFilter} class initialized with the specified bit array size.
   *
   * @param size the size of the bit array used as a filter.
   * @throws IllegalArgumentException if size is less than equal to 0.
   */
  public BloomFilter(int size) {

    Assert.isTrue(size > 0, "Size [%d] must be greater than 0", size);

    this.bitArray = new int[size];

    Arrays.fill(this.bitArray, 0);
  }

  /**
   * Gets a reference to the bit array used as the filter in this {@link BloomFilter}.
   *
   * @return a reference to the bit array used as the filter in this {@link BloomFilter}.
   */
  int[] getBitArray() {
    return this.bitArray;
  }

  /**
   * Determines the number of bits to set in this filter based on the number's class type.
   *
   * @param number the number who's class type will determine the number of bits to set in this filter.
   * @return an integer value indicating the number of bits to set in this filter based on the number's class type.
   */
  protected int getBitCount(Number number) {
    return Optional.ofNullable(number).map(it -> getTypeResolver().resolveType(it)).map(BIT_COUNT_MAP::get)
      .orElse(DEFAULT_BIT_COUNT);
  }

  /**
   * Returns the total number of bits in this filter.  The number of bits is used as an upper bound
   * during random number generation.
   *
   * @param number the number being added or evaluated by this filter.
   * @return an integer value indicating the total number of bits in this filter.
   */
  protected int getBound(T number) {
    return (getBitArray().length * 32);
  }

  /**
   * Returns the {@link TypeResolver} used to determine the type of an object.
   *
   * @return the {@link TypeResolver} used to determine the type of an object.
   * @see org.cp.elements.data.struct.BloomFilter.TypeResolver
   */
  protected TypeResolver getTypeResolver() {
    return this.typeResolver;
  }

  /**
   * Determines whether the given {@link Number} is a member of this filter set.
   *
   * @param number {@link Number} being evaluated.
   * @return a boolean value indicating whether the given {@link Number} is a member of this filer set.
   * @see #add(Number)
   */
  @Override
  public synchronized boolean accept(T number) {

    boolean accepted = (number != null);

    if (accepted) {

      int bitCount = getBitCount(number);
      int bound = getBound(number);

      this.random.setSeed(number.longValue());

      for (int count = 0; accepted && count < bitCount; count++) {
        int bitIndex = this.random.nextInt(bound);
        accepted = ((this.bitArray[bitIndex / 32] & BIT_MASKS[bitIndex % 32]) != 0);
      }
    }

    return accepted;
  }

  /**
   * Adds the given number to the set of numbers tracked by this filter.
   *
   * @param number the number to add to the set of numbers tracked by this filter.
   * @see #accept(Number)
   */
  public synchronized void add(T number) {

    Assert.notNull(number, "Number cannot be null");

    int bitCount = getBitCount(number);
    int bound = getBound(number);

    this.random.setSeed(number.longValue());

    for (int count = 0; count < bitCount; count++) {
      int bitIndex = this.random.nextInt(bound);
      this.bitArray[bitIndex / 32] |= BIT_MASKS[bitIndex % 32];
    }
  }

  /**
   * {@link TypeResolver} is a strategy interface that determines the {@link Class type} of an {@link Object}.
   */
  public interface TypeResolver {
    Class resolveType(Object obj);
  }

  /**
   * {@link SimpleTypeResolver} is a {@link TypeResolver} implementation that determines the {@link Class type}
   * of an {@link Object} a null-safe implementation of {@link Object#getClass()}.
   *
   * @see org.cp.elements.data.struct.BloomFilter.TypeResolver
   * @see org.cp.elements.lang.ClassUtils#getClass(Object)
   */
  public static class SimpleTypeResolver implements TypeResolver {

    /* (non-Javadoc) */
    @Override
    public Class resolveType(Object obj) {
      return ClassUtils.getClass(obj);
    }
  }

  /**
   * {@link SmartTypeResolver} is an extension of {@link SimpleTypeResolver} and an implementation
   * of the {@link TypeResolver} interface that compresses the {@link Class type} of a {@link Number}
   * based on its value.
   *
   * @see org.cp.elements.data.struct.BloomFilter.SimpleTypeResolver
   */
  public static class SmartTypeResolver extends SimpleTypeResolver {

    /* (non-Javadoc) */
    @Override
    public Class resolveType(Object obj) {

      if (obj instanceof Number) {

        Number value = (Number) obj;

        if (isDecimal(value)) {
          return (isFloat(value) ? Float.class : Double.class);
        }
        else {
          return (isByte(value) ? Byte.class
            : (isShort(value) ? Short.class
            : (isInteger(value) ? Integer.class : Long.class)));
        }
      }

      return super.resolveType(obj);
    }
  }
}
