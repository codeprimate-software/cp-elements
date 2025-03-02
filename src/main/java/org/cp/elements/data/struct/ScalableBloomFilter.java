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

import static org.cp.elements.util.stream.StreamUtils.stream;

import java.util.Iterator;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.Array;

/**
 * Implementation of {@link BloomFilter} that is scalable at runtime.
 * <p>
 * The objective of this {@link BloomFilter} implementation is to save space (memory) and improve on reliability
 * with precision accuracy by minimizing saturation when managing larger data sets, which leads to more false positives,
 * particularly as the data set grows, which is definitely the case with event streams.
 * <p>
 * In a world of Big Data, technically implying an endless supply of data and an unlimited number of data sources,
 * the data set is only going to get bigger.  Therefore, approximating/estimating the number of data elements
 * in advance will only become increasingly more difficult.  It is largely already in-deterministic given continuous
 * streams of data inherit in the Internet of Things (e.g. event sources).
 * <p>
 * Therefore, being able to reliably determine the presence of a data element in a set with accuracy and efficiency
 * is paramount to reasoning about data and performing meaningful actions with the data given/provided.
 * <p>
 * This {@link BloomFilter} introduces another dimension by serving as a facade for a collection (aggregation) of
 * {@link BloomFilter Bloom Filters} over which the managed set of elements can be amortized.
 * <p>
 * To add and store 1 billion data elements with a 1% false positive rate, the {@link BloomFilter} would need ~1.2 GB
 * of space.  That is expensive in terms of memory!  It would be an improvement if the cost could be amortized
 * over time as the need for more space grows.
 * <p>
 * This implementation is still pretty crude and needs improvement.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the elements added to this {@link BloomFilter}.
 * @see java.lang.Iterable
 * @see org.cp.elements.data.struct.BloomFilter
 * @see org.cp.elements.data.struct.SimpleBloomFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ScalableBloomFilter<T> implements BloomFilter<T>, Iterable<BloomFilter<T>> {

  protected static final float DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE =
    SimpleBloomFilter.DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE;

  protected static final int DEFAULT_SCALE = 64;
  protected static final int DEFAULT_NUMBER_OF_ELEMENTS_PER_FILTER = 10000000;

  /**
   * Factory method used to construct a new {@link ScalableBloomFilter} with a single {@link BloomFilter}.
   *
   * @param <T> {@link Class type} of elements added to this {@link BloomFilter}.
   * @return a new instance of {@link ScalableBloomFilter} with a single {@link BloomFilter}.
   * @see #of(int)
   */
  public static <T> ScalableBloomFilter<T> ofOne() {
    return of(1);
  }

  /**
   * Factory method used to construct an instance of {@link ScalableBloomFilter} initialized with the given scale.
   *
   * @param <T> {@link Class type} of elements added to this {@link BloomFilter}.
   * @param scale integer value indicating the maximum number of {@link BloomFilter Bloom Filters} managed by
   * this {@link ScalableBloomFilter}.
   * @return a new instance of {@link ScalableBloomFilter} initialized with the given scale.
   * @throws IllegalArgumentException if scale is less than equal to 0.
   * @see #ScalableBloomFilter(int)
   */
  public static <T> ScalableBloomFilter<T> of(int scale) {
    return new ScalableBloomFilter<>(scale);
  }

  private volatile float acceptableFalsePositiveRate = DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE;

  private volatile int approximateNumberOfElementsPerFilter = DEFAULT_NUMBER_OF_ELEMENTS_PER_FILTER;

  private final BloomFilter<T>[] bloomFilters;

  /**
   * Constructs a new {@link ScalableBloomFilter} initialized with the default scale.
   *
   * @see #ScalableBloomFilter(int)
   */
  public ScalableBloomFilter() {
    this(DEFAULT_SCALE);
  }

  /**
   * Constructs a new {@link ScalableBloomFilter} initialized with the given scale.
   *
   * @param scale integer value indicating the maximum number of {@link BloomFilter Bloom Filters} managed by
   * this {@link ScalableBloomFilter}.
   * @throws IllegalArgumentException if scale is less than equal to 0.
   */
  @SuppressWarnings("unchecked")
  public ScalableBloomFilter(int scale) {

    Assert.isTrue(scale > 0, "Scale [%d] must be greater than 0", scale);

    this.bloomFilters = new BloomFilter[scale];
  }

  /**
   * Returns the configured acceptable, false positive rate allowed by this aggregate {@link BloomFilter}.
   * <p>
   * Defaults to 1% (or 0.01).
   *
   * @return the configured acceptable, false positive rate allowed by this aggregate {@link BloomFilter}.
   */
  public float getAcceptableFalsePositiveRate() {
    return this.acceptableFalsePositiveRate;
  }

  /**
   * Returns the configured approximate (estimated) number of elements that will be added to each
   * of the {@link BloomFilter Bloom Filters} managed by this {@link ScalableBloomFilter}.
   * <p>
   * Defaults to 10,000,000 elements.
   *
   * @return the configured approximate (estimated) number of elements that will be added to each
   * of the {@link BloomFilter Bloom Filters} managed by this {@link ScalableBloomFilter}.
   */
  public int getApproximateNumberOfElementsPerFilter() {
    return this.approximateNumberOfElementsPerFilter;
  }

  /**
   * Returns a reference to the collection of {@link BloomFilter Bloom Filters} managed by
   * this {@link ScalableBloomFilter}.
   *
   * @return a reference to the collection of {@link BloomFilter Bloom Filters} managed by
   * this {@link ScalableBloomFilter}.
   * @see org.cp.elements.data.struct.BloomFilter
   */
  @NullSafe
  Array<BloomFilter<T>> getBloomFilters() {
    return Array.immutableOf(this.bloomFilters);
  }

  /**
   * Returns the maximum number of {@link BloomFilter Bloom Filters} managed by this {@link ScalableBloomFilter}
   * at runtime.
   *
   * @return an integer value with the maximum number of {@link BloomFilter Bloom Filters} managed by
   * this {@link ScalableBloomFilter} at runtime.
   * @see #getBloomFilters()
   */
  public int getScale() {
    return getBloomFilters().length();
  }

  /**
   * Determines whether the given element is a member of the set contained by this {@link BloomFilter}.
   *
   * @param element {@link Object element} to evaluate.
   * @return a boolean value indicating whether the given element is a member of the set
   * contained by this {@link BloomFilter}.
   * @see #add(Object)
   */
  @Override
  @NullSafe
  public synchronized boolean accept(T element) {

    Array<BloomFilter<T>> bloomFilters = getBloomFilters();

    return Optional.ofNullable(element)
      .map(Object::hashCode)
      .map(hashCode -> hashCode % getScale())
      .map(bloomFilters::get)
      .map(bloomFilter -> bloomFilter.accept(element))
      .orElse(false);
  }

  /**
   * Adds the given element to the set managed by this {@link BloomFilter}.
   *
   * @param element {@link Object element} to add to this {@link BloomFilter}.
   * @see #accept(Object)
   */
  @NullSafe
  @Override
  public synchronized void add(T element) {

    Optional.ofNullable(element)
      .ifPresent(it -> {

        int hashCode = it.hashCode();
        int index = (hashCode % getScale());

        BloomFilter<T> bloomFilter = resolveBloomFilter(index);

        bloomFilter.add(it);
      });
  }

  /**
   * Returns an {@link Iterator} over the {@link BloomFilter Bloom Filters} aggregated by
   * this {@link ScalableBloomFilter}.
   *
   * @return an {@link Iterator} over the {@link BloomFilter Bloom Filters} aggregated by
   * this {@link ScalableBloomFilter}.
   * @see org.cp.elements.data.struct.BloomFilter
   * @see java.util.Iterator
   */
  @Override
  @SuppressWarnings("all")
  public Iterator<BloomFilter<T>> iterator() {
    return getBloomFilters().iterator();
  }

  /**
   * Returns the approximate, estimated number of data elements managed by this {@link BloomFilter}.
   *
   * @return the approximate, estimated number of data elements managed by this {@link BloomFilter}.
   */
  public int size() {
    return stream(this).mapToInt(BloomFilter::size).sum();
  }

  /**
   * Constructs a new {@link BloomFilter} initialized with the given approximate, estimated number of elements
   * and acceptable false positive rate.
   *
   * @param approximateNumberOfElements integer value indicating the approximate, estimated number of elements
   * the user expects will be added to the returned {@link BloomFilter} instance.
   * @param acceptableFalsePositiveRate a floating point value indicating the acceptable percentage of false positives
   * returned by the constructed {@link BloomFilter}.
   * @return a new instance of {@link BloomFilter}.
   * @throws IllegalArgumentException if the {@code approximateNumberOfElements} is less than equal to 0
   * or the {@code acceptableFalsePositiveRate} is less than equal to 0.0 or greater than equal to 1.0.
   * @see org.cp.elements.data.struct.BloomFilter
   */
  @NullSafe
  protected BloomFilter<T> newBloomFilter(int approximateNumberOfElements, float acceptableFalsePositiveRate) {
    return SimpleBloomFilter.of(approximateNumberOfElements, acceptableFalsePositiveRate);
  }

  /**
   * Resolves the actual {@link BloomFilter} at index used to add or evaluate an element.
   *
   * @param index integer value representing the index of the {@link BloomFilter} to retrieve.
   * @return the {@link BloomFilter} at index.  If the {@link BloomFilter} at index is {@literal null},
   * then this method constructs a new {@link BloomFilter} and sets a reference to it at index.
   * @see #getApproximateNumberOfElementsPerFilter()
   * @see #getAcceptableFalsePositiveRate()
   * @see #newBloomFilter(int, float)
   */
  @NullSafe
  protected BloomFilter<T> resolveBloomFilter(int index) {

    BloomFilter<T> bloomFilter = this.bloomFilters[index];

    if (bloomFilter == null) {
      bloomFilter = newBloomFilter(getApproximateNumberOfElementsPerFilter(), getAcceptableFalsePositiveRate());
      this.bloomFilters[index] = bloomFilter;
    }

    return bloomFilter;
  }

  /**
   * Configures this {@link BloomFilter} with the desired, acceptable false positive rate.
   *
   * @param acceptableFalsePositiveRate a floating point value indicating the acceptable percentage of false positives
   * returned by this {@link BloomFilter}.
   * @return this {@link ScalableBloomFilter}.
   * @throws IllegalArgumentException if the {@code acceptableFalsePositiveRate} is less than equal to 0.0
   * or greater than equal to 1.0.
   */
  public ScalableBloomFilter<T> with(float acceptableFalsePositiveRate) {

    Assert.isTrue(acceptableFalsePositiveRate > 0.0f && acceptableFalsePositiveRate < 1.0f,
      "The acceptable false positive rate [%s] must be greater than 0.0 and less than 1.0",
        String.valueOf(acceptableFalsePositiveRate));

    this.acceptableFalsePositiveRate = acceptableFalsePositiveRate;

    return this;
  }

  /**
   * Configures this {@link BloomFilter} with the approximate number of elements per filter.
   *
   * @param approximateNumberOfElementsPerFilter integer value indicating the approximate, estimated number of elements
   * the user expects will be added to each {@link BloomFilter}.
   * @return this {@link ScalableBloomFilter}.
   * @throws IllegalArgumentException if the {@code approximateNumberOfElementsPerFilter} is less than equal to 0.
   */
  public ScalableBloomFilter<T> with(int approximateNumberOfElementsPerFilter) {

    Assert.isTrue(approximateNumberOfElementsPerFilter > 0,
      "The approximate number of elements [%d] per Bloom Filter must be greater than 0",
        approximateNumberOfElementsPerFilter);

    this.approximateNumberOfElementsPerFilter = approximateNumberOfElementsPerFilter;

    return this;
  }
}
