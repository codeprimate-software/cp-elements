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
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.util.Objects;
import java.util.Random;
import java.util.stream.Stream;

import org.junit.Test;

/**
 * The ScalableBloomFilterTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class ScalableBloomFilterTests {

  protected static final int NUMBER_COUNT = 500000;

  @SuppressWarnings("unused)")
  private void log(String message, Object... args) {
    System.err.printf(message, args);
    System.err.flush();
  }

  @Test
  public void constructDefaultScalableBloomFilter() {

    ScalableBloomFilter<Object> bloomFilter = new ScalableBloomFilter<>();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getAcceptableFalsePositiveRate())
      .isEqualTo(ScalableBloomFilter.DEFAULT_ACCEPTABLE_FALSE_POSITIVE_RATE);
    assertThat(bloomFilter.getApproximateNumberOfElementsPerFilter())
      .isEqualTo(ScalableBloomFilter.DEFAULT_NUMBER_OF_ELEMENTS_PER_FILTER);
    assertThat(bloomFilter.getBloomFilters()).isNotNull();
    assertThat(stream(bloomFilter).filter(Objects::isNull).count()).isEqualTo(ScalableBloomFilter.DEFAULT_SCALE);
    assertThat(bloomFilter.getScale()).isEqualTo(ScalableBloomFilter.DEFAULT_SCALE);
  }

  @Test
  public void constructCustomizedScalableBloomFilter() {

    ScalableBloomFilter<Object> bloomFilter = new ScalableBloomFilter<>(32)
      .with(0.1f).with(256000);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getAcceptableFalsePositiveRate()).isEqualTo(0.1f);
    assertThat(bloomFilter.getApproximateNumberOfElementsPerFilter()).isEqualTo(256000);
    assertThat(bloomFilter.getBloomFilters()).isNotNull();
    assertThat(stream(bloomFilter).filter(Objects::isNull).count()).isEqualTo(32);
    assertThat(bloomFilter.getScale()).isEqualTo(32);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructBloomFilterWithIllegalScale() {

    try {
      new ScalableBloomFilter(-128);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Scale [-128] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void ofWithIllegalScaleThrowsIllegalArgumentException() {

    try {
      ScalableBloomFilter.of(-64);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Scale [-64] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void ofReturnsScalableBloomFilterInitializedWithTheGivenScale() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.of(16);

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getScale()).isEqualTo(16);
  }

  @Test
  public void ofOneReturnsScalableBloomFilterWithSingleBloomFilter() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.getScale()).isEqualTo(1);
  }

  @Test
  public void addsAndAcceptsRandomNumbers() {

    ScalableBloomFilter<Integer> bloomFilter = new ScalableBloomFilter<>();

    Random random = new Random(System.currentTimeMillis());

    synchronized (bloomFilter) {
      for (int count = 0; count < NUMBER_COUNT; count++) {

        int number = Math.abs(random.nextInt());

        bloomFilter.add(number);

        assertThat(bloomFilter.accept(number))
          .describedAs("Number [%d] was added but not accepted", number)
          .isTrue();
      }
    }

    //log("Non-null BloomFilter Count is %d%n", stream(bloomFilter).filter(Objects::nonNull).count());

    assertThat(bloomFilter.size()).isGreaterThanOrEqualTo(NUMBER_COUNT - (int) (NUMBER_COUNT * 0.01d));
  }

  @Test
  public void iteratesOverAggregatedBloomFilters() {

    ScalableBloomFilter<Integer> aggregateBloomFilter = ScalableBloomFilter.of(9);

    Stream.of(1, 2, 3, 4, 5, 6, 7, 8, 9).forEach(aggregateBloomFilter::add);

    for (BloomFilter<Integer> bloomFilter : aggregateBloomFilter) {
      assertThat(bloomFilter).isNotNull();
      assertThat(bloomFilter.size()).isEqualTo(1);
    }

    assertThat(aggregateBloomFilter.size()).isEqualTo(9);
  }

  @Test
  public void newBloomFilterReturnsNewBloomFilter() {

    BloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne()
      .newBloomFilter(2, 0.1f);

    assertThat(bloomFilter).isInstanceOf(SimpleBloomFilter.class);
    assertThat(((SimpleBloomFilter) bloomFilter).getFalsePositiveRate()).isEqualTo(0.1f);
  }

  @Test
  public void resolveBloomFilterWithIndexReturnsBloomFilter() {

    ScalableBloomFilter<Object> aggregateBloomFilter = ScalableBloomFilter.of(2);

    assertThat(aggregateBloomFilter).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters()).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters().length).isEqualTo(2);
    assertThat(aggregateBloomFilter.getScale()).isEqualTo(2);
    assertThat(aggregateBloomFilter.getBloomFilters()[0]).isNull();
    assertThat(aggregateBloomFilter.getBloomFilters()[1]).isNull();

    BloomFilter<Object> bloomFilter = aggregateBloomFilter.resolveBloomFilter(0);

    assertThat(bloomFilter).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters()[0]).isSameAs(bloomFilter);
    assertThat(aggregateBloomFilter.getBloomFilters()[1]).isNull();
    assertThat(aggregateBloomFilter.resolveBloomFilter(0)).isSameAs(bloomFilter);
  }

  @Test
  public void withAcceptableFalsePositiveRate() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.with(0.50f)).isSameAs(bloomFilter);
    assertThat(bloomFilter.getAcceptableFalsePositiveRate()).isEqualTo(0.50f);
  }

  @Test(expected = IllegalArgumentException.class)
  public void withIllegalAcceptableFalsePositiveRate() {

    try {
      ScalableBloomFilter.ofOne().with(-0.05f);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The acceptable false positive rate [-0.05] must be greater than 0.0 and less than 1.0");
      assertThat(expected).hasNoCause();

      throw expected;

    }
  }

  @Test
  public void withApproximateNumberOfElementsPerFilter() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat(bloomFilter).isNotNull();
    assertThat(bloomFilter.with(128000)).isSameAs(bloomFilter);
    assertThat(bloomFilter.getApproximateNumberOfElementsPerFilter()).isEqualTo(128000);
  }

  @Test(expected = IllegalArgumentException.class)
  public void withIllegalApproximateNumberOfElementsPerFilter() {

    try {
      ScalableBloomFilter.ofOne().with(-512000);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The approximate number of elements [-512000] per Bloom Filter must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;

    }
  }
}
