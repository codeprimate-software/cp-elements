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
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.util.Objects;
import java.util.Random;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ScalableBloomFilter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.struct.ScalableBloomFilter
 * @since 1.0.0
 */
public class ScalableBloomFilterTests {

  protected static final int NUMBER_COUNT =
    SystemPropertyValue.newSystemPropertyValue("cp.elements.data.struct.ScalableBloomFilter.number-count")
      .getValueAs(Integer.class, 10000); // 500000;

  @SuppressWarnings("unused")
  private void log(String message, Object... args) {
    System.err.printf(message, args);
    System.err.flush();
  }

  @Test
  public void constructDefaultScalableBloomFilter() {

    ScalableBloomFilter<Object> bloomFilter = new ScalableBloomFilter<>();

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
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

    ScalableBloomFilter<Object> bloomFilter = new ScalableBloomFilter<>(16)
      .with(0.1f).with(10000);

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
    assertThat(bloomFilter.getAcceptableFalsePositiveRate()).isEqualTo(0.1f);
    assertThat(bloomFilter.getApproximateNumberOfElementsPerFilter()).isEqualTo(10000);
    assertThat(bloomFilter.getBloomFilters()).isNotNull();
    assertThat(stream(bloomFilter).filter(Objects::isNull).count()).isEqualTo(16);
    assertThat(bloomFilter.getScale()).isEqualTo(16);
  }

  @Test
  public void constructBloomFilterWithIllegalScale() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ScalableBloomFilter<>(-128))
      .withMessage("Scale [-128] must be greater than 0")
      .withNoCause();
  }

  @Test
  public void ofWithIllegalScaleThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ScalableBloomFilter.of(-64))
      .withMessage("Scale [-64] must be greater than 0")
      .withNoCause();
  }

  @Test
  public void ofReturnsScalableBloomFilterInitializedWithTheGivenScale() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.of(8);

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
    assertThat(bloomFilter.getScale()).isEqualTo(8);
  }

  @Test
  public void ofOneReturnsScalableBloomFilterWithSingleBloomFilter() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
    assertThat(bloomFilter.getScale()).isEqualTo(1);
  }

  @Test
  public void addsAndAcceptsRandomNumbers() {

    ScalableBloomFilter<Integer> bloomFilter = ScalableBloomFilter.<Integer>of(2).with(NUMBER_COUNT);

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

    ScalableBloomFilter<Integer> aggregateBloomFilter = ScalableBloomFilter.<Integer>of(9).with(1);

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
      .newBloomFilter(1, 0.1f);

    assertThat(bloomFilter).isInstanceOf(SimpleBloomFilter.class);
    assertThat(((SimpleBloomFilter<?>) bloomFilter).getFalsePositiveRate()).isEqualTo(0.1f);
    assertThat(((SimpleBloomFilter<?>) bloomFilter).getFilterSize()).isEqualTo(32);
    assertThat(((SimpleBloomFilter<?>) bloomFilter).getHashFunctionCount(null))
      .isLessThan(SimpleBloomFilter.DEFAULT_NUMBER_OF_HASH_FUNCTIONS);
  }

  @Test
  public void resolveBloomFilterWithIndexReturnsBloomFilter() {

    ScalableBloomFilter<Object> aggregateBloomFilter = ScalableBloomFilter.of(2).with(1);

    assertThat((Predicate<Object>) aggregateBloomFilter).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters()).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters().length()).isEqualTo(2);
    assertThat(aggregateBloomFilter.getScale()).isEqualTo(2);
    assertThat(aggregateBloomFilter.getBloomFilters().get(0)).isNull();
    assertThat(aggregateBloomFilter.getBloomFilters().get(1)).isNull();

    BloomFilter<Object> bloomFilter = aggregateBloomFilter.resolveBloomFilter(0);

    assertThat(bloomFilter).isNotNull();
    assertThat(aggregateBloomFilter.getBloomFilters().get(0)).isSameAs(bloomFilter);
    assertThat(aggregateBloomFilter.getBloomFilters().get(1)).isNull();
    assertThat(aggregateBloomFilter.resolveBloomFilter(0)).isSameAs(bloomFilter);
  }

  @Test
  public void withAcceptableFalsePositiveRate() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
    assertThat((Predicate<Object>) bloomFilter.with(0.50f)).isSameAs(bloomFilter);
    assertThat(bloomFilter.getAcceptableFalsePositiveRate()).isEqualTo(0.50f);
  }

  @Test
  public void withIllegalAcceptableFalsePositiveRate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ScalableBloomFilter.ofOne().with(-0.05f))
      .withMessage("The acceptable false positive rate [-0.05] must be greater than 0.0 and less than 1.0")
      .withNoCause();
  }

  @Test
  public void withApproximateNumberOfElementsPerFilter() {

    ScalableBloomFilter<Object> bloomFilter = ScalableBloomFilter.ofOne();

    assertThat((Predicate<Object>) bloomFilter).isNotNull();
    assertThat((Predicate<Object>) bloomFilter.with(128000)).isSameAs(bloomFilter);
    assertThat(bloomFilter.getApproximateNumberOfElementsPerFilter()).isEqualTo(128000);
  }

  @Test
  public void withIllegalApproximateNumberOfElementsPerFilter() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ScalableBloomFilter.ofOne().with(-512000))
      .withMessage("The approximate number of elements [-512000] per Bloom Filter must be greater than 0")
      .withNoCause();
  }
}
