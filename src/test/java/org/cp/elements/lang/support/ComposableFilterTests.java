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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.text.MessageFormat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;
import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link ComposableFilter}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.LogicalOperator
 * @see org.cp.elements.lang.support.ComposableFilter
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ComposableFilterTests {

  @Mock(strictness = Strictness.LENIENT)
  private Filter<Object> falseFilter;

  @Mock(strictness = Strictness.LENIENT)
  private Filter<Object> trueFilter;

  @BeforeEach
  public void setup() {
    doReturn(false).when(this.falseFilter).accept(any());
    doReturn(true).when(this.trueFilter).accept(any());
  }

  @Test
  public void acceptUsingAndReturnsTrue() {
    assertThat(ComposableFilter.and(this.trueFilter, this.trueFilter).accept("test")).isTrue();
  }

  @Test
  public void acceptUsingAndReturnsFalse() {

    assertThat(ComposableFilter.and(this.falseFilter, this.falseFilter).accept("test")).isFalse();
    assertThat(ComposableFilter.and(this.falseFilter, this.trueFilter).accept("test")).isFalse();
    assertThat(ComposableFilter.and(this.trueFilter, this.falseFilter).accept("test")).isFalse();
  }

  @Test
  public void acceptUsingOrReturnsTrue() {

    assertThat(ComposableFilter.or(this.falseFilter, this.trueFilter).accept("test")).isTrue();
    assertThat(ComposableFilter.or(this.trueFilter, this.falseFilter).accept("test")).isTrue();
    assertThat(ComposableFilter.or(this.trueFilter, this.trueFilter).accept("test")).isTrue();
  }

  @Test
  public void acceptUsingOrReturnsFalse() {
    assertThat(ComposableFilter.or(this.falseFilter, this.falseFilter).accept("test")).isFalse();
  }

  @Test
  public void acceptUsingXorReturnsTrue() {

    assertThat(ComposableFilter.xor(this.falseFilter, this.trueFilter).accept("test")).isTrue();
    assertThat(ComposableFilter.xor(this.trueFilter, this.falseFilter).accept("test")).isTrue();
  }

  @Test
  public void acceptUsingXorReturnsFalse() {

    assertThat(ComposableFilter.xor(this.falseFilter, this.falseFilter).accept("test")).isFalse();
    assertThat(ComposableFilter.xor(this.trueFilter, this.trueFilter).accept("test")).isFalse();
  }

  @Test
  public void acceptUsingAndWithOr() {

    Filter<Driver> driverFilter = ComposableFilter.or(new DrivingAgeFilter(16),
      ComposableFilter.and(new DrivingAgeFilter(14), new ParentRequiredFilter()));

    assertThat(driverFilter).isInstanceOf(ComposableFilter.class);
    assertThat(driverFilter.accept(new Driver(18, true))).isTrue();
    assertThat(driverFilter.accept(new Driver(18, false))).isTrue();
    assertThat(driverFilter.accept(new Driver(16, true))).isTrue();
    assertThat(driverFilter.accept(new Driver(16, false))).isTrue();
    assertThat(driverFilter.accept(new Driver(15, true))).isTrue();
    assertThat(driverFilter.accept(new Driver(15, false))).isFalse();
    assertThat(driverFilter.accept(new Driver(14, true))).isTrue();
    assertThat(driverFilter.accept(new Driver(14, false))).isFalse();
    assertThat(driverFilter.accept(new Driver(13, true))).isFalse();
    assertThat(driverFilter.accept(new Driver(13, false))).isFalse();
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void composeReturnsCompositeFilter() {

    Filter mockFilterOne = mock(Filter.class);
    Filter mockFilterTwo = mock(Filter.class);

    Filter composite = ComposableFilter.compose(mockFilterOne, LogicalOperator.XOR, mockFilterTwo);

    assertThat(composite).isInstanceOf(ComposableFilter.class);
    assertThat(((ComposableFilter) composite).getFilterOne()).isEqualTo(mockFilterOne);
    assertThat(((ComposableFilter) composite).getFilterTwo()).isEqualTo(mockFilterTwo);
    assertThat(((ComposableFilter) composite).getOp()).isEqualTo(LogicalOperator.XOR);
  }

  @Test
  public void composeReturnsFilter() {

    assertThat(ComposableFilter.compose(this.trueFilter, LogicalOperator.OR, null)).isEqualTo(this.trueFilter);
    assertThat(ComposableFilter.compose(null, LogicalOperator.OR, this.falseFilter)).isEqualTo(this.falseFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void composeReturnsNull() {
    assertThat(ComposableFilter.compose(null, LogicalOperator.AND, null)).isNull();
  }

  @Test
  @SuppressWarnings("all")
  public void composeAndWithNullFiltersReturnsNull() {
    assertThat(ComposableFilter.and(null, null)).isNull();
  }

  @Test
  public void composeAndWithOneFilterReturnsOneFilter() {

    assertThat(ComposableFilter.and(null, this.falseFilter)).isSameAs(this.falseFilter);
    assertThat(ComposableFilter.and(this.trueFilter, null)).isSameAs(this.trueFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void composeAndWithTwoFiltersReturnsCompositeFilter() {

    Filter<Object> composedFilter = ComposableFilter.and(this.trueFilter, this.falseFilter);

    assertThat(composedFilter).isNotNull();
    assertThat(composedFilter).isNotSameAs(this.trueFilter);
    assertThat(composedFilter).isNotSameAs(this.falseFilter);
    assertThat(composedFilter).isInstanceOf(ComposableFilter.class);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterOne()).isSameAs(this.trueFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterTwo()).isSameAs(this.falseFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getOp()).isEqualTo(LogicalOperator.AND);
  }

  @Test
  @SuppressWarnings("all")
  public void composeOrWithNullFiltersReturnsNull() {
    assertThat(ComposableFilter.or(null, null)).isNull();
  }

  @Test
  public void composeOrWithOneFilterReturnsOneFilter() {

    assertThat(ComposableFilter.or(null, this.falseFilter)).isSameAs(this.falseFilter);
    assertThat(ComposableFilter.or(this.trueFilter, null)).isSameAs(this.trueFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void composeOrWithTwoFiltersReturnsCompositeFilter() {

    Filter<Object> composedFilter = ComposableFilter.or(this.trueFilter, this.falseFilter);

    assertThat(composedFilter).isNotNull();
    assertThat(composedFilter).isNotSameAs(this.trueFilter);
    assertThat(composedFilter).isNotSameAs(this.falseFilter);
    assertThat(composedFilter).isInstanceOf(ComposableFilter.class);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterOne()).isSameAs(this.trueFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterTwo()).isSameAs(this.falseFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getOp()).isEqualTo(LogicalOperator.OR);
  }

  @Test
  @SuppressWarnings("all")
  public void composeXorWithNullFiltersReturnsNull() {
    assertThat(ComposableFilter.xor(null, null)).isNull();
  }

  @Test
  public void composeXorWithOneFilterReturnsOneFilter() {

    assertThat(ComposableFilter.xor(null, this.falseFilter)).isSameAs(this.falseFilter);
    assertThat(ComposableFilter.xor(this.trueFilter, null)).isSameAs(this.trueFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void composeXorWithTwoFiltersReturnsCompositeFilter() {

    Filter<Object> composedFilter = ComposableFilter.xor(this.trueFilter, this.falseFilter);

    assertThat(composedFilter).isNotNull();
    assertThat(composedFilter).isNotSameAs(this.trueFilter);
    assertThat(composedFilter).isNotSameAs(this.falseFilter);
    assertThat(composedFilter).isInstanceOf(ComposableFilter.class);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterOne()).isSameAs(this.trueFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getFilterTwo()).isSameAs(this.falseFilter);
    assertThat(((ComposableFilter<Object>) composedFilter).getOp()).isEqualTo(LogicalOperator.XOR);
  }

  @Test
  public void composeNonNullFiltersWithBuilder() {

    Filter<Object> filter = ComposableFilter.builder().compose(this.falseFilter, null);

    assertThat(filter).isSameAs(this.falseFilter);
    assertThat(filter.accept("test")).isFalse();

    filter = ComposableFilter.builder().compose(this.trueFilter, null);

    assertThat(filter).isSameAs(this.trueFilter);
    assertThat(filter.accept("test")).isTrue();

    filter = ComposableFilter.builder().compose(this.trueFilter, this.trueFilter);

    assertThat(filter).isNotSameAs(this.trueFilter);
    assertThat(filter).isInstanceOf(ComposableFilter.class);
    assertThat(filter.accept("test")).isTrue();

    Filter<Object> compositeFilter = ComposableFilter.builder().compose(filter, this.trueFilter);

    assertThat(compositeFilter).isNotSameAs(filter);
    assertThat(compositeFilter).isNotSameAs(this.trueFilter);
    assertThat(compositeFilter).isInstanceOf(ComposableFilter.class);
    assertThat(compositeFilter.accept("test")).isTrue();

    compositeFilter = ComposableFilter.builder().compose(compositeFilter, this.falseFilter);

    assertThat(compositeFilter).isInstanceOf(ComposableFilter.class);
    assertThat(compositeFilter.accept("test")).isFalse();

    compositeFilter = ComposableFilter.builder().compose(compositeFilter, this.trueFilter);

    assertThat(compositeFilter).isInstanceOf(ComposableFilter.class);
    assertThat(compositeFilter.accept("test")).isFalse();
  }

  @Test
  public void composeNullFiltersWithBuilder() {
    assertThat(ComposableFilter.builder().compose(null, null)).isNull();
  }

  @Test
  public void toStringIsFullyQualifiedClassName() {
    assertThat(ComposableFilter.builder().toString()).isEqualTo(ComposableFilter.class.getName());
  }

  private static final class Driver {

    private final boolean drivingWithParent;
    private final int age;

    private Driver(int age, boolean drivingWithParent) {
      this.age = age;
      this.drivingWithParent = drivingWithParent;
    }

    private int getAge() {
      return age;
    }

    private boolean isDrivingWithParent() {
      return drivingWithParent;
    }

    @Override
    public String toString() {
      return MessageFormat.format("The driver is ({0}) and {1} driving with parent.", getAge(),
        (isDrivingWithParent() ? "is" : "is not"));
    }
  }

  private static final class DrivingAgeFilter implements Filter<Driver> {

    private final int legalDrivingAge;

    private DrivingAgeFilter(int legalDrivingAge) {
      this.legalDrivingAge = legalDrivingAge;
    }

    public boolean accept(Driver driver) {
      return (driver.getAge() >= this.legalDrivingAge);
    }
  }

  private static final class ParentRequiredFilter implements Filter<Driver> {

    public boolean accept(Driver driver) {
      return driver.isDrivingWithParent();
    }
  }
}
