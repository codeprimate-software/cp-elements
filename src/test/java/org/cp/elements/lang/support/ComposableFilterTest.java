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

package org.cp.elements.lang.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.when;

import java.text.MessageFormat;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The ComposableFilterTest class is a test suite of test cases testing the contract and functionality 
 * of the ComposableFilter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableFilterTest {

  @Mock
  private Filter<Object> falseFilter;

  @Mock
  private Filter<Object> trueFilter;

  @Before
  public void setup() {
    when(falseFilter.accept(anyObject())).thenReturn(false);
    when(trueFilter.accept(anyObject())).thenReturn(true);
  }

  @Test
  public void testAcceptUsingAnd() {
    assertFalse(ComposableFilter.and(falseFilter, falseFilter).accept("test"));
    assertFalse(ComposableFilter.and(trueFilter, falseFilter).accept("test"));
    assertFalse(ComposableFilter.and(falseFilter, trueFilter).accept("test"));
    assertTrue(ComposableFilter.and(trueFilter, trueFilter).accept("test"));
  }

  @Test
  public void testAcceptUsingOr() {
    assertFalse(ComposableFilter.or(falseFilter, falseFilter).accept("test"));
    assertTrue(ComposableFilter.or(trueFilter, falseFilter).accept("test"));
    assertTrue(ComposableFilter.or(falseFilter, trueFilter).accept("test"));
    assertTrue(ComposableFilter.or(trueFilter, trueFilter).accept("test"));
  }

  @Test
  public void testAcceptUsingAndWithOr() {
    final Filter<Driver> driverFilter = ComposableFilter.or(new DrivingAgeFilter(16),
      ComposableFilter.and(new DrivingAgeFilter(14), new ParentRequiredFilter()));

    assertNotNull(driverFilter);
    assertTrue(driverFilter instanceof ComposableFilter);
    assertTrue(driverFilter.accept(new Driver(18, true)));
    assertTrue(driverFilter.accept(new Driver(18, false)));
    assertTrue(driverFilter.accept(new Driver(16, true)));
    assertTrue(driverFilter.accept(new Driver(16, false)));
    assertTrue(driverFilter.accept(new Driver(15, true)));
    assertFalse(driverFilter.accept(new Driver(15, false)));
    assertTrue(driverFilter.accept(new Driver(14, true)));
    assertFalse(driverFilter.accept(new Driver(14, false)));
    assertFalse(driverFilter.accept(new Driver(13, true)));
    assertFalse(driverFilter.accept(new Driver(13, false)));
  }

  @Test
  public void testComposeAnd() {
    assertNull(ComposableFilter.and(null, null));
    assertSame(trueFilter, ComposableFilter.and(trueFilter, null));
    assertSame(falseFilter, ComposableFilter.and(null, falseFilter));

    final Filter<Object> composedFilter = ComposableFilter.and(trueFilter, falseFilter);

    assertNotNull(composedFilter);
    assertNotSame(trueFilter, composedFilter);
    assertNotSame(falseFilter, composedFilter);
    assertTrue(composedFilter instanceof ComposableFilter);
    assertSame(trueFilter, ((ComposableFilter) composedFilter).getLeftFilter());
    assertSame(falseFilter, ((ComposableFilter) composedFilter).getRightFilter());
    assertEquals(LogicalOperator.AND, ((ComposableFilter) composedFilter).getOp());
  }

  @Test
  public void testComposeOr() {
    assertNull(ComposableFilter.or(null, null));
    assertSame(trueFilter, ComposableFilter.or(trueFilter, null));
    assertSame(falseFilter, ComposableFilter.or(null, falseFilter));

    final Filter<Object> composedFilter = ComposableFilter.or(trueFilter, falseFilter);

    assertNotNull(composedFilter);
    assertNotSame(trueFilter, composedFilter);
    assertNotSame(falseFilter, composedFilter);
    assertTrue(composedFilter instanceof ComposableFilter);
    assertSame(trueFilter, ((ComposableFilter) composedFilter).getLeftFilter());
    assertSame(falseFilter, ((ComposableFilter) composedFilter).getRightFilter());
    assertEquals(LogicalOperator.OR, ((ComposableFilter) composedFilter).getOp());
  }

  private static final class Driver {

    private final boolean drivingWithParent;
    private final int age;

    public Driver(final int age, final boolean drivingWithParent) {
      this.age = age;
      this.drivingWithParent = drivingWithParent;
    }

    public int getAge() {
      return age;
    }

    public boolean isDrivingWithParent() {
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

    public DrivingAgeFilter(final int legalDrivingAge) {
      this.legalDrivingAge = legalDrivingAge;
    }

    public boolean accept(final Driver driver) {
      return (driver.getAge() >= this.legalDrivingAge);
    }
  }

  private static final class ParentRequiredFilter implements Filter<Driver> {

    public boolean accept(final Driver driver) {
      return driver.isDrivingWithParent();
    }
  }
}
