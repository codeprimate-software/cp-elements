/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

import java.text.MessageFormat;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;
import org.junit.Test;

/**
 * The ComposableFilterTest class is a test suite of test cases testing the contract and functionality 
 * of the ComposableFilter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ComposableFilterTest {

  @Test
  public void testAcceptUsingAnd() {
    final Filter<Object> falseFilter = new DefaultFilter<Object>(false);
    final Filter<Object> trueFilter = new DefaultFilter<Object>(true);

    assertFalse(ComposableFilter.and(falseFilter, falseFilter).accept("test"));
    assertFalse(ComposableFilter.and(trueFilter, falseFilter).accept("test"));
    assertFalse(ComposableFilter.and(falseFilter, trueFilter).accept("test"));
    assertTrue(ComposableFilter.and(trueFilter, trueFilter).accept("test"));
  }

  @Test
  public void testAcceptUsingOr() {
    final Filter<Object> falseFilter = new DefaultFilter<Object>(false);
    final Filter<Object> trueFilter = new DefaultFilter<Object>(true);

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
    final Filter<Object> leftFilter = new DefaultFilter<Object>(true);
    final Filter<Object> rightFilter = new DefaultFilter<Object>(false);

    assertNull(ComposableFilter.and(null, null));
    assertSame(leftFilter, ComposableFilter.and(leftFilter, null));
    assertSame(rightFilter, ComposableFilter.and(null, rightFilter));

    final Filter<Object> composedFilter = ComposableFilter.and(leftFilter, rightFilter);

    assertNotNull(composedFilter);
    assertNotSame(leftFilter, composedFilter);
    assertNotSame(rightFilter, composedFilter);
    assertTrue(composedFilter instanceof ComposableFilter);
    assertSame(leftFilter, ((ComposableFilter) composedFilter).getLeftFilter());
    assertSame(rightFilter, ((ComposableFilter) composedFilter).getRightFilter());
    assertEquals(LogicalOperator.AND, ((ComposableFilter) composedFilter).getOp());
  }

  @Test
  public void testComposeOr() {
    final Filter<Object> leftFilter = new DefaultFilter<Object>(true);
    final Filter<Object> rightFilter = new DefaultFilter<Object>(false);

    assertNull(ComposableFilter.or(null, null));
    assertSame(leftFilter, ComposableFilter.or(leftFilter, null));
    assertSame(rightFilter, ComposableFilter.or(null, rightFilter));

    final Filter<Object> composedFilter = ComposableFilter.or(leftFilter, rightFilter);

    assertNotNull(composedFilter);
    assertNotSame(leftFilter, composedFilter);
    assertNotSame(rightFilter, composedFilter);
    assertTrue(composedFilter instanceof ComposableFilter);
    assertSame(leftFilter, ((ComposableFilter) composedFilter).getLeftFilter());
    assertSame(rightFilter, ((ComposableFilter) composedFilter).getRightFilter());
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
