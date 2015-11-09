/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.cp.elements.lang.LangExtensions.*;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ComparatorUtils;
import org.hamcrest.CoreMatchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The LangExtensionsTest class is a test suite of test cases testing the contract and functionality
 * of the LangExtensions class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.LangExtensions
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LangExtensionsTest {

  private final Comparable NULL = null;

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Test
  public void assertThatClassTypesAreAssignable() {
    assertThat(Boolean.class).isAssignableTo(Object.class);
    assertThat(Character.class).isAssignableTo(Object.class);
    assertThat(Double.class).isAssignableTo(Number.class);
    assertThat(Integer.class).isAssignableTo(Number.class);
    assertThat(Number.class).isAssignableTo(Object.class);
    assertThat(String.class).isAssignableTo(String.class);
    assertThat(String.class).isAssignableTo(Object.class);
    assertThat(Object.class).isAssignableTo(Object.class);
  }

  @Test
  public void assertThatClassTypesAreNotAssignable() {
    assertThat(Boolean.class).not().isAssignableTo(Number.class);
    assertThat(Character.class).not().isAssignableTo(String.class);
    assertThat(Float.class).not().isAssignableTo(Double.class);
    assertThat(Integer.class).not().isAssignableTo(Long.class);
    assertThat(Object.class).not().isAssignableTo(String.class);
  }

  @Test
  public void assertThatBooleanIsNotAssignableToNumberThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(Boolean.class).throwing(new IllegalArgumentException("test")).isAssignableTo(Number.class);
  }

  @Test
  public void assertThatFloatTypeIsNotAssignableToIntegerType() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(class java.lang.Float) is not assignable to (class java.lang.Integer)");

    assertThat(Float.class).isAssignableTo(Integer.class);
  }

  @Test
  public void assertThatObjectIsNotAssignableToStringUsingCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(Object.class).using("This is a %1$s{1}", "test", "!").isAssignableTo(String.class);
  }

  @Test
  public void assertThatStringTypeIsAssignableToObjectType() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(class java.lang.String) is assignable to (class java.lang.Object)");

    assertThat(String.class).not().isAssignableTo(Object.class);
  }

  @Test
  public void test() {
  }

  @Test
  public void assertThatFalseIsFalse() {
    LangExtensions.assertThat(false).isFalse();
    LangExtensions.assertThat(Boolean.FALSE).isFalse();
    LangExtensions.assertThat(false).not().isTrue();
  }

  @Test
  public void assertThatFalseIsTrueThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(false) is not true");
    LangExtensions.assertThat(false).isTrue();
  }

  @Test
  public void assertThatNonNullObjectIsNotNull() {
    LangExtensions.assertThat("test").isNotNull();
    LangExtensions.assertThat("test").not().isNull();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not null");
    LangExtensions.assertThat("test").isNull();
  }

  @Test
  public void assertThatNullObjectIsNull() {
    LangExtensions.assertThat(null).isNull();
    LangExtensions.assertThat(null).not().isNotNull();
  }

  @Test
  public void assertThatNullObjectIsNotNullThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(null) is null");
    LangExtensions.assertThat(null).isNotNull();
  }

  @Test
  public void assertThatTrueIsTrue() {
    LangExtensions.assertThat(true).isTrue();
    LangExtensions.assertThat(Boolean.TRUE).isTrue();
    LangExtensions.assertThat(true).not().isFalse();
  }

  @Test
  public void assertThatTrueIsFalseThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(true) is not false");
    LangExtensions.assertThat(true).isFalse();
  }

  @Test
  public void isAssignableFrom() {
    assertTrue(is(Object.class).assignableTo(Object.class));
    assertTrue(is(Boolean.class).assignableTo(Object.class));
    assertTrue(is(Character.class).assignableTo(Object.class));
    assertTrue(is(Number.class).assignableTo(Object.class));
    assertTrue(is(String.class).assignableTo(Object.class));
    assertTrue(is(Double.class).assignableTo(Number.class));
    assertTrue(is(Integer.class).assignableTo(Number.class));
    assertTrue(is(java.sql.Date.class).assignableTo(java.util.Date.class));
    assertTrue(is(Object.class).not().assignableTo(String.class));
  }

  @Test
  public void isNotAssignableFrom() {
    assertFalse(is(Object.class).assignableTo(String.class));
    assertFalse(is(Boolean.TYPE).assignableTo(Boolean.class));
    assertFalse(is(Double.class).assignableTo(BigDecimal.class));
    assertFalse(is(Integer.class).assignableTo(BigInteger.class));
    assertFalse(is(Integer.class).assignableTo(Long.class));
    assertFalse(is(String.class).assignableTo(BigDecimal.class));
    assertFalse(is(String.class).assignableTo(Character.class));
    assertFalse(is(Character.class).assignableTo(String.class));
  }

  @Test
  public void testIsEqualByComparison() {
    final Person jonDoe1 = new Person(1l, "Jon", "Doe");
    final Person jonDoe2 = new Person(2l, "Jon", "Doe");

    assertTrue(is(jonDoe1).comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).not().comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).equalTo(jonDoe2));
  }

  @Test
  public void testIsNotEqualByComparison() {
    final Person johnBlum = new Person(1l, "John", "Blum");
    final Person jonBloom = new Person(1l, "Jon", "Bloom");

    assertFalse(is(johnBlum).comparableTo(jonBloom));
    assertTrue(is(johnBlum).not().comparableTo(jonBloom));
    assertTrue(is(johnBlum).equalTo(jonBloom));
  }

  @Test
  public void testIsEqualTo() {
    assertTrue(is(true).equalTo(Boolean.TRUE));
    assertTrue(is('c').equalTo('c'));
    assertTrue(is(2).equalTo(2));
    assertTrue(is(Math.PI).equalTo(Math.PI));
    assertTrue(is("test").equalTo("test"));
  }

  @Test
  public void testIsEqualToWithUnequalValues() {
    assertFalse(is(NULL).equalTo(NULL));
    assertFalse(is("null").equalTo(null));
    assertFalse(is("null").equalTo("nil"));
    assertFalse(is(Boolean.FALSE).equalTo(Boolean.TRUE));
    assertFalse(is((Comparable) 'c').equalTo("c"));
    assertFalse(is(-2).equalTo(2));
    assertFalse(is(3.14159d).equalTo(Math.PI));
    assertFalse(is("test").equalTo("TEST"));
  }

  @Test
  public void testIsNotEqualTo() {
    assertTrue((is(NULL).not().equalTo(NULL)));
    assertTrue(is((String) null).not().equalTo("null"));
    assertTrue(is("null").not().equalTo(null));
    assertTrue(is("null").not().equalTo("nil"));
    assertTrue(is(Boolean.FALSE).not().equalTo(Boolean.TRUE));
    assertTrue(is((Comparable) 'c').not().equalTo("c"));
    assertTrue(is(-2).not().equalTo(2));
    assertTrue(is(3.14159d).not().equalTo(Math.PI));
    assertTrue(is("test").not().equalTo("TEST"));
    assertTrue(is(TestUtils.createCalendar(2011, Calendar.OCTOBER, 13)).not().equalTo(Calendar.getInstance()));
  }

  @Test
  public void testIsFalse() {
    assertTrue(is(false).False());
    assertTrue(is(Boolean.FALSE).False());
    assertTrue(is(!Boolean.TRUE).False());
  }

  @Test
  public void testIsNotFalse() {
    assertTrue(is(true).not().False());
    assertTrue(is(Boolean.TRUE).not().False());
    assertTrue(is(!Boolean.FALSE).not().False());
    assertTrue(is((Boolean) null).not().False());
  }

  @Test
  public void testIsGreaterThan() {
    assertTrue(is(3).greaterThan(1));
    assertTrue(is(3).greaterThan(2));
    assertFalse(is(3).greaterThan(3));
    assertFalse(is(3).greaterThan(4));
    assertFalse(is(3).greaterThan(5));
  }

  @Test
  public void testIsNotGreaterThan() {
    assertFalse(is(3).not().greaterThan(1));
    assertFalse(is(3).not().greaterThan(2));
    assertTrue(is(3).not().greaterThan(3));
    assertTrue(is(3).not().greaterThan(4));
    assertTrue(is(3).not().greaterThan(5));
  }

  @Test
  public void testIsGreaterThanAndLessThan() {
    assertFalse(is(1).greaterThanAndLessThan(2, 4));
    assertFalse(is(2).greaterThanAndLessThan(2, 4));
    assertTrue(is(3).greaterThanAndLessThan(2, 4));
    assertFalse(is(4).greaterThanAndLessThan(2, 4));
    assertFalse(is(5).greaterThanAndLessThan(2, 4));
  }

  @Test
  public void testIsNotGreaterThanAndLessThan() {
    assertTrue(is(1).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThan(2, 4));
  }

  @Test
  public void testIsGreaterThanAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(2).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotGreaterThanAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsGreaterThanEqualTo() {
    assertTrue(is(3).greaterThanEqualTo(1));
    assertTrue(is(3).greaterThanEqualTo(2));
    assertTrue(is(3).greaterThanEqualTo(3));
    assertFalse(is(3).greaterThanEqualTo(4));
    assertFalse(is(3).greaterThanEqualTo(5));
  }

  @Test
  public void testIsNotGreaterThanEqualTo() {
    assertFalse(is(3).not().greaterThanEqualTo(1));
    assertFalse(is(3).not().greaterThanEqualTo(2));
    assertFalse(is(3).not().greaterThanEqualTo(3));
    assertTrue(is(3).not().greaterThanEqualTo(4));
    assertTrue(is(3).not().greaterThanEqualTo(5));
  }

  @Test
  public void testIsGreaterThanEqualToAndLessThan() {
    assertFalse(is(1).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(4).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void testIsNotGreaterThanEqualToAndLessThan() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void testIsGreaterThanEqualToAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotGreaterThanEqualToAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsInstanceOf() {
    assertTrue(is(false).instanceOf(Boolean.class));
    assertTrue(is(Boolean.TRUE).instanceOf(Boolean.class));
    assertTrue(is(Calendar.getInstance()).instanceOf(Calendar.class));
    assertTrue(is('c').instanceOf(Character.class));
    assertTrue(is('0').instanceOf(Character.class));
    assertTrue(is(2).instanceOf(Number.class));
    assertTrue(is(0).instanceOf(Integer.class));
    assertTrue(is(Math.PI).instanceOf(Number.class));
    assertTrue(is(0.0d).instanceOf(Double.class));
    assertTrue(is("null").instanceOf(String.class));
    assertTrue(is("test").instanceOf(Object.class));
  }

  @Test
  public void testIsNotInstanceOf() {
    assertTrue(is("false").not().instanceOf(Boolean.class));
    assertTrue(is(Calendar.getInstance()).not().instanceOf(Date.class));
    assertTrue(is("c").not().instanceOf(Character.class));
    assertTrue(is(Math.PI).not().instanceOf(Integer.class));
    assertTrue(is(2).not().instanceOf(Double.class));
    assertTrue(is('c').not().instanceOf(String.class));
    assertTrue(is(NULL).not().instanceOf(Object.class));
    assertTrue(is("test").not().instanceOf(null));
  }

  @Test
  public void testIsLessThan() {
    assertFalse(is(3).lessThan(1));
    assertFalse(is(3).lessThan(2));
    assertFalse(is(3).lessThan(3));
    assertTrue(is(3).lessThan(4));
    assertTrue(is(3).lessThan(5));
  }

  @Test
  public void testIsNotLessThan() {
    assertTrue(is(3).not().lessThan(1));
    assertTrue(is(3).not().lessThan(2));
    assertTrue(is(3).not().lessThan(3));
    assertFalse(is(3).not().lessThan(4));
    assertFalse(is(3).not().lessThan(5));
  }

  @Test
  public void testIsLessThanOrGreaterThan() {
    assertTrue(is(1).lessThanOrGreaterThan(2, 4));
    assertFalse(is(2).lessThanOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void testIsNotLessThanOrGreaterThan() {
    assertFalse(is(1).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void testIsLessThanOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotLessThanOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsLessThanEqualTo() {
    assertFalse(is(3).lessThanEqualTo(1));
    assertFalse(is(3).lessThanEqualTo(2));
    assertTrue(is(3).lessThanEqualTo(3));
    assertTrue(is(3).lessThanEqualTo(4));
    assertTrue(is(3).lessThanEqualTo(5));
  }

  @Test
  public void testIsNotLessThanEqualTo() {
    assertTrue(is(3).not().lessThanEqualTo(1));
    assertTrue(is(3).not().lessThanEqualTo(2));
    assertFalse(is(3).not().lessThanEqualTo(3));
    assertFalse(is(3).not().lessThanEqualTo(4));
    assertFalse(is(3).not().lessThanEqualTo(5));
  }

  @Test
  public void testIsLessThanEqualToOrGreaterThan() {
    assertTrue(is(1).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void testIsNotLessThanEqualToOrGreaterThan() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void testIsLessThanEqualToOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotLessThanEqualToOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotNull() {
    assertTrue(is(Boolean.FALSE).notNull());
    assertTrue(is(Calendar.getInstance()).notNull());
    assertTrue(is('\0').notNull());
    assertTrue(is(Integer.MIN_VALUE).notNull());
    assertTrue(is(Double.MAX_VALUE).notNull());
    assertTrue(is("null").notNull());
  }

  @Test
  public void testIsNull() {
    assertTrue(is(NULL).Null());
  }

  @Test
  public void testIsSameAs() {
    assertTrue(is(NULL).sameAs(NULL));
    assertTrue(is(Boolean.TRUE).sameAs(Boolean.TRUE));
    assertTrue(is("test").sameAs("test"));
  }

  @Test
  @SuppressWarnings("all")
  public void testIsNotSameAs() {
    assertTrue(is(NULL).notSameAs("null"));
    assertTrue(is(Boolean.TRUE).notSameAs(Boolean.FALSE));
    assertTrue(is("test").notSameAs(new String("test")));
  }

  @Test
  public void testIsTrue() {
    assertTrue(is(true).True());
    assertTrue(is(Boolean.TRUE).True());
    assertTrue(is(!Boolean.FALSE).True());
  }

  @Test
  public void testIsNotTrue() {
    assertTrue(is(false).not().True());
    assertTrue(is(Boolean.FALSE).not().True());
    assertTrue(is(!Boolean.TRUE).not().True());
    assertTrue(is((Boolean) null).not().True());
  }

  @Test
  public void testIsReuse() {
    final Is<String> isOperator = is("test");

    assertNotNull(isOperator);
    assertTrue(isOperator.equalTo("test"));
    assertFalse(isOperator.equalTo("testing"));
    assertTrue(isOperator.not().equalTo("testing"));
    assertTrue(isOperator.equalTo("test"));
    assertFalse(isOperator.equalTo("TEST"));
    assertTrue(isOperator.not().equalTo("TEST"));
    assertTrue(isOperator.equalTo("test"));
  }

  @Test
  public void testIsDoubleNegative() {
    assertTrue(is(true).not().not().True());
    assertTrue(is(false).not().not().False());
  }

  protected static class Person implements Comparable<Person> {

    private final Long id;

    private final String firstName;
    private final String lastName;

    public Person(final Long id, final String firstName, final String lastName) {
      this.id = id;
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public Long getId() {
      return id;
    }

    public String getFirstName() {
      return firstName;
    }

    public String getLastName() {
      return lastName;
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(final Person person) {
      int compareValue = ComparatorUtils.compareIgnoreNull(getFirstName(), person.getFirstName());
      return (compareValue != 0 ? compareValue : ComparatorUtils.compareIgnoreNull(getLastName(), person.getLastName()));
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      final Person that = (Person) obj;

      return (ObjectUtils.equalsIgnoreNull(this.getId(), that.getId()));
        //&& ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        //&& ObjectUtils.equals(this.getLastName(), that.getLastName()));
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getId());
      //hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getFirstName());
      //hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getLastName());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("{ id = %1$s, firstName = %2$s, lastName = %3$s }", getId(), getFirstName(), getLastName());
    }

  }

}
