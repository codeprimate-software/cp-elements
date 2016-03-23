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

package org.cp.elements.lang;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.test.TestUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The AssertTest class is a test suite of test cases to test the contract and functionality of the Assert class
 * in the org.cp.elements API and Framework.
 * 
 * @author John J. Blum
 * @see org.cp.elements.lang.Assert
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Test
 * @since 1.0.0
 * @version 1.0.0
 */
public class AssertTest {

  private static final Object LOCK = new Object();

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected static Object returnsNull() {
    return null;
  }

  @Test
  public void assertArgumentIsValid() {
    Assert.argument(true);
  }

  @Test
  public void assertArgumentIsInvalid() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("argument is not valid");

    Assert.argument(false);
  }

  @Test
  public void assertArgumentWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not valid");

    Assert.argument(null, "[%1$s] is {1} valid {1}", null, "not");
  }

  @Test
  public void assertArgumentFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[spy] is a bad argument, a bad, bad, bad argument; not good");

    Assert.argument(false, "[%1$s] is a {1} argument, a %2$s, {1}, %2$s argument; {2}",
      "spy", "bad", "not good");
  }

  @Test
  public void assertArgumentThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.argument(false, new AssertionException("test"));
  }

  @Test
  public void assertComparable() {
    Assert.comparable(true, true);
    Assert.comparable('c', 'c');
    Assert.comparable(1, 1);
    Assert.comparable(Math.PI, Math.PI);
    Assert.comparable("test", "test");
  }

  @Test
  public void assertNotComparable() {
    exception.expect(ComparisonException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[3.14159] is not comparable to [%1$s]", Math.PI));

    Assert.comparable(3.14159d, Math.PI);
  }

  @Test
  public void assertComparableWithNullArguments() {
    exception.expect(ComparisonException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not comparable to [null]");

    Assert.comparable(null, null);
  }

  @Test
  public void assertComparableWithNullFirstArgument() {
    exception.expect(ComparisonException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not comparable to [test]");

    Assert.comparable(null, "test");
  }

  @Test
  public void assertComparableWithNullSecondArgument() {
    exception.expect(ComparisonException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[test] is not comparable to [null]");

    Assert.comparable("test", null);
  }

  @Test
  public void assertComparableFormatsMessageWithArguments() {
    exception.expect(ComparisonException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is NOT comparable with [nil]");

    Assert.comparable("null", "nil", "[%1$s] is NOT comparable with [{1}]", "null", "nil");
  }

  @Test
  public void assertNotComparableThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.comparable(-1, 1, new AssertionException("test"));
  }

  @Test
  public void assertEqualsWithEqualValues() {
    Assert.equals(Boolean.TRUE, true);
    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4),
      TestUtils.createCalendar(2011, Calendar.OCTOBER, 4));
    Assert.equals(TestUtils.createCalendar(2013, Calendar.JANUARY, 13),
      TestUtils.createCalendar(2013, Calendar.JANUARY, 13));
    Assert.equals(TestUtils.createCalendar(2015, Calendar.JULY, 16),
      TestUtils.createCalendar(2015, Calendar.JULY, 16));
    Assert.equals(TestUtils.createCalendar(2016, Calendar.MARCH, 22),
      TestUtils.createCalendar(2016, Calendar.MARCH, 22));
    Assert.equals("c".charAt(0), 'c');
    Assert.equals(Double.valueOf(String.valueOf(Math.PI)), Math.PI);
    Assert.equals(Integer.valueOf("2"), 2);
    Assert.equals("test", "test");
    Assert.equals(TestEnum.valueOf("ONE"), TestEnum.ONE);
  }

  @Test
  public void assertEqualsWithUnequalBooleanValues() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[true] is not equal to [false]");

    Assert.equals(Boolean.TRUE, false);
  }

  @Test
  public void assertEqualsWithUnequalCalendars() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("calendars are not equal");

    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4), Calendar.getInstance(),
      "calendars are not equal");
  }

  @Test
  public void assertEqualsWithUnequalCharacters() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[x] is not equal to [X]");

    Assert.equals('x', 'X');
  }

  @Test
  public void assertEqualsWithUnequalDoubleValues() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[3.14159] is not equal to [%1$s]", Math.PI));

    Assert.equals(3.14159d, Math.PI);
  }

  @Test
  public void assertEqualsWithUnequalIntegerValues() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[-1] is not equal to [1]");

    Assert.equals(-1, 1);
  }

  @Test
  public void assertEqualsWithUnequalStrings() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[test] is not equal to [TEST]");

    Assert.equals("test", "TEST");
  }

  @Test
  public void assertEqualsWithNullAndNil() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not equal to [nil]");

    Assert.equals("null", "nil");
  }

  @Test
  public void assertEqualsWithNullValueAndNullStringLiteral() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not equal to [null]");

    Assert.equals(null, "null");
  }

  @Test
  public void assertEqualsWithNullValues() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] is not equal to [null]");

    Assert.equals(null, null);
  }

  @Test
  public void assertEqualsFormatsMessageUsingArguments() {
    exception.expect(EqualityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Expected true; but was false");

    Assert.equals(true, false, "Expected %1$s; but was {1}", true, false);
  }

  @Test
  public void assertEqualsThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.equals(new Object(), new Object(), new AssertionException("test"));
  }

  @Test
  public void assertHoldsLock() {
    synchronized (LOCK) {
      Assert.holdsLock(LOCK);
    }
  }

  @Test
  public void assertDoesNotHoldLock() {
    exception.expect(IllegalMonitorStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("current thread [%1s] does not hold lock [%2$s]",
      Thread.currentThread().getName(), LOCK));

    Assert.holdsLock(LOCK);
  }

  @Test
  public void assertHoldsLockWithNullLock() {
    exception.expect(IllegalMonitorStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("current thread [%1s] does not hold lock [null]",
      Thread.currentThread().getName()));

    Assert.holdsLock(null);
  }

  @Test
  public void assertHoldsLockFormatsMessageWithArguments() {
    exception.expect(IllegalMonitorStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("lock [%1$s] not held by current thread", LOCK));

    Assert.holdsLock(LOCK, "lock [%1$s] not held by {1} thread", LOCK, "current");
  }

  @Test
  public void assertHoldsLockThrowsAssertionFailedException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.holdsLock(LOCK, new AssertionException("test"));
  }

  @Test
  public void assertIsAssignableToWithAssignableClassTypes() {
    Assert.isAssignableTo(Boolean.class, Boolean.class);
    Assert.isAssignableTo(Character.class, Object.class);
    Assert.isAssignableTo(java.sql.Date.class, java.util.Date.class);
    Assert.isAssignableTo(Double.class, Number.class);
    Assert.isAssignableTo(Integer.class, Number.class);
    Assert.isAssignableTo(String.class, Object.class);
    Assert.isAssignableTo(TestEnum.class, Enum.class);
  }

  @Test
  public void assertIsAssignableToWithNonAssignableClassTypes() {
    exception.expect(ClassCastException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[%1$s] is not assignable to [%2$s]",
      Character.class, String.class));

    Assert.isAssignableTo(Character.class, String.class);
  }

  @Test
  public void assertIsAssignableToWithNullFromClassType() {
    Assert.isAssignableTo(null, Object.class, "null is not assignable to Object");
  }

  @Test
  public void assertIsAssignableToWithNullToClassType() {
    exception.expect(ClassCastException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Object is not assignable to null");

    Assert.isAssignableTo(Object.class, null, "Object is not assignable to null");
  }

  @Test
  public void assertIsAssignableToFormatsMessageWithArguments() {
    exception.expect(ClassCastException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Object is not assignable to String");

    Assert.isAssignableTo(Object.class, String.class, "%1$s is not assignable to {1}", "Object", "String");
  }

  @Test
  public void assertIsAssignableToThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.isAssignableTo(Object.class, String.class, new AssertionException("test"));
  }

  @Test
  public void assertIsFalseWithFalse() {
    Assert.isFalse(false);
    Assert.isFalse(Boolean.FALSE);
    Assert.isFalse(!Boolean.TRUE);
    Assert.isFalse(new Object() == new Object());
  }

  @Test
  public void assertIsFalseWithTrue() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("condition [true] is not false");

    Assert.isFalse(true);
  }

  @Test
  public void assertIsFalseWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("null is not false");

    Assert.isFalse(null, "null is not false");
  }

  @Test
  public void assertIsFalseFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("expected false; but was true");

    Assert.isFalse(true, "expected %1$s; but was {1}", false, true);
  }

  @Test
  public void assertIsFalseThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.isFalse(Boolean.TRUE, new AssertionException("test"));
  }

  @Test
  public void assertIsInstanceOf() throws Exception {
    Assert.isInstanceOf(true, Boolean.class);
    Assert.isInstanceOf('c', Character.class);
    Assert.isInstanceOf(3.14159f, Float.class);
    Assert.isInstanceOf(Math.PI, Double.class);
    Assert.isInstanceOf(0, Integer.class);
    Assert.isInstanceOf(1l, Long.class);
    Assert.isInstanceOf("mock", CharSequence.class);
    Assert.isInstanceOf("spy", Serializable.class);
    Assert.isInstanceOf("test", String.class);
    Assert.isInstanceOf(new Object(), Object.class);
    Assert.isInstanceOf(Object.class, Class.class);
    Assert.isInstanceOf(TestEnum.ONE, Enum.class);
  }

  @Test
  public void assertIsNotInstanceOf() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[0123456789] is not an instance of [class java.lang.Long]");

    Assert.isInstanceOf("0123456789", Long.class);
  }

  @Test
  public void assertIsInstanceOfWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("null is not an instance of Object");

    Assert.isInstanceOf(null, Object.class, "null is not an instance of Object", "unused", "args");
  }

  @Test
  public void assertIsAssignableOfFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("null is not an instance of Object");

    Assert.isInstanceOf(null, Object.class, "%1$s is not an instance of {1}", null, Object.class.getSimpleName());
  }

  @Test
  public void assertIsInstanceOfThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.isInstanceOf(new Object(), Class.class, new AssertionException("test"));
  }

  @Test
  @SuppressWarnings("all")
  public void assertIsTrueWithTrue() {
    Assert.isTrue(true);
    Assert.isTrue(Boolean.TRUE);
    Assert.isTrue(!Boolean.FALSE);
    Assert.isTrue("test".equals("test"));
    Assert.isTrue("test".equalsIgnoreCase("TEST"));
    Assert.isTrue(LOCK.equals(LOCK));
    Assert.isTrue(LOCK == LOCK);
  }

  @Test
  public void assertIsTrueWithFalse() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("condition [false] is not true");

    Assert.isTrue(false);
  }

  @Test
  public void assertIsTrueWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("null is not true");

    Assert.isTrue(null, "null is not true");
  }

  @Test
  public void assertIsTrueFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("expected true; but was false");

    Assert.isTrue(false, "expected %1$s; but was {1}", true, false);
  }

  @Test
  public void assertIsTrueThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.isTrue(Boolean.FALSE, new AssertionException("test"));
  }

  @Test
  public void assertNotBlankWithNonBlankValues() throws Exception {
    Assert.notBlank("test");
    Assert.notBlank("blank");
    Assert.notBlank("empty");
    Assert.notBlank("null");
    Assert.notBlank("space");
    Assert.notBlank("_");
    Assert.notBlank("--");
    Assert.notBlank("0");
    Assert.notBlank("! ");
    Assert.notBlank(" !");
    Assert.notBlank(" _ ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotBlankWithEmptyString() {
    Assert.notBlank("", "empty String is blank");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotBlankWithNull() {
    Assert.notBlank(null, "null is blank");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotBlankWithNullCharacter() {
    Assert.notBlank("\0", "null Character is blank");
  }

  @Test
  public void assertNotBlankWithSpaces() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("argument is blank");

    Assert.notBlank("  ");
  }

  @Test
  public void assertNotBlankWithTabCarriageReturnAndNewLine() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("tab and newlines are blank");

    Assert.notBlank("\t\r\n", "%1$s and {1} are blank", "tab", "newlines");
  }

  @Test
  public void assertNotBlankThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notBlank("\n", new AssertionException("test"));
  }

  @Test
  public void assertNotEmptyStringWithNonEmptyStrings() {
    Assert.notEmpty((String) null);
    Assert.notEmpty("blank");
    Assert.notEmpty("empty");
    Assert.notEmpty("null");
    Assert.notEmpty(" ");
    Assert.notEmpty("   ");
    Assert.notEmpty("_");
    Assert.notEmpty("___");
    Assert.notEmpty("\0");
    Assert.notEmpty("\n");
    Assert.notEmpty("\r");
    Assert.notEmpty("\t");
    Assert.notEmpty("test");
  }

  @Test
  public void assertNotEmptyStringWithEmptyString() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("argument is empty");

    Assert.notEmpty("");
  }

  @Test
  public void assertNotEmptyStringFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("empty String is empty");

    Assert.notEmpty("", "{0} String is %1$s", "empty");
  }

  @Test
  public void assertNotEmptyStringThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notEmpty("", new AssertionException("test"));
  }

  @Test
  public void assertNotEmptyArray() {
    Assert.notEmpty(new Object[] { "one", "two", "three" });
    Assert.notEmpty(new String[1]);
  }

  @Test
  public void assertNotEmptyArrayWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("array is empty");

    Assert.notEmpty((Object[]) null);
  }

  @Test
  public void assertNotEmptyArrayWithEmptyArray() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("array is empty");

    Assert.notEmpty(new Object[0]);
  }

  @Test
  public void assertNotEmptyArrayFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Object array is empty");

    Assert.notEmpty(new Object[0], "%1$s is {1}", "Object array", "empty");
  }

  @Test
  public void assertNotEmptyArrayThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notEmpty(new Object[0], new AssertionException("test"));
  }

  @Test
  public void assertNotEmptyCollection() {
    Assert.notEmpty(Arrays.asList("ant", "bat", "cat", "dog", "eel"));
    Assert.notEmpty(Collections.singleton(1));
    Assert.notEmpty(Collections.singletonList("test"));
  }

  @Test
  public void assertNotEmptyCollectionWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("collection is empty");

    Assert.notEmpty((Collection) null);
  }

  @Test
  public void assertNotEmptyCollectionWithEmptyCollection() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("collection is empty");

    Assert.notEmpty(Collections.emptyList());
  }

  @Test
  public void assertNotEmptyCollectionFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Set is empty");
    Assert.notEmpty(Collections.emptySet(), "%1$s is {1}", "Set", "empty");
  }

  @Test
  public void assertNotEmptyCollectionThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notEmpty((Collection) null, new AssertionException("test"));
  }

  @Test
  public void assertNotEmptyMap() {
    Map<String, String> map = new HashMap<>(2);

    map.put("one", "1");
    map.put("two", "2");

    Assert.notEmpty(map);
    Assert.notEmpty(Collections.singletonMap("key", "value"));
  }

  @Test
  public void assertNotEmptyMapWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("map is empty");

    Assert.notEmpty((Map) null);
  }

  @Test
  public void assertNotEmptyMapWithEmptyMap() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("map is empty");

    Assert.notEmpty(Collections.emptyMap());
  }

  @Test
  public void assertNotEmptyMapFormatsMessageWithArguments() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Map is empty");

    Assert.notEmpty(Collections.emptyMap(), "%1$s is {1}", "Map", "empty");
  }

  @Test
  public void assertNotEmptyMapThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notEmpty(Collections.emptyMap(), new AssertionException("test"));
  }

  @Test
  public void assertNotNull() throws Exception {
    Assert.notNull(false);
    Assert.notNull('\0');
    Assert.notNull(0);
    Assert.notNull(0.0d);
    Assert.notNull("nil");
    Assert.notNull("null");
    Assert.notNull(new Object());
    Assert.notNull(new Object[0]);
    Assert.notNull(Collections.emptySet());
    Assert.notNull(Void.class);
  }

  @Test
  public void assertNotNullWithNull() {
    exception.expect(NullPointerException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("argument is null");

    Assert.notNull(null);
  }

  @Test
  public void assertNotNullWithNullReturningMethod() {
    exception.expect(NullPointerException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("argument is null");

    Assert.notNull(returnsNull());
  }

  @Test
  public void assertNotNullFormatsMessageWithArguments() {
    exception.expect(NullPointerException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Object reference is null");

    Assert.notNull(null, "%1$s is {1}", "Object reference", null);
  }

  @Test
  public void assertNotNullThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.notNull(null, new AssertionException("test"));
  }

  @Test
  public void assertSameWithIdenticalObjects() {
    Assert.same(null, null);
    Assert.same(true, Boolean.TRUE);
    Assert.same('c', 'c');
    Assert.same(1, 1);
    //Assert.same(Math.PI, Math.PI, "PI should be the same as PI!");
    Assert.same("test", "test");
    Assert.same(LOCK, LOCK);
  }

  @Test
  public void assertSameWithCharacterAndString() {
    exception.expect(IdentityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[x] is not the same as [x]");

    Assert.same('x', "x");
  }

  @Test
  public void assertSameWithDoubleAndInteger() {
    exception.expect(IdentityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[1.0] is not the same as [1]");

    Assert.same(1.0d, 1);
  }

  @Test
  public void assetSameWithNullAndObject() {
    Object value = new Object();

    exception.expect(IdentityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[null] is not the same as [%1$s]", value));

    Assert.same(null, value);
  }

  @Test
  public void assertSameFormatsMessageWithArguments() {
    exception.expect(IdentityException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Strings are not identical");

    Assert.same("test", "TEST", "%1$s are not {1}", "Strings", "identical");
  }

  @Test
  public void assertSameThrowsAssertFailedException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.same(new Object(), new Object(), new AssertionException("test"));
  }

  @Test
  public void assertStateIsValid() {
    Assert.state(true);
  }

  @Test
  public void assertStateIsInvalid() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("state is invalid");

    Assert.state(false);
  }

  @Test
  public void assertStateWithNull() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("state is invalid");

    Assert.state(null);
  }

  @Test
  public void assertStateFormatsMessageWithArguments() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("object not initialized");

    Assert.state(Boolean.FALSE, "%1$s not {1}", "object", "initialized");
  }

  @Test
  public void assertStateThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.state(false, new AssertionException("test"));
  }

  @Test
  public void assertSupportedForSupportedOperation() {
    Assert.supported(true);
  }

  @Test
  public void assertSupportedForUnsupportedOperation() {
    exception.expect(UnsupportedOperationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("operation not supported");

    Assert.supported(false);
  }

  @Test
  public void assertSupportedWithNull() {
    exception.expect(UnsupportedOperationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("operation not supported");

    Assert.supported(null);
  }

  @Test
  public void assertSupportedFormatsMessageWithArguments() {
    exception.expect(UnsupportedOperationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("write operation is unsupported");

    Assert.supported(Boolean.FALSE, "%1$s is {1}", "write operation", "unsupported");
  }

  @Test
  public void assertSupportedThrowsAssertionException() {
    exception.expect(AssertionException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("test");

    Assert.supported(false, new AssertionException("test"));
  }

  protected enum TestEnum {
    ONE,
    TWO
  }

}
