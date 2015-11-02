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

import static org.hamcrest.CoreMatchers.*;

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
  public ExpectedException expectedException = ExpectedException.none();

  protected static Object returnsNull() {
    return null;
  }

  @Test
  public void assertArgumentIsValid() {
    Assert.argument(true, "argument is invalid");
  }

  @Test
  public void assertArgumentIsInvalid() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not a valid argument");
    Assert.argument(false, "({0}) is not a valid argument", "test");
  }

  @Test
  public void assertArgumentWithNullCondition() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(mock) is not a valid argument");
    Assert.argument(null, "(%1$s) is not a valid argument", "mock");
  }

  @Test
  public void assertArgumentThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.argument(false, new AssertionFailedException("test"));
  }

  @Test
  public void assertArgumentThrowsIllegalArgumentExceptionWithMixedMessageFormatting() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(spy) is a bad argument, a bad, bad, bad argument; just terrible");
    Assert.argument(false, "argument (%1$s) is a {1} argument, a %2$s, {1}, %2$s argument; just {2}",
      "spy", "bad", "terrible");
  }

  @Test
  public void assertEqualsWithEqualValues() {
    Assert.equals(Boolean.TRUE, true, "the values are unequal");
    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4),
      TestUtils.createCalendar(2011, Calendar.OCTOBER, 4), "the values are unequal");
    Assert.equals(TestUtils.createCalendar(2013, Calendar.JANUARY, 13),
      TestUtils.createCalendar(2013, Calendar.JANUARY, 13), "the values are unequal");
    Assert.equals(TestUtils.createCalendar(2015, Calendar.JULY, 16),
      TestUtils.createCalendar(2015, Calendar.JULY, 16), "the values are unequal");
    Assert.equals("c".charAt(0), 'c', "the values are unequal");
    Assert.equals(Double.valueOf(String.valueOf(Math.PI)), Math.PI, "the values are unequal");
    Assert.equals(Integer.valueOf("2"), 2, "the values are unequal");
    Assert.equals("test", "test", "the values are unequal");
    Assert.equals(TestEnum.valueOf("ONE"), TestEnum.ONE, "the values are unequal");
  }

  @Test
  public void assertEqualsWithUnequalBooleanValues() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the boolean values are not equal");
    Assert.equals(Boolean.TRUE, false, "the boolean values are not equal");
  }

  @Test
  public void assertEqualsWithUnequalCalendars() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the calendars are not equal");
    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4), Calendar.getInstance(),
      "the calendars are not equal");
  }

  @Test
  public void assertEqualsWithUnequalCharacters() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the characters are not equal");
    Assert.equals('x', 'X', "the characters are not equal");
  }

  @Test
  public void assertEqualsWithUnequalDoubleValues() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the double values are not equal");
    Assert.equals(3.14159d, Math.PI, "the double values are not equal");
  }

  @Test
  public void assertEqualsWithUnequalIntegerValues() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the integer values are not equal");
    Assert.equals(-1, 1, "the integer values are not equal");
  }

  @Test
  public void assertEqualsWithUnequalStrings() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the strings are not equal");
    Assert.equals("test", "TEST", "the strings are not equal");
  }

  @Test
  public void assertEqualsWithNullValues() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("null values are not equal");
    Assert.equals(null, null, "null values are not equal");
  }

  @Test
  public void assertEqualsWithNullValueAndNullStringLiteral() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("null is not equal to \"null\"");
    Assert.equals(null, "null", "null is not equal to \"null\"");
  }

  @Test
  public void assertEqualsWithNullAndNil() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("null is not equal to nil");
    Assert.equals("null", "nil", "null is not equal to nil");
  }

  @Test
  public void assertEqualsFormatsMessageUsingArguments() {
    expectedException.expect(EqualityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Expected true; but was false");
    Assert.equals(true, false, "Expected %1$s; but was {1}", true, false);
  }

  @Test
  public void assertEqualsThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.equals(new Object(), new Object(), new IllegalArgumentException("test"));
  }

  @Test
  public void assertHoldsLock() {
    synchronized (LOCK) {
      Assert.holdsLock(LOCK, "current Thread does not hold the lock");
    }
  }

  @Test
  public void assertHoldsLockWhenCurrentThreadDoesNotHoldTheLock() {
    expectedException.expect(IllegalMonitorStateException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage(String.format("current Thread (%1s) does not hold lock (%2$s)",
      Thread.currentThread().getName(), LOCK));
    Assert.holdsLock(LOCK, "current Thread ({0}) does not hold lock (%2$s)!", Thread.currentThread().getName(), LOCK);
  }

  @Test
  public void assertHoldsLockWithNullLock() {
    expectedException.expect(IllegalMonitorStateException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage(String.format("current Thread (%1s) does not hold lock (%2$s)",
      Thread.currentThread().getName(), null));
    Assert.holdsLock(null, "current Thread (%1$s) does not hold lock ({1})", Thread.currentThread().getName(), null);
  }

  @Test
  public void assertHoldsLockThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.holdsLock(LOCK, new AssertionFailedException("test"));
  }

  @Test
  public void assertIsAssignableToWithAssignableClassTypes() {
    Assert.isAssignableTo(Boolean.class, Boolean.class, "class type is not assignable");
    Assert.isAssignableTo(Character.class, Object.class, "class type is not assignable");
    Assert.isAssignableTo(java.sql.Date.class, java.util.Date.class, "class type is not assignable");
    Assert.isAssignableTo(Double.class, Number.class, "class type is not assignable");
    Assert.isAssignableTo(Integer.class, Number.class, "class type is not assignable");
    Assert.isAssignableTo(String.class, Object.class, "class type is not assignable");
    Assert.isAssignableTo(TestEnum.class, Enum.class, "class type is not assignable");
  }

  @Test
  public void assertIsAssignableToWithNonAssignableClassTypes() {
    expectedException.expect(ClassCastException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Character is not assignable to String");
    Assert.isAssignableTo(Character.class, String.class, "%1$s is not assignable to {1}!",
      "Character", "String");
  }

  @Test
  public void assertIsAssignableToWithNullFromClassType() {
    Assert.isAssignableTo(null, Object.class, "null was not assignable to Object");
  }

  @Test
  public void assertIsAssignableToWithNullToClassType() {
    expectedException.expect(ClassCastException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("String is not assignable to null");
    Assert.isAssignableTo(String.class, null, "String is not assignable to null");
  }

  @Test
  public void assertIsAssignableToThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.isAssignableTo(Object.class, String.class, new AssertionFailedException("test"));
  }

  @Test
  public void assertIsFalseWithFalse() {
    Assert.isFalse(false, "value is not false");
    Assert.isFalse(Boolean.FALSE, "value is not false");
    Assert.isFalse(!Boolean.TRUE, "value is not false");
    Assert.isFalse(new Object() == new Object(), "value is not false");
  }

  @Test
  public void assertIsFalseWithTrue() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("true is not false");
    Assert.isFalse(true, "%1$s is not {1}", "true", "false");
  }

  @Test
  public void assertIsFalseThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.isFalse(Boolean.TRUE, new AssertionFailedException("test"));
  }

  @Test
  public void assertIsInstanceOfWithInstances() throws Exception {
    Assert.isInstanceOf(true, Boolean.class, "object is not an instance of class type");
    Assert.isInstanceOf('c', Character.class, "object is not an instance of class type");
    Assert.isInstanceOf(3.14159f, Float.class, "object is not an instance of class type");
    Assert.isInstanceOf(Math.PI, Double.class, "object is not an instance of class type");
    Assert.isInstanceOf(0, Integer.class, "object is not an instance of class type");
    Assert.isInstanceOf(1l, Long.class, "object is not an instance of class type");
    Assert.isInstanceOf("mock", CharSequence.class, "object is not an instance of class type");
    Assert.isInstanceOf("test", String.class, "object is not an instance of class type");
    Assert.isInstanceOf(new Object(), Object.class, "object is not an instance of class type");
    Assert.isInstanceOf(Object.class, Class.class, "object is not an instance of class type");
    Assert.isInstanceOf(TestEnum.ONE, Enum.class, "object is not an instance of class type");
  }

  @Test
  public void assertIsInstanceOfWithNonInstances() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("A String is not an instance of the Long class!");
    Assert.isInstanceOf("0123456789", Long.class, "A %1$s is not an instance of the {1} class!",
      "String", "Long");
  }

  @Test
  public void assertIsInstanceOfWithNull() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("null is not an instance of Object");
    Assert.isInstanceOf(null, Object.class, "null is not an instance of Object", "unused", "args");
  }

  @Test
  public void assertIsInstanceOfThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.isInstanceOf(new Object(), Class.class, new AssertionFailedException("test"));
  }

  @Test
  @SuppressWarnings("all")
  public void assertIsTrueWithTrue() {
    Assert.isTrue(true, "value is not true");
    Assert.isTrue(Boolean.TRUE, "value is not true");
    Assert.isTrue(!Boolean.FALSE, "value is not true");
    Assert.isTrue("test".equals("test"), "value is not true");
    Assert.isTrue("test".equalsIgnoreCase("TEST"), "value is not true");
    Assert.isTrue(LOCK.equals(LOCK), "value is not true");
    Assert.isTrue(LOCK == LOCK, "value is not true");
  }

  @Test
  public void assertIsTrueWithFalse() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("false is not true");
    Assert.isTrue(false, "%1$s is not {1}", "false", "true");
  }

  @Test
  public void assertIsTrueThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.isTrue(Boolean.FALSE, new AssertionFailedException("test"));
  }

  @Test
  public void assertNotBlankWithNonBlankValues() throws Exception {
    Assert.notBlank("test", "value is blank");
    Assert.notBlank("blank", "value is blank");
    Assert.notBlank("empty", "value is blank");
    Assert.notBlank("null", "value is blank");
    Assert.notBlank("space", "value is blank");
    Assert.notBlank("_", "value is blank");
    Assert.notBlank("--", "value is blank");
    Assert.notBlank("0", "value is blank");
    Assert.notBlank("! ", "value is blank");
    Assert.notBlank(" !", "value is blank");
    Assert.notBlank(" _ ", "value is blank");
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotBlankWithSpaces() {
    Assert.notBlank("  ", "spaces are blank");
  }

  @Test
  public void assertNotBlankWithTabAndNewline() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("tab and newlines are blank");
    Assert.notBlank("\t\n", "%1$s and {1} are blank", "tab", "newlines");
  }

  @Test
  public void assertNotBlankWithNewlineThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notBlank("\n", new AssertionFailedException("test"));
  }

  @Test
  public void assertNotEmptyStringWithNonEmptyStrings() {
    Assert.notEmpty((String) null, "String is empty");
    Assert.notEmpty("blank", "String is empty");
    Assert.notEmpty("empty", "String is empty");
    Assert.notEmpty("null", "The String 'null' is not empty!");
    Assert.notEmpty(" ", "String is empty");
    Assert.notEmpty("   ", "String is empty");
    Assert.notEmpty("_", "String is empty");
    Assert.notEmpty("___", "String is empty");
    Assert.notEmpty("\0", "String is empty");
    Assert.notEmpty("\n", "String is empty");
    Assert.notEmpty("\t", "String is empty");
    Assert.notEmpty("test", "String is empty");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyStringWithEmptyString() {
    Assert.notEmpty("", "String is empty");
  }

  @Test
  public void assertNotEmptyStringWithEmptyStringAndPlaceholderMessage() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("empty String is empty");
    Assert.notEmpty("", "{0} String is %1$s", "empty");
  }

  @Test
  public void assertNotEmptyStringThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notEmpty("", new AssertionFailedException("test"));
  }

  @Test
  public void assertNotEmptyArray() {
    Assert.notEmpty(new Object[] { "assert", "mock", "test" }, "Object array is empty");
    Assert.notEmpty(new String[1], "Object array is empty");
  }

  @Test
  public void assertNotEmptyArrayWithNull() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Object array is null");
    Assert.notEmpty((Object[]) null, "Object array is %1$s", "null", "empty");
  }

  @Test
  public void assertNotEmptyArrayWithEmptyArray() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Object array is empty");
    Assert.notEmpty(new Object[0], "Object array is {1}", "null", "empty");
  }

  @Test
  public void assertNotEmptyArrayThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notEmpty((Object[]) null, new AssertionFailedException("test"));
  }

  @Test
  public void assertNotEmptyCollection() {
    Assert.notEmpty(Arrays.asList("ant", "bat", "cat", "dog"), "Collection is empty");
    Assert.notEmpty(Collections.singleton(1), "Collection is empty");
    Assert.notEmpty(Collections.singletonList("test"), "Collection is empty");
  }

  @Test
  public void assertNotEmptyCollectionWithNull() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Collection is null");
    Assert.notEmpty((Collection) null, "Collection is %1$s", "null", "empty");
  }

  @Test
  public void assertNotEmptyCollectionWithEmptyCollection() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Collection is empty");
    Assert.notEmpty(Collections.emptyList(), "Collection is {1}", "null", "empty");
  }

  @Test
  public void assertNotEmptyCollectionThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notEmpty(Collections.emptyList(), new AssertionFailedException("test"));
  }

  @Test
  public void assertNotEmptyMap() {
    Map<String, String> map = new HashMap<>(2);

    map.put("one", "1");
    map.put("two", "2");

    Assert.notEmpty(map, "Map is empty");
    Assert.notEmpty(Collections.singletonMap("myKey", "myValue"), "Map is empty");
  }

  @Test
  public void assertNotEmptyMapWithNull() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Map is null");
    Assert.notEmpty((Map) null, "Map is %1$s", "null", "empty");
  }

  @Test
  public void assertNotEmptyMapWithEmptyMap() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Map is empty");
    Assert.notEmpty(Collections.emptyMap(), "Map is {1}", "null", "empty");
  }

  @Test
  public void assertNotEmptyMapThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notEmpty(Collections.emptyMap(), new AssertionFailedException("test"));
  }

  @Test
  public void assertNotNull() throws Exception {
    Assert.notNull(false, "object reference is null");
    Assert.notNull('\0', "object reference is null");
    Assert.notNull(0, "object reference is null");
    Assert.notNull(0.0d, "object reference is null");
    Assert.notNull("nil", "object reference is null");
    Assert.notNull("null", "object reference is null");
    Assert.notNull(new Object(), "object reference is null");
    Assert.notNull(new Object[0], "object reference is null");
    Assert.notNull(Collections.emptyList(), "object reference is null");
    Assert.notNull(Void.class, "object reference is null");
  }

  @Test
  public void assertNotNullWithNull() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("expected non-null Object reference");
    Assert.notNull(null, "expected non-null {0} reference!", "Object reference");
  }

  @Test
  public void assertNotNullWithMethodReturningNull() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("the returnsNull() method returned null");
    Assert.notNull(returnsNull(), "the %1$s method returned {1}", "returnsNull()", null);
  }

  @Test
  public void assertNotNullThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.notNull(null, new AssertionFailedException("test"));
  }

  @Test
  public void assertSameWithIdenticalObjects() {
    Assert.same(null, null, "objects are not the same");
    Assert.same(true, Boolean.TRUE, "objects are not the same");
    Assert.same('c', 'c', "objects are not the same");
    Assert.same(1, 1, "objects are not the same");
    //Assert.same(Math.PI, Math.PI, "PI should be the same as PI!");
    Assert.same("test", "test", "objects are not the same");
    Assert.same(LOCK, LOCK, "objects are not the same");
  }

  @Test
  public void assertSameWithDifferentObjects() {
    expectedException.expect(IdentityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("character x is not the same as string x");
    Assert.same('x', "x", "character {0} is not the same as string %2$s", "x", "x");
  }

  @Test
  public void assetSameWithNullAndObject() {
    expectedException.expect(IdentityException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("null is not the same as Object");
    Assert.same(null, new Object(), "null is not the same as Object");
  }

  @Test
  public void assertSameThrowsAssertFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.same(new Object(), new Object(), new AssertionFailedException("test"));
  }

  @Test
  public void assertStateIsValid() {
    Assert.state(true, "state is invalid");
  }

  @Test
  public void assertStateIsInvalid() {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(object) state is invalid");
    Assert.state(false, "(%1$s) state is invalid", "object");
  }

  @Test
  public void assertStateWithNullCondition() {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(bean) state is invalid");
    Assert.state(null, "({0}) state is invalid", "bean");
  }

  @Test
  public void assertStateThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.state(false, new AssertionFailedException("test"));
  }

  @Test
  public void assertSupportedForSupportedOperation() {
    Assert.supported(true, "operation is unsupported");
  }

  @Test
  public void assertSupportedForUnsupportedOperation() {
    expectedException.expect(UnsupportedOperationException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(read) operation is unsupported");
    Assert.supported(false, "(%1$s) operation is unsupported", "read");
  }

  @Test
  public void assertSupportedWithNullCondition() {
    expectedException.expect(UnsupportedOperationException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(create) operation is unsupported");
    Assert.supported(null, "({0}) operation is unsupported", "create");
  }

  @Test
  public void assertSupportedThrowsAssertionFailedException() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");
    Assert.supported(false, new AssertionFailedException("test"));
  }

  protected enum TestEnum {
    ONE,
    TWO
  }

}
