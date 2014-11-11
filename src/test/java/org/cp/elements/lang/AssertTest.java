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

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

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

  @Test
  public void testArgument() {
    Assert.argument(true, "The argument is valid!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testArgumentInvalid() {
    try {
      Assert.argument(false, "The argument ({0}) is invalid!", "TEST");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The argument (TEST) is invalid!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testArgumentInvalidThrowsAssertionFailedException() {
    Assert.argument(false, new AssertionFailedException());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testArgumentInvalidThrowsIllegalArgumentExceptionWithMixedMessage() {
    try {
      Assert.argument(false, "The argument (%1$s) is a {1} argument, a %2$s, {1}, %2$s argument, just plain {2}, {3}!",
        "TEST", "bad", "terrible", "horrible");
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The argument (TEST) is a bad argument, a bad, bad, bad argument, just plain terrible, horrible!",
        expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testAssertEquals() {
    Assert.equals(Boolean.TRUE, true, "The Boolean values are not equal!");
    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4),
      TestUtils.createCalendar(2011, Calendar.OCTOBER, 4), "The Calendars are not equal!");
    Assert.equals(TestUtils.createCalendar(2013, Calendar.JANUARY, 13),
      TestUtils.createCalendar(2013, Calendar.JANUARY, 13), "The Calendars are not equal!");
    Assert.equals(new Character('c'), 'c', "The Characters are not equal!");
    Assert.equals(new Double(Math.PI), Math.PI, "The Double values are not equal!");
    Assert.equals(new Integer(2), 2, "The Integer values are not equal!");
    Assert.equals("TEST", "TEST", "The Strings are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalBooleanValues() {
    Assert.equals(Boolean.TRUE, false, "The Boolean values are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalCalendars() {
    Assert.equals(TestUtils.createCalendar(2011, Calendar.OCTOBER, 4), Calendar.getInstance(),
      "The Calendars are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalCharacters() {
    Assert.equals('a', 'A', "The Characters are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalDoubles() {
    Assert.equals(3.14159d, Math.PI, "The Double values are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalIntegers() {
    Assert.equals(-1, 1, "The Integer values are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithUnequalStrings() {
    Assert.equals("TEST", "test", "The Strings are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithNullValues() {
    Assert.equals(null, null, "Null values are not equal!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsWithOneNullValue() {
    Assert.equals(null, "null", "A 'null' String is not equal to a null value!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertEqualsFormatsMessage() {
    try {
      Assert.equals(true, false, "Expected ({0}); but was ({1})!", true, false);
    }
    catch (AssertionFailedException e) {
      assertEquals("Expected (true); but was (false)!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertEqualsThrowsIllegalArgumentException() {
    Assert.equals(true, false, new IllegalArgumentException());
  }

  @Test
  public void testAssertHoldsLock() {
    synchronized (LOCK) {
      Assert.holdsLock(LOCK, "The current Thread does not hold the lock!");
    }
  }


  @Test(expected = IllegalMonitorStateException.class)
  public void testAssertHoldsLockWhenLockNotHeld() {
    final String currentThreadName = Thread.currentThread().getName();

    try {
      Assert.holdsLock(LOCK, "The current Thread ({0}) does not hold lock ({1})!", currentThreadName, LOCK);
    }
    catch (IllegalMonitorStateException e) {
      assertEquals("The current Thread (" + currentThreadName + ") does not hold lock (" + LOCK + ")!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertHoldsLockThrowsAssertionFailedException() {
    Assert.holdsLock(LOCK, new AssertionFailedException());
  }

  @Test
  public void testAssertIsAssignableTo() {
    Assert.isAssignableTo(Boolean.class, Boolean.class, "The class type is not assignable to a Boolean!");
    Assert.isAssignableTo(Character.class, Object.class, "The class type is not assignable to Object!");
    Assert.isAssignableTo(java.sql.Date.class, java.util.Date.class, "The class type is not assignable to java.util.Date!");
    Assert.isAssignableTo(Double.class, Number.class, "The class type is not assignable to Number!");
    Assert.isAssignableTo(Integer.class, Number.class, "The class type is not assignable to Number!");
    Assert.isAssignableTo(String.class, Object.class, "The class type is not assignable to Object!");
  }

  @Test(expected = ClassCastException.class)
  public void testAssertIsAssignableToCastingObjectToString() {
    try {
      Assert.isAssignableTo(Object.class, String.class, "{0} is not assignable to a reference of type {1}!",
        "Object", "String");
    }
    catch (ClassCastException e) {
      assertEquals("Object is not assignable to a reference of type String!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = ClassCastException.class)
  public void testAssertIsAssignableToWithIncompatibleClassTypes() {
    try {
      Assert.isAssignableTo(Integer.class, Boolean.class, "{0} is not type compatible with {1}!",
        "Integer", "Boolean");
    }
    catch (ClassCastException e) {
      assertEquals("Integer is not type compatible with Boolean!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertIsAssignableToThrowsAssertionFailedException() {
    Assert.isAssignableTo(Calendar.class, Date.class, new AssertionFailedException());
  }

  @Test
  public void testAssertIsFalse() {
    Assert.isFalse(false, "The value is not false!");
    Assert.isFalse(Boolean.FALSE, "The value is not false!");
    Assert.isFalse(!Boolean.TRUE, "The value is not false!");
    Assert.isFalse(new Object() == new Object(), "An object compared for identity with another object is false!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertIsFalseWithTrue() {
    try {
      Assert.isFalse(true, "{0} is not {1}!", "True", "false");
    }
    catch (IllegalArgumentException e) {
      assertEquals("True is not false!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertIsFalseThrowsAssertionFailedException() {
    Assert.isFalse(Boolean.TRUE, new AssertionFailedException());
  }

  @Test
  public void testAssertIsInstanceOf() throws Exception {
    Assert.isInstanceOf(true, Boolean.class, "The Object is not an instance of the Boolean class!");
    Assert.isInstanceOf('c', Character.class, "The Object is not an instance of the Character class!");
    Assert.isInstanceOf(3.14f, Float.class, "The Object is not an instance of the Float class!");
    Assert.isInstanceOf(Math.PI, Double.class, "The Object is not an instance of the Double class!");
    Assert.isInstanceOf(0, Integer.class, "The Object is not an instance of the Integer class!");
    Assert.isInstanceOf(1l, Long.class, "The Object is not an instance of the Long class!");
    Assert.isInstanceOf("test", CharSequence.class, "The Object is not an instance of the CharSequence class!");
    Assert.isInstanceOf(new Object(), Object.class, "The Object is not an instance of the Object class!");
    Assert.isInstanceOf(Object.class, Class.class, "The Object is not an instance of the Class class!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertIsInstanceOfIsNotAnInstance() {
    try {
      Assert.isInstanceOf("0123456789", Long.class, "A {0} is not an instance of the {1} class!",
        "String", "Long");
    }
    catch (IllegalArgumentException e) {
      assertEquals("A String is not an instance of the Long class!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertIsInstanceOfThrowsAssertionFailedException() {
    Assert.isInstanceOf(new Object(), Class.class, new AssertionFailedException());
  }

  @Test
  public void testAssertIsTrue() {
    Assert.isTrue(true, "The value is not true!");
    Assert.isTrue(Boolean.TRUE, "The value is not true!");
    Assert.isTrue(!Boolean.FALSE, "The value is not true!");
    Assert.isTrue("test".equals("test"), "The value is not true!");
    Assert.isTrue("test".equalsIgnoreCase("TEST"), "The value is not true!");
    Assert.isTrue(LOCK.equals(LOCK), "An Object is equal to itself!");
    Assert.isTrue(LOCK == LOCK, "An identity comparison of an Object with itself is true!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertIsTrueWithFalse() {
    try {
      Assert.isTrue(false, "{0} is not {1}!", "False", "true");
    }
    catch (IllegalArgumentException e) {
      assertEquals("False is not true!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertIsTrueThrowsAssertionFailedException() {
    Assert.isTrue(Boolean.FALSE, new AssertionFailedException());
  }

  @Test
  public void testAssertNotBlank() throws Exception {
    Assert.notBlank("test", "The String is blank!");
    Assert.notBlank("blank", "The String is blank!");
    Assert.notBlank("empty", "The String is blank!");
    Assert.notBlank("null", "The String is blank!");
    Assert.notBlank("space", "The String is blank!");
    Assert.notBlank("_", "The String is blank!");
    Assert.notBlank("--", "The String is blank!");
    Assert.notBlank("0", "The String is blank!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotBlankWithEmptyString() {
    Assert.notBlank("", "The empty String is blank!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotBlankWithNull() {
    Assert.notBlank(null, "Null is blank!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotBlankWithNullCharacter() {
    Assert.notBlank("\0", "The null Character is blank!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotBlankWithSpaces() {
    Assert.notBlank("  ", "Spaces is blank!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotBlankWithTabs() {
    try {
      Assert.notBlank("\t", "The {0} is blank!", "tab character");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The tab character is blank!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotBlankWithNewlineThrowsAssertionFailedException() {
    Assert.notBlank("\n", new AssertionFailedException("The newline character is blank!"));
  }

  @Test
  public void testAssertNotEmptyStrings() {
    Assert.notEmpty((String) null, "The String is empty!");
    Assert.notEmpty("blank", "The String is empty!");
    Assert.notEmpty("empty", "The String is empty!");
    Assert.notEmpty("null", "The String 'null' is not empty!");
    Assert.notEmpty(" ", "The String is empty!");
    Assert.notEmpty("   ", "The String is empty!");
    Assert.notEmpty("_", "The String is empty!");
    Assert.notEmpty("___", "The String is empty!");
    Assert.notEmpty("\0", "The String is empty!");
    Assert.notEmpty("\n", "The String is empty!");
    Assert.notEmpty("\t", "The String is empty!");
    Assert.notEmpty("test", "The String is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyWithEmptyString() {
    Assert.notEmpty("", "The String is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyStringUsingMessage() {
    try {
      Assert.notEmpty("", "The {0} String is empty!", "empty");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The empty String is empty!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotEmptyStringThrowsAssertionFailedException() {
    Assert.notEmpty("", new AssertionFailedException());
  }

  @Test
  public void testAssertNotEmptyArray() {
    Assert.notEmpty(new Object[] { "assert", "mock", "test" }, "The Object array is empty!");
    Assert.notEmpty(new String[1], "The Object array is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyArrayWithNull() {
    Assert.notEmpty((Object[]) null, "A null Object array reference is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyArrayWithEmptyArray() {
    Assert.notEmpty(new Object[0], "A zero element Object array is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyArrayUsingMessage() {
    try {
      Assert.notEmpty(new Object[0], "The {0} is empty!", "Object array");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The Object array is empty!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotEmptyArrayThrowsAssertionFailedException() {
    Assert.notEmpty(new Object[0], new AssertionFailedException());
  }

  @Test
  public void testAssertNotEmptyCollection() {
    final Collection<String> animals = new ArrayList<String>(3);

    animals.add("bat");
    animals.add("cat");
    animals.add("dog");

    assertNotNull(animals);
    assertFalse(animals.isEmpty());

    Assert.notEmpty(animals, "The Collection is empty!");
    Assert.notEmpty(Collections.singleton(1), "The Collection is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyCollectionWithNull() {
    Assert.notEmpty((Collection) null, "A null Collection reference is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyCollectionWithEmptyCollection() {
    Assert.notEmpty(Collections.emptyList(), "The List is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyCollectionUsingMessage() {
    try {
      Assert.notEmpty(Collections.emptySet(), "The {0} is empty!", "Set");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The Set is empty!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotEmptyCollectionThrowsAssertionFailedException() {
    Assert.notEmpty(Collections.emptyList(), new AssertionFailedException());
  }

  @Test
  public void testAssertNotEmptyMap() {
    final Map<String, String> map = new HashMap<String, String>(1);

    map.put("key", "value");

    assertNotNull(map);
    assertFalse(map.isEmpty());

    Assert.notEmpty(map, "The Map is empty!");
    Assert.notEmpty(Collections.singletonMap("myKey", "myValue"), "The Map is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyMapWithNull() {
    Assert.notEmpty((Map) null, "A null Map reference is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyMapWithEmptyMap() {
    Assert.notEmpty(Collections.emptyMap(), "The Map is empty!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertNotEmptyMapUsingMessage() {
    try {
      Assert.notEmpty(Collections.emptyMap(), "The {0} is empty!", "Map");
    }
    catch (IllegalArgumentException e) {
      assertEquals("The Map is empty!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotEmptyMapThrowsAssertionFailedException() {
    Assert.notEmpty(Collections.emptyMap(), new AssertionFailedException());
  }

  @Test
  public void testAssertNotNull() throws Exception {
    Assert.notNull("nil", "The Object reference is null!");
    Assert.notNull("null", "The Object reference is null!");
    Assert.notNull('\0', "The Object reference is null!");
    Assert.notNull(new Object(), "The Object reference is null!");
    Assert.notNull(new Object[0], "The Object reference is null!");
  }

  @Test(expected = NullPointerException.class)
  public void testAssertNotNullWithNullObjectReference() {
    try {
      Assert.notNull(null, "The {0} is null!", "Object reference");
    }
    catch (NullPointerException e) {
      assertEquals("The Object reference is null!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertNotNullThrowsAssertionFailedException() {
    Assert.notNull(null, new AssertionFailedException());
  }

  @Test
  public void testAssertSame() {
    Assert.same(null, null, "The Objects are not the same!");
    Assert.same(true, Boolean.TRUE, "The Objects are not the same!");
    Assert.same('c', 'c', "The Objects are not the same!");
    Assert.same(1, 1, "The Objects are not the same!");
    //Assert.same(Math.PI, Math.PI, "PI should be the same as PI!");
    Assert.same("test", "test", "The Objects are not the same!");
    Assert.same(LOCK, LOCK, "The Objects are not the same!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertSameWithDifferentObjects() {
    Assert.same("test", new String("test"), "The literal String 'test' is not the same as new String 'test'!");
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssetSameUsingMessage() {
    try {
      Assert.same(new Object(), new Object(), "A new {0} is not the same as a new {1}!", "Object", "Object");
    }
    catch (AssertionFailedException e) {
      assertEquals("A new Object is not the same as a new Object!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertSameThrowsIllegalArgumentException() {
    Assert.same(new Object(), new Object(), new IllegalArgumentException());
  }

  @Test
  public void testAssertState() {
    Assert.state(true, "The state is not valid!");
  }

  @Test(expected = IllegalStateException.class)
  public void testAssertStateIsNotValid() {
    Assert.state(false, "The state is not valid!");
  }

  @Test(expected = IllegalStateException.class)
  public void testAssertStateUsingMessage() {
    try {
      Assert.state(null, "The {0} has not been properly initialized!", "Object");
    }
    catch (IllegalStateException e) {
      assertEquals("The Object has not been properly initialized!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = AssertionFailedException.class)
  public void testAssertStateThrowsAssertionFailedException() {
    Assert.state(false, new AssertionFailedException());
  }

}
