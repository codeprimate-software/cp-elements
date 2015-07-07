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

package org.cp.elements.lang;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;

import org.junit.Test;

/**
 * The ObjectUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ObjectUtils class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.ObjectUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ObjectUtilsTest {

  @Test
  public void cloneWithCloneableObject() {
    CloneableObject<String> cloneableObject = new CloneableObject<String>("test");
    CloneableObject<String> cloneableObjectClone = ObjectUtils.clone(cloneableObject);

    assertThat(cloneableObjectClone, is(not(nullValue())));
    assertThat(cloneableObjectClone, is(not(sameInstance(cloneableObject))));
    assertThat(cloneableObjectClone, is(equalTo(cloneableObject)));
  }

  @Test
  public void cloneWithCopyableObject() {
    CopyableObject<Object> copyableObject = new CopyableObject<Object>("test");
    CopyableObject<Object> copyableObjectClone = ObjectUtils.clone(copyableObject);

    assertThat(copyableObjectClone, is(not(nullValue())));
    assertThat(copyableObjectClone, is(not(sameInstance(copyableObject))));
    assertThat(copyableObjectClone, is(equalTo(copyableObject)));
  }

  @Test
  public void cloneWithAssignmentCompatibleArgumentTypeCopyConstructor() {
    SoftwareEngineer jonDoe = new SoftwareEngineer("Jon", "Doe");
    SoftwareEngineer jonDoeClone = ObjectUtils.clone(jonDoe);

    assertThat(jonDoeClone, is(not(nullValue())));
    assertThat(jonDoeClone, is(not(sameInstance(jonDoe))));
    assertThat(jonDoeClone, is(equalTo(jonDoe)));
  }

  @Test(expected = CloneException.class)
  public void cloneWithExceptionThrowingCopyConstructor() {
    try {
      ObjectUtils.clone(new ExceptionThrowingCopyConstructorObject<String>("test"));
    }
    catch (CloneException expected) {
      assertThat(expected.getMessage(), is(equalTo("'clone' using 'copy constructor' was unsuccessful")));
      assertThat(expected.getCause(), is(instanceOf(InvocationTargetException.class)));
      assertThat(expected.getCause().getCause(), is(instanceOf(IllegalArgumentException.class)));
      assertThat(expected.getCause().getCause().getMessage(), is(equalTo("test")));
      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void cloneWithNonCloneableNonCopyableObject() {
    try {
      ObjectUtils.clone(new Object());
    }
    catch (UnsupportedOperationException expected) {
      assertThat(expected.getMessage(), containsString("'clone' is not supported for (Object) value"));
      assertThat(expected.getCause(), is(instanceOf(CloneNotSupportedException.class)));
      assertThat(expected.getCause().getMessage(), is(equalTo("'clone' is not supported for (Object) value")));
      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void cloneWithNullObject() {
    try {
      ObjectUtils.clone(null);
    }
    catch (UnsupportedOperationException expected) {
      assertThat(expected.getMessage(), containsString("'clone' is not supported for (null) value"));
      assertThat(expected.getCause(), is(instanceOf(CloneNotSupportedException.class)));
      assertThat(expected.getCause().getMessage(), is(equalTo("'clone' is not supported for (null) value")));
      throw expected;
    }
  }

  @Test
  public void defaultIfNullWithNonNullValues() {
    assertEquals("test", ObjectUtils.defaultIfNull("test", null, null, null));
    assertEquals("test", ObjectUtils.defaultIfNull(null, "test"));
    assertEquals("test", ObjectUtils.defaultIfNull(null, null, null, "test"));
    assertEquals("test", ObjectUtils.defaultIfNull(null, null, "test", null));
    assertEquals("null", ObjectUtils.defaultIfNull("null", (Object[]) null));
    assertEquals("mock", ObjectUtils.defaultIfNull("mock", "test"));
    assertEquals("nil", ObjectUtils.defaultIfNull("nil", "test"));
    assertEquals("null", ObjectUtils.defaultIfNull("null", "test"));
  }

  @Test
  public void defaultIfNullWithNullValues() {
    assertNull(ObjectUtils.defaultIfNull((Object[]) null));
    assertNull(ObjectUtils.defaultIfNull(null, (Object[]) null));
    assertNull(ObjectUtils.defaultIfNull(null, null, null));
  }

  @Test
  @SuppressWarnings("boxing")
  public void equals() {
    Object testObject = new Object();
    assertTrue(ObjectUtils.equals(testObject, testObject));
    assertTrue(ObjectUtils.equals(true, Boolean.TRUE));
    assertTrue(ObjectUtils.equals('c', new Character('c')));
    assertTrue(ObjectUtils.equals(1, new Integer(1)));
    assertTrue(ObjectUtils.equals(Math.PI, new Double(Math.PI)));
    assertTrue(ObjectUtils.equals("test", "test"));
  }

  @Test
  public void equalsWithUnequalValues() {
    assertFalse(ObjectUtils.equals(null, null));
    assertFalse(ObjectUtils.equals("test", null));
    assertFalse(ObjectUtils.equals(null, "test"));
    assertFalse(ObjectUtils.equals(null, "null"));
    assertFalse(ObjectUtils.equals("null", "nil"));
    assertFalse(ObjectUtils.equals(true, false));
    assertFalse(ObjectUtils.equals('c', 'C'));
    assertFalse(ObjectUtils.equals('c', "c"));
    assertFalse(ObjectUtils.equals(-2, 2));
    assertFalse(ObjectUtils.equals(3.14159d, Math.PI));
    assertFalse(ObjectUtils.equals("mock", "proxy"));
    assertFalse(ObjectUtils.equals("test", "TEST"));
    assertFalse(ObjectUtils.equals("test", "testing"));
    assertFalse(ObjectUtils.equals("test", "tested"));
  }

  @Test
  public void equalsIgnoreNull() {
    assertTrue(ObjectUtils.equalsIgnoreNull(null, null));
    assertTrue(ObjectUtils.equalsIgnoreNull("null", "null"));
    assertTrue(ObjectUtils.equalsIgnoreNull("nil", "nil"));
    assertTrue(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.FALSE));
    assertTrue(ObjectUtils.equalsIgnoreNull('\0', '\0'));
    assertTrue(ObjectUtils.equalsIgnoreNull(0, -0));
    assertTrue(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE + 1));
    assertTrue(ObjectUtils.equalsIgnoreNull(Math.PI, Math.PI));
    assertTrue(ObjectUtils.equalsIgnoreNull("testing", "testing"));
  }

  @Test
  public void equalsIgnoreNullWithUnequalValues() {
    assertFalse(ObjectUtils.equalsIgnoreNull(null, "null"));
    assertFalse(ObjectUtils.equalsIgnoreNull("null", null));
    assertFalse(ObjectUtils.equalsIgnoreNull("null", "nil"));
    assertFalse(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.TRUE));
    assertFalse(ObjectUtils.equalsIgnoreNull('\0', null));
    assertFalse(ObjectUtils.equalsIgnoreNull(0, 1));
    assertFalse(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE));
    assertFalse(ObjectUtils.equalsIgnoreNull(Double.MIN_VALUE, Double.MAX_VALUE));
    assertFalse(ObjectUtils.equalsIgnoreNull(0.0d, -0.0d));
    assertFalse(ObjectUtils.equalsIgnoreNull("gnitset", "testing"));
  }

  @Test
  public void hashCodeWithNonNullValue() {
    assertEquals("test".hashCode(), ObjectUtils.hashCode("test"));
  }

  @Test
  public void hashCodeWithNullValue() {
    assertEquals(0, ObjectUtils.hashCode(null));
  }

  @Test
  public void toStringWithValues() {
    assertNull(ObjectUtils.toString(null));
    assertEquals("true", ObjectUtils.toString(Boolean.TRUE));
    assertEquals("\0", ObjectUtils.toString('\0'));
    assertEquals("c", ObjectUtils.toString('c'));
    assertEquals("8192", ObjectUtils.toString(8192));
    assertEquals("3.14159", ObjectUtils.toString(3.14159d));
    assertEquals("test", ObjectUtils.toString("test"));
    assertEquals("null", ObjectUtils.toString("null"));
    assertEquals("nil", ObjectUtils.toString("nil"));
  }

  @SuppressWarnings("unused")
  protected static class CopyableObject<T> {

    private final T value;

    protected CopyableObject() {
      this.value = null;
    }

    public CopyableObject(final T value) {
      this.value = value;
    }

    public CopyableObject(final CopyableObject<T> copyableObject) {
      this(copyableObject.getValue());
    }

    public T getValue() {
      return value;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof CopyableObject)) {
        return false;
      }

      CopyableObject that = (CopyableObject) obj;

      return ObjectUtils.equals(this.getValue(), that.getValue());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getValue());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.valueOf(getValue());
    }
  }

  protected static class CloneableObject<T> extends CopyableObject<T> implements Cloneable {

    protected CloneableObject(final T value) {
      super(value);
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
      return new CloneableObject<T>(getValue());
    }
  }

  protected static class ExceptionThrowingCopyConstructorObject<T> extends CopyableObject<T> {

    public ExceptionThrowingCopyConstructorObject(final T value) {
      super(value);
    }

    @SuppressWarnings("unused")
    public ExceptionThrowingCopyConstructorObject(final ExceptionThrowingCopyConstructorObject<T> copyableObject) {
      throw new IllegalArgumentException("test");
    }
  }

  protected static abstract class Person {

    private final String firstName;
    private final String lastName;

    public Person(final String firstName, final String lastName) {
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public Person(final Person person) {
      this(person.getFirstName(), person.getLastName());
    }

    public String getFirstName() {
      return firstName;
    }

    public String getLastName() {
      return lastName;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        && ObjectUtils.equals(this.getLastName(), that.getLastName());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getFirstName());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getLastName());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }
  }

  protected static class SoftwareEngineer extends Person {

    public SoftwareEngineer(final String firstName, final String lastName) {
      super(firstName, lastName);
    }

    @SuppressWarnings("unused")
    public SoftwareEngineer(final Person person) {
      super(person);
    }
  }

}
