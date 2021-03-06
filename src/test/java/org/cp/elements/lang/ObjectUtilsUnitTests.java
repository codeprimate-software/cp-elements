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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Unit Tests for {@link ObjectUtils}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ObjectUtils
 * @since 1.0.0
 */
public class ObjectUtilsUnitTests {

  @Test
  public void areAllNullReturnsTrue() {

    assertThat(ObjectUtils.areAllNull(new Object[] { null })).isTrue();
    assertThat(ObjectUtils.areAllNull(null, null, null)).isTrue();
  }

  @Test
  public void areAllNullReturnsTrueWhenArrayIsNull() {
    assertThat(ObjectUtils.areAllNull((Object[]) null)).isTrue();
  }

  @Test
  public void areAllNullReturnsTrueWhenArrayIsEmpty() {
    assertThat(ObjectUtils.areAllNull()).isTrue();
  }

  @Test
  public void areAllNullReturnsFalseWithNonNullValue() {

    assertThat(ObjectUtils.areAllNull(null, "null", null)).isFalse();
    assertThat(ObjectUtils.areAllNull("test", "testing", "tested")).isFalse();
  }

  @Test
  public void areAnyNullReturnsTrue() {

    assertThat(ObjectUtils.areAnyNull(new Object[] { null })).isTrue();
    assertThat(ObjectUtils.areAnyNull("test", null, "tested")).isTrue();
    assertThat(ObjectUtils.areAnyNull(null, "testing", null)).isTrue();
    assertThat(ObjectUtils.areAnyNull(null, null, null)).isTrue();
  }

  @Test
  public void areAnyNullReturnsFalseWhenArrayIsNull() {
    assertThat(ObjectUtils.areAnyNull((Object[]) null)).isFalse();
  }

  @Test
  public void areAnyNullReturnsFalseWhenArrayIsEmpty() {
    assertThat(ObjectUtils.areAnyNull()).isFalse();
  }

  @Test
  public void areAnyNullReturnsFalseWhenNoNullValuesExist() {

    assertThat(ObjectUtils.areAnyNull("test", "testing", "tested")).isFalse();
    assertThat(ObjectUtils.areAnyNull("test", "null", "nil")).isFalse();
    assertThat(ObjectUtils.areAnyNull("nil", "null")).isFalse();
    assertThat(ObjectUtils.areAnyNull("null")).isFalse();
  }

  @Test
  public void cloneWithCloneableObject() {

    CloneableObject<String> cloneableObject = new CloneableObject<>("test");
    CloneableObject<String> cloneableObjectClone = ObjectUtils.clone(cloneableObject);

    assertThat(cloneableObjectClone).isNotNull();
    assertThat(cloneableObjectClone).isNotSameAs(cloneableObject);
    assertThat(cloneableObjectClone).isEqualTo(cloneableObject);
  }

  @Test
  public void cloneWithCopyableObject() {

    CopyableObject<Object> copyableObject = new CopyableObject<>("test");
    CopyableObject<Object> copyableObjectClone = ObjectUtils.clone(copyableObject);

    assertThat(copyableObjectClone).isNotNull();
    assertThat(copyableObjectClone).isNotSameAs(copyableObject);
    assertThat(copyableObjectClone).isEqualTo(copyableObject);
  }

  @Test
  public void cloneWithAssignmentCompatibleArgumentTypeCopyConstructor() {

    SoftwareEngineer jonDoe = new SoftwareEngineer("Jon", "Doe");
    SoftwareEngineer jonDoeClone = ObjectUtils.clone(jonDoe);

    assertThat(jonDoeClone).isNotNull();
    assertThat(jonDoeClone).isNotSameAs(jonDoe);
    assertThat(jonDoeClone).isEqualTo(jonDoe);
  }

  @Test(expected = CloneException.class)
  public void cloneWithExceptionThrowingCopyConstructor() {

    try {
      ObjectUtils.clone(new ExceptionThrowingCopyConstructorObject<>("test"));
    }
    catch (CloneException expected) {

      assertThat(expected).hasMessage("[clone] using [copy constructor] was unsuccessful");
      assertThat(expected).hasCauseInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause().getCause()).hasMessage("test");
      assertThat(expected.getCause().getCause().getCause()).isNull();

      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void cloneWithNonCloneableNonCopyableObject() {

    try {
      ObjectUtils.clone(new Object());
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessageContaining("[clone] is not supported for object of type [Object]");
      assertThat(expected).hasCauseInstanceOf(CloneNotSupportedException.class);
      assertThat(expected.getCause()).hasMessage("[clone] is not supported for object of type [Object]");
      assertThat(expected.getCause().getCause()).isNull();

      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void cloneWithNullObject() {

    try {
      ObjectUtils.clone(null);
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessageContaining("[clone] is not supported for object of type [null]");
      assertThat(expected).hasCauseInstanceOf(CloneNotSupportedException.class);
      assertThat(expected.getCause()).hasMessage("[clone] is not supported for object of type [null]");
      assertThat(expected.getCause().getCause()).isNull();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void defaultIfNullWithNonNullValues() {

    assertThat(ObjectUtils.defaultIfNull("test", null, null, null)).isEqualTo("test");
    assertThat(ObjectUtils.defaultIfNull(null, "test")).isEqualTo("test");
    assertThat(ObjectUtils.defaultIfNull(null, null, null, "test")).isEqualTo("test");
    assertThat(ObjectUtils.defaultIfNull(null, "test", null, null)).isEqualTo("test");
    assertThat(ObjectUtils.defaultIfNull("null", (Object[]) null)).isEqualTo("null");
    assertThat(ObjectUtils.defaultIfNull("null", "test")).isEqualTo("null");
    assertThat(ObjectUtils.defaultIfNull("nil", "test")).isEqualTo("nil");
    assertThat(ObjectUtils.defaultIfNull("mock", "test")).isEqualTo("mock");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void defaultIfNullWithNullValues() {

    assertThat(ObjectUtils.defaultIfNull((Object[][]) null)).isNull();
    assertThat(ObjectUtils.defaultIfNull(null, (Object[]) null)).isNull();
    assertThat(ObjectUtils.defaultIfNull((Object[]) null, null, null)).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void doOperationSafelyReturnsValue() {
    assertThat(ObjectUtils.doOperationSafely(() -> "test")).isEqualTo("test");
  }

  @Test(expected = IllegalStateException.class)
  @SuppressWarnings("unchecked")
  public void doOperationSafelyThrowsIllegalStateException() {

    try {
      ObjectUtils.doOperationSafely(() -> { throw new Exception("test"); });
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessageStartingWith("Failed to execute operation");
      assertThat(expected).hasCauseInstanceOf(Exception.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void doOperationSafelyWithDefaultValueReturnsValue() {
    assertThat(ObjectUtils.doOperationSafely(() -> "test", "default")).isEqualTo("test");
  }

  @Test
  public void doOperationSafelyWithDefaultValueReturnsDefaultValue() {
    assertThat(ObjectUtils.doOperationSafely(() -> { throw new Exception("test"); }, "default"))
      .isEqualTo("default");
  }

  @Test(expected = IllegalStateException.class)
  @SuppressWarnings("unchecked")
  public void doOperationSafelyWithDefaultValueThrowsIllegalStateException() {

    try {
      ObjectUtils.doOperationSafely(() -> { throw new Exception("test"); }, (Object) null);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessageStartingWith("Failed to execute operation");
      assertThat(expected).hasCauseInstanceOf(Exception.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void returnValueOrDefaultIfNullReturnsValue() {
    assertThat(ObjectUtils.returnValueOrDefaultIfNull("value", "defaultValue")).isEqualTo("value");
  }

  @Test
  public void returnValueOrDefaultIfNullReturnsDefaultValue() {
    assertThat(ObjectUtils.returnValueOrDefaultIfNull(null, "defaultValue"))
      .isEqualTo("defaultValue");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void returnValueOrDefaultIfNullWithSupplierReturnsValue() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("supplier");

    assertThat(ObjectUtils.returnValueOrDefaultIfNull("value", mockSupplier)).isEqualTo("value");

    verify(mockSupplier, never()).get();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void returnValueOrDefaultIfNullWithSupplierReturnsSupplierValue() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("supplier");

    assertThat(ObjectUtils.returnValueOrDefaultIfNull(null, mockSupplier)).isEqualTo("supplier");

    verify(mockSupplier, times(1)).get();
  }

  @Test
  public void returnValueOrThrowIfNullWithNonNullValue() {

    assertThat(ObjectUtils.returnValueOrThrowIfNull("test", new NullPointerException("null"))).isEqualTo("test");
    assertThat(ObjectUtils.returnValueOrThrowIfNull("null", new NullPointerException("test"))).isEqualTo("null");
  }

  @Test(expected = IllegalArgumentException.class)
  public void returnValueOrThrowIfNullWithNullValue() {

    try {
      ObjectUtils.returnValueOrThrowIfNull(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value must not be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = NullPointerException.class)
  public void returnValueOrThrowIfNullWithNullValueUsingCustomRuntimeException() {

    try {
      ObjectUtils.returnValueOrThrowIfNull(null, new NullPointerException("Value is null"));
    }
    catch (NullPointerException expected) {

      assertThat(expected).hasMessage("Value is null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void returnValueOThrowIfNullWithNullValueAndNullRuntimeException() {

    try {
      ObjectUtils.returnValueOrThrowIfNull(null, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("RuntimeException must not be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void safeGetValueReturnsSupplierValue() {
    assertThat(ObjectUtils.safeGetValue(() -> "test", null)).isEqualTo("test");
  }

  @Test
  public void safeGetValueReturnsDefaultValue() {
    assertThat(ObjectUtils.safeGetValue(() -> { throw new RuntimeException("error"); }, "test"))
      .isEqualTo("test");
  }

  @Test
  public void safeGetValueReturnsNull() {
    assertThat(ObjectUtils.<Object>safeGetValue(() -> { throw new RuntimeException("error"); })).isNull();
  }

  @Test
  public void isNullOrEqualToReturnsTrueWhenObjectIsNull() {
    assertThat(ObjectUtils.isNullOrEqualTo(null, "test")).isTrue();
  }

  @Test
  public void isNullOrEqualToReturnsTrueWhenObjectsAreEqual() {
    assertThat(ObjectUtils.isNullOrEqualTo("test", "test")).isTrue();
  }

  @Test
  public void isNullOrEqualToReturnsFalseWhenObjectsAreNotEqual() {

    assertThat(ObjectUtils.isNullOrEqualTo("test", null)).isFalse();
    assertThat(ObjectUtils.isNullOrEqualTo("test", "mock")).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equals() {

    Object testObject = new Object();

    assertThat(ObjectUtils.equals(testObject, testObject)).isTrue();
    assertThat(ObjectUtils.equals(true, Boolean.TRUE)).isTrue();
    assertThat(ObjectUtils.equals('c', new Character('c'))).isTrue();
    assertThat(ObjectUtils.equals(1, new Integer(1))).isTrue();
    assertThat(ObjectUtils.equals(Math.PI, new Double(Math.PI))).isTrue();
    assertThat(ObjectUtils.equals("test", "test")).isTrue();
  }

  @Test
  public void equalsWithUnequalValues() {

    assertThat(ObjectUtils.equals(null, null)).isFalse();
    assertThat(ObjectUtils.equals("test", null)).isFalse();
    assertThat(ObjectUtils.equals(null, "test")).isFalse();
    assertThat(ObjectUtils.equals(null, "null")).isFalse();
    assertThat(ObjectUtils.equals("null", "nil")).isFalse();
    assertThat(ObjectUtils.equals(true, false)).isFalse();
    assertThat(ObjectUtils.equals('c', 'C')).isFalse();
    assertThat(ObjectUtils.equals('c', "c")).isFalse();
    assertThat(ObjectUtils.equals(-2, 2)).isFalse();
    assertThat(ObjectUtils.equals(3.14159d, Math.PI)).isFalse();
    assertThat(ObjectUtils.equals("mock", "proxy")).isFalse();
    assertThat(ObjectUtils.equals("test", "TEST")).isFalse();
    assertThat(ObjectUtils.equals("test", "testing")).isFalse();
    assertThat(ObjectUtils.equals("test", "tested")).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsIgnoreNull() {

    assertThat(ObjectUtils.equalsIgnoreNull(null, null)).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull("null", "null")).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull("nil", "nil")).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.FALSE)).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull('\0', '\0')).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull(0, -0)).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE + 1)).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull(Math.PI, Math.PI)).isTrue();
    assertThat(ObjectUtils.equalsIgnoreNull("testing", "testing")).isTrue();
  }

  @Test
  public void equalsIgnoreNullWithUnequalValues() {

    assertThat(ObjectUtils.equalsIgnoreNull(null, "null")).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull("null", null)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull("null", "nil")).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.TRUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull('\0', null)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(0, 1)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(Double.MIN_VALUE, Double.MAX_VALUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(0.0d, -0.0d)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull("gnitset", "testing")).isFalse();
  }

  @Test
  public void hashCodeWithNonNullValue() {
    assertThat(ObjectUtils.hashCode("test")).isEqualTo("test".hashCode());
  }

  @Test
  public void hashCodeWithNullValue() {
    assertThat(ObjectUtils.hashCode(null)).isEqualTo(0);
  }

  @Test
  public void toStringWithNull() {
    assertThat(ObjectUtils.toString(null)).isNull();
  }

  @Test
  public void toStringWithValues() {

    assertThat(ObjectUtils.toString(Boolean.TRUE)).isEqualTo("true");
    assertThat(ObjectUtils.toString('\0')).isEqualTo("\0");
    assertThat(ObjectUtils.toString('c')).isEqualTo("c");
    assertThat(ObjectUtils.toString(8192)).isEqualTo("8192");
    assertThat(ObjectUtils.toString(3.14159d)).isEqualTo("3.14159");
    assertThat(ObjectUtils.toString("test")).isEqualTo("test");
    assertThat(ObjectUtils.toString("null")).isEqualTo("null");
    assertThat(ObjectUtils.toString("nil")).isEqualTo("nil");
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
    @SuppressWarnings("rawtypes")
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
    @SuppressWarnings("all")
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

    public Person(String firstName, String lastName) {
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public Person(Person person) {
      this(person.getFirstName(), person.getLastName());
    }

    public String getFirstName() {
      return this.firstName;
    }

    public String getLastName() {
      return this.lastName;
    }

    @Override
    public boolean equals(Object obj) {

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
