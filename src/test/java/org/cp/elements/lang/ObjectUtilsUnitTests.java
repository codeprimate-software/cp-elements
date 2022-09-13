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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.Serializable;
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

  @Test
  public void cloneWithSerializableObject() {

    Message message = new Message("TEST");
    Message messageClone = ObjectUtils.clone(message);

    assertThat(messageClone).isNotNull();
    assertThat(messageClone).isNotSameAs(message);
    assertThat(messageClone).isEqualTo(message);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void cloneWithNonCloneableNonCopyableNonSerializableObject() {

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
  public void doOperationSafelyReturnsResult() {
    assertThat(ObjectUtils.<String>doOperationSafely(arguments -> "test")).isEqualTo("test");
  }

  @Test(expected = IllegalStateException.class)
  public void doOperationSafelyThrowsIllegalStateException() {

    try {
      ObjectUtils.doOperationSafely(arguments -> { throw new Exception("ERROR"); });
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessageStartingWith("Failed to execute operation");
      assertThat(expected).hasCauseInstanceOf(Exception.class);
      assertThat(expected.getCause()).hasMessage("ERROR");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void doOperationSafelyWithDefaultValueReturnsResult() {
    assertThat(ObjectUtils.doOperationSafely(arguments -> "test", "default")).isEqualTo("test");
  }

  @Test
  public void doOperationSafelyWithDefaultValueReturnsDefaultValue() {
    assertThat(ObjectUtils.doOperationSafely(arguments -> { throw new Exception("ERROR"); }, "default"))
      .isEqualTo("default");
  }

  @Test(expected = IllegalStateException.class)
  public void doOperationSafelyWithDefaultValueThrowsIllegalStateException() {

    try {
      ObjectUtils.doOperationSafely(arguments -> { throw new Exception("ERROR"); }, (Object) null);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessageStartingWith("Failed to execute operation");
      assertThat(expected).hasCauseInstanceOf(Exception.class);
      assertThat(expected.getCause()).hasMessage("ERROR");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void doOperationSafelyWithSuppliedValueReturnsResult() {
    assertThat(ObjectUtils.<Object>doOperationSafely(arguments -> "test", () -> "supplied")).isEqualTo("test");
  }

  @Test
  public void doOperationSafelyWithSuppliedValueReturnsSuppliedValue() {
    assertThat(ObjectUtils.<Object>doOperationSafely(arguments -> { throw new Exception("ERROR"); }, () -> "supplied"))
      .isEqualTo("supplied");
  }

  @Test(expected = IllegalStateException.class)
  public void doOperationSafelyWithSuppliedValueThrowsIllegalStateException() {

    try {
      ObjectUtils.<Object>doOperationSafely(arguments -> { throw new Exception("ERROR"); }, () -> null);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessageStartingWith("Failed to execute operation");
      assertThat(expected).hasCauseInstanceOf(Exception.class);
      assertThat(expected.getCause()).hasMessage("ERROR");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void doOperationSafelyWithFunctionReturnsResult() {
    assertThat(ObjectUtils.<Object>doOperationSafely(arguments -> "test",
      cause -> { throw new RuntimeException("ERROR", cause); })).isEqualTo("test");
  }

  @Test
  public void doOperationSafelyWithFunctionReturnsFunctionResult() {
    assertThat(ObjectUtils.<Object>doOperationSafely(arguments -> { throw new Exception("ERROR"); },
      cause -> "functionResult" )).isEqualTo("functionResult");
  }

  @Test(expected = TestException.class)
  public void doOperationSafelyWithFunctionThrowsFunctionException() {

    try {
      ObjectUtils.<Object>doOperationSafely(arguments -> { throw new Error("ERROR"); },
        cause -> { throw new TestException("TEST EXCEPTION", cause); });
    }
    catch (RuntimeException expected) {

      assertThat(expected).isInstanceOf(TestException.class);
      assertThat(expected).hasMessage("TEST EXCEPTION");
      assertThat(expected).hasCauseInstanceOf(Error.class);
      assertThat(expected.getCause()).hasMessage("ERROR");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void requireObjectWithNonNullObject() {

    Object object = new Object();

    assertThat(ObjectUtils.requireObject(object, "Object is required")).isSameAs(object);
  }

  @Test
  public void requireObjectWithNullObjectThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ObjectUtils.requireObject(null, "This is a %s message!", "test"))
      .withMessage("This is a test message!")
      .withNoCause();
  }

  @Test
  public void requireObjectWithNullObjectThrowsIllegalArgumentExceptionDefaultsMessageArguments() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ObjectUtils.requireObject(null, "Object [%s] must not be null!"))
      .withMessage("Object [null] must not be null!")
      .withNoCause();
  }

  @Test
  public void requireStateWithNonNullObject() {

    Object object = new Object();

    assertThat(ObjectUtils.requireState(object, "Object state is required")).isSameAs(object);
  }

  @Test
  public void requireStateWithNullObjectThrowsIllegalStateException() {

    assertThatIllegalStateException()
      .isThrownBy(() -> ObjectUtils.requireState(null, "This is a %s message!", "test"))
      .withMessage("This is a test message!")
      .withNoCause();
  }

  @Test
  public void requireStateWithNullObjectThrowsIllegalStateExceptionDefaultsMessageArguments() {

    assertThatIllegalStateException()
      .isThrownBy(() -> ObjectUtils.requireState(null, "Object state [%s] is not valid!"))
      .withMessage("Object state [null] is not valid!")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  // The SuppressWarnings annotation is needed to suppress compiler warnings involving unchecked generic array creation
  // for varargs parameter.
  public void returnFirstNonNullValue() {

    assertThat(ObjectUtils.returnFirstNonNullValue("test", null, null, null)).isEqualTo("test");
    assertThat(ObjectUtils.returnFirstNonNullValue(null, "test")).isEqualTo("test");
    assertThat(ObjectUtils.returnFirstNonNullValue(null, null, null, "test")).isEqualTo("test");
    assertThat(ObjectUtils.returnFirstNonNullValue(null, "test", null, null)).isEqualTo("test");
    assertThat(ObjectUtils.returnFirstNonNullValue("null", (Object[]) null)).isEqualTo("null");
    assertThat(ObjectUtils.returnFirstNonNullValue("null", "test")).isEqualTo("null");
    assertThat(ObjectUtils.returnFirstNonNullValue("nil", "test")).isEqualTo("nil");
    assertThat(ObjectUtils.returnFirstNonNullValue("mock", "test")).isEqualTo("mock");
  }

  @Test
  @SuppressWarnings("unchecked")
  // The SuppressWarnings annotation is needed to suppress compiler warnings involving unchecked generic array creation
  // for varargs parameter.
  public void returnFirstNonNullValueWithNullValues() {

    assertThat(ObjectUtils.returnFirstNonNullValue((Object[][]) null)).isNull();
    assertThat(ObjectUtils.returnFirstNonNullValue(null, (Object[]) null)).isNull();
    assertThat(ObjectUtils.returnFirstNonNullValue((Object[]) null, null, null)).isNull();
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
  @SuppressWarnings("all")
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
  @SuppressWarnings("all")
  public void equalsWithNullArgumentsIsNullSafe() {

    assertThat(ObjectUtils.equals(null, null)).isFalse();
    assertThat(ObjectUtils.equals(null, "test")).isFalse();
    assertThat(ObjectUtils.equals(null, "null")).isFalse();
    assertThat(ObjectUtils.equals(null, "nil")).isFalse();
    assertThat(ObjectUtils.equals("test", null)).isFalse();
  }

  @Test
  public void equalsWithUnequalValues() {

    assertThat(ObjectUtils.equals(true, false)).isFalse();
    assertThat(ObjectUtils.equals('c', 'C')).isFalse();
    assertThat(ObjectUtils.equals('c', "c")).isFalse();
    assertThat(ObjectUtils.equals(-2, 2)).isFalse();
    assertThat(ObjectUtils.equals(3.14159d, Math.PI)).isFalse();
    assertThat(ObjectUtils.equals("mock", "proxy")).isFalse();
    assertThat(ObjectUtils.equals("null", "nil")).isFalse();
    assertThat(ObjectUtils.equals("test", "TEST")).isFalse();
    assertThat(ObjectUtils.equals("test", "testing")).isFalse();
    assertThat(ObjectUtils.equals("testing", "tested")).isFalse();
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
  @SuppressWarnings("all")
  public void equalsIgnoreNullWithSingleNullValue() {

    assertThat(ObjectUtils.equalsIgnoreNull(null, "test")).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(null, "null")).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(null, "nil")).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull("null", null)).isFalse();
  }

  @Test
  public void equalsIgnoreNullWithUnequalValues() {

    assertThat(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.TRUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull('\0', null)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(0, 1)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(Double.MIN_VALUE, Double.MAX_VALUE)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull(0.0d, -0.0d)).isFalse();
    assertThat(ObjectUtils.equalsIgnoreNull("null", "nil")).isFalse();
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
  public void hashCodeOfPerson() {

    Person person = Person.as("Jon", "Doe");

    assertThat(ObjectUtils.hashCodeOf(person.getFirstName(), person.getLastName())).isEqualTo(person.hashCode());
  }

  @Test
  public void hashCodeOfEmptyArray() {
    assertThat(ObjectUtils.hashCodeOf()).isEqualTo(17);
  }

  @Test
  public void hashCodeOfNullArrayIsNullSafe() {
    assertThat(ObjectUtils.hashCodeOf((Object[]) null)).isEqualTo(17);
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

    public CopyableObject(T value) {
      this.value = value;
    }

    public CopyableObject(CopyableObject<T> copyableObject) {
      this(copyableObject.getValue());
    }

    public T getValue() {
      return this.value;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public boolean equals(Object obj) {

      if (this == obj) {
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

    protected CloneableObject(T value) {
      super(value);
    }

    @Override
    @SuppressWarnings("all")
    public Object clone() throws CloneNotSupportedException {
      return new CloneableObject<T>(getValue());
    }
  }

  protected static class ExceptionThrowingCopyConstructorObject<T> extends CopyableObject<T> {

    public ExceptionThrowingCopyConstructorObject(T value) {
      super(value);
    }

    @SuppressWarnings("unused")
    public ExceptionThrowingCopyConstructorObject(final ExceptionThrowingCopyConstructorObject<T> copyableObject) {
      throw new IllegalArgumentException("test");
    }
  }

  protected static class Message implements Serializable {

    private final String text;

    private Message(String text) {
      this.text = text;
    }

    public Object getText() {
      return this.text;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Message)) {
        return false;
      }

      Message that = (Message) obj;

      return ObjectUtils.equals(getText(), that.getText());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getText());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.valueOf(getText());
    }
  }

  protected static abstract class Person {

    public static Person as(String firstName, String lastName) {
      return new Person(firstName, lastName) { };
    }

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

      if (this == obj) {
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

    public SoftwareEngineer(String firstName, String lastName) {
      super(firstName, lastName);
    }

    @SuppressWarnings("unused")
    public SoftwareEngineer(Person person) {
      super(person);
    }
  }

  protected static class TestException extends RuntimeException {

    protected TestException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
