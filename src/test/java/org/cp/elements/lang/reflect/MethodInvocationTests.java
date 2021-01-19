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
package org.cp.elements.lang.reflect;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.reflect.MethodInvocation.newMethodInvocation;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.mockito.Mockito.mock;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.Month;
import java.time.Period;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link MethodInvocation}.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see java.time.LocalDate
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @since 1.0.0
 */
public class MethodInvocationTests {

  private Object target = new Object();

  @Test
  public void newMethodInvocationWithMethodAndArguments() throws NoSuchMethodException {

    LocalDate johnBirthDate = LocalDate.of(1974, Month.MAY, 27);

    Method getAge = AgeCalculator.class.getMethod("getAge", LocalDate.class);

    MethodInvocation methodInvocation = newMethodInvocation(getAge, johnBirthDate);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getArguments()).contains(johnBirthDate);
    assertThat(methodInvocation.getDeclaringClass()).isEqualTo(AgeCalculator.class);
    assertThat(methodInvocation.getMethod()).isSameAs(getAge);
    assertThat(methodInvocation.getTarget()).isNull();
  }

  @Test
  public void newMethodInvocationWithTargetObjectMethodAndArguments() throws NoSuchMethodException {

    ContactRepository mockContactRepository = mock(ContactRepository.class);

    Method findById = ContactRepository.class.getMethod("findBy", Long.class);

    MethodInvocation methodInvocation = newMethodInvocation(mockContactRepository, findById, 1L);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getArguments()).contains(1L);
    assertThat(methodInvocation.getDeclaringClass()).isEqualTo(ContactRepository.class);
    assertThat(methodInvocation.getMethod()).isSameAs(findById);
    assertThat(methodInvocation.getTarget()).isSameAs(mockContactRepository);
  }

  @Test
  public void newMethodInvocationWithClassTypeMethodNameAndArguments() throws NoSuchMethodException {

    LocalDate ellieBirthDate = LocalDate.of(2008, Month.AUGUST, 25);

    MethodInvocation methodInvocation = newMethodInvocation(AgeCalculator.class, "getAge", ellieBirthDate);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getArguments()).contains(ellieBirthDate);
    assertThat(methodInvocation.getDeclaringClass()).isEqualTo(AgeCalculator.class);
    assertThat(methodInvocation.getMethod()).isEqualTo(AgeCalculator.class.getMethod("getAge", LocalDate.class));
    assertThat(methodInvocation.getTarget()).isNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void newMethodInvocationWithNullClassTypeThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> newMethodInvocation((Class<?>) null, "testMethod", "argOne", "argTwo"),
        () -> "Class type cannot be null");
  }

  @Test
  public void newMethodInvocationWithTargetObjectMethodNameAndArguments() throws NoSuchMethodException {

    Contact johnBlum = () -> "John Blum";

    MethodInvocation methodInvocation = newMethodInvocation(johnBlum, "getName");

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getArguments()).isNotNull();
    assertThat(methodInvocation.getArguments()).isEmpty();
    assertThat(methodInvocation.getDeclaringClass()).isEqualTo(johnBlum.getClass());
    assertThat(methodInvocation.getMethod()).isEqualTo(johnBlum.getClass().getMethod("getName"));
    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);
  }

  @Test(expected = IllegalArgumentException.class)
  public void newMethodInvocationWithNullTargetObjectThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> newMethodInvocation((Object) null, "mockMethod", "argOne", "argTwo"),
        () -> "Target object cannot be null");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructMethodInvocationWithNullMethodThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> new MethodInvocation(target, null, "argOne", "argTwo"),
        () -> "Method cannot be null");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructMethodInvocationWithNullTargetObjectAndNonStaticMethodThrowsIllegalArgumentException()
      throws NoSuchMethodException {

    try {
      new MethodInvocation(null, Contact.class.getMethod("getName"));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Method must be static if target is null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructMethodInvocationValidateArguments() throws NoSuchMethodException {

    Method getName = Contact.class.getMethod("getName");

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new MethodInvocation(target, getName, "JonDoe"),
      () -> String.format("The number of arguments [1] does not match the number of parameters [0] for method [getName] in class [%s]",
        Contact.class.getName()));
  }

  @Test
  public void validateArgumentsIsSuccessful() throws NoSuchMethodException {

    Method findByLastName = ContactRepository.class.getMethod("findBy", String.class);

    MethodInvocation methodInvocation = newMethodInvocation(target, findByLastName, "Blum");

    assertThat(methodInvocation.validateArguments(findByLastName, "Hill")).isEqualTo(asArray("Hill"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void validateArgumentsThrowsIllegalArgumentExceptionForNullMethod() throws NoSuchMethodException {

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    assertThat(methodInvocation.getMethod()).isSameAs(getName);

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> methodInvocation.validateArguments(null, "argOne", "argTwo"),
        () -> "Method cannot be null");
  }

  @Test(expected = IllegalArgumentException.class)
  public void validateArgumentsThrowsIllegalArgumentExceptionForWrongNumberOfArguments() throws NoSuchMethodException {

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> methodInvocation.validateArguments(getName, "John", "Blum"),
        () -> String.format("The number of arguments [2] does not match the number of parameters [0] for method [getName] in class [%s]",
          Contact.class.getName()));
  }

  @Test(expected = IllegalArgumentException.class)
  public void validateArgumentsThrowsIllegalArgumentExceptionForArgumentTypeParameterTypeMismatch()
      throws NoSuchMethodException {

    Method findById = ContactRepository.class.getMethod("findBy", Long.class);

    MethodInvocation methodInvocation = newMethodInvocation(target, findById, 1L);

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> methodInvocation.validateArguments(findById, 2),
      () -> "Argument [2] is not assignable to parameter [0] of type [java.lang.Long]");
  }

  @Test
  public void invokeOnConfiguredTarget() throws NoSuchMethodException {

    Contact johnBlum = () -> "John Blum";

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(johnBlum, getName);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getMethod()).isSameAs(getName);
    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);
    assertThat(methodInvocation.invoke().orElse(null)).isEqualTo("John Blum");
  }

  @Test
  public void invokeOnGivenTarget() throws NoSuchMethodException {

    Contact johnBlum = () -> "John Blum";
    Contact ellieBlum = () -> "Ellie Blum";

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(johnBlum, getName);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getMethod()).isSameAs(getName);
    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);
    assertThat(methodInvocation.invoke(ellieBlum).orElse(null)).isEqualTo("Ellie Blum");
    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);
  }

  @Test
  public void invokeStaticMethod() throws NoSuchMethodException {

    LocalDate saraBirthDate = LocalDate.of(1975, Month.JANUARY, 22);

    Method getAge = AgeCalculator.class.getMethod("getAge", LocalDate.class);

    MethodInvocation methodInvocation = newMethodInvocation(getAge, saraBirthDate);

    assertThat(methodInvocation).isNotNull();
    assertThat(methodInvocation.getArguments()).contains(saraBirthDate);
    assertThat(methodInvocation.getMethod()).isSameAs(getAge);
    assertThat(methodInvocation.getTarget()).isNull();
    assertThat(methodInvocation.invoke().orElse(null)).isEqualTo(42);
  }

  @Test(expected = MethodInvocationException.class)
  public void invokeHandlesInvocationTargetException() throws NoSuchMethodException {

    Contact error = () -> { throw newIllegalStateException("test"); };

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(error, getName);

    try {
      methodInvocation.invoke();
    }
    catch (MethodInvocationException expected) {

      assertThat(expected).hasMessage("Failed to invoke method [getName] on target object [%s]", error);
      assertThat(expected).hasCauseInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(IllegalStateException.class);
      assertThat(expected.getCause().getCause()).hasMessage("test");
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void resolveTargetReturnsConfiguredTarget() throws NoSuchMethodException {

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    assertThat(methodInvocation.resolveTarget(null)).isSameAs(target);
  }

  @Test
  public void resolveTargetReturnsGivenTarget() throws NoSuchMethodException {

    Contact mockContact = mock(Contact.class);

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    assertThat(methodInvocation.resolveTarget(mockContact)).isSameAs(mockContact);
  }

  @Test
  public void makeAccessibleIsSuccessful() throws NoSuchMethodException {

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    assertThat(getName.isAccessible()).isFalse();
    assertThat(methodInvocation.makeAccessible()).isSameAs(methodInvocation);
    assertThat(getName.isAccessible()).isTrue();
  }

  @Test
  public void onTargetSwitchesTarget() throws NoSuchMethodException {

    Contact johnBlum = () -> "John Blum";

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(target, getName);

    assertThat(methodInvocation.getTarget()).isSameAs(target);
    assertThat(methodInvocation.on(johnBlum)).isSameAs(methodInvocation);
    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);
  }

  @Test(expected = IllegalArgumentException.class)
  public void onNullTargetForNonStaticMethodThrowsIllegalArgumentException() throws NoSuchMethodException {

    Contact johnBlum = () -> "John Blum";

    Method getName = Contact.class.getMethod("getName");

    MethodInvocation methodInvocation = newMethodInvocation(johnBlum, getName);

    assertThat(methodInvocation.getTarget()).isSameAs(johnBlum);

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> methodInvocation.on(null),
      () -> "Method must be static if target is null");
  }

  @Test
  public void passingArgumentsSwitchesArguments() throws NoSuchMethodException {

    Method findById = ContactRepository.class.getMethod("findBy", Long.class);

    MethodInvocation methodInvocation = newMethodInvocation(target, findById, 1L);

    assertThat(methodInvocation.getArguments()).contains(1L);
    assertThat(methodInvocation.passing(2L)).isSameAs(methodInvocation);
    assertThat(methodInvocation.getArguments()).contains(2L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void passingArgumentsValidatesArguments() throws NoSuchMethodException {

    Method findByLastName = ContactRepository.class.getMethod("findBy", String.class);

    MethodInvocation methodInvocation = newMethodInvocation(target, findByLastName, "Blum");

    assertThat(methodInvocation.getArguments()).contains("Blum");

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> methodInvocation.passing('x'),
      () -> "Argument [x] is not assignable to parameter [0] of type [java.lang.String]");
  }

  @SuppressWarnings("all")
  static class AgeCalculator {
    public static int getAge(LocalDate birthDate) {
      return Period.between(birthDate, LocalDate.of(2017, Month.FEBRUARY, 27)).getYears();
    }
  }

  interface Contact {
    String getName();
  }

  @SuppressWarnings("all")
  interface ContactRepository {
    Contact findBy(Long id);
    Iterable<Contact> findBy(String lastName);
  }
}
