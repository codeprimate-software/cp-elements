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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.Period;
import java.util.Arrays;

import org.cp.elements.lang.reflect.ReflectionUtils.MethodReference;
import org.cp.elements.lang.reflect.ReflectionUtils.MethodResolver;
import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link ReflectionUtils} is the {@link ReflectionUtils#isOverloaded(MethodReference)}
 * and {@link ReflectionUtils#isOverridden(MethodReference)} utility methods.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodReference
 * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodResolver
 * @since 1.0.0
 */
public class OverloadedOverriddenMethodReflectionUtilsUnitTests {

  // Java Unit Tests and Assertions that must be true for ReflectionUtils to function properly.

  @Test
  public void arraysEqualsWithEqualArraysReturnsTrue() {

    Class<?>[] arrayOne = { String.class, Integer.class };
    Class<?>[] arrayTwo = { String.class, Integer.class };

    assertThat(Arrays.equals(arrayOne, arrayTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void arraysEqualsForEmptyArraysReturnsTrue() {

    Class<?>[] arrayOne = {};
    Class<?>[] arrayTwo = {};

    assertThat(Arrays.equals(arrayOne, arrayTwo)).isTrue();
  }

  @Test
  public void arrayEqualsWithDifferentNumberOfArgumentsReturnsFalse() {

    Class<?>[] arrayOne = { String.class, Integer.class };
    Class<?>[] arrayTwo = { String.class };

    assertThat(Arrays.equals(arrayOne, arrayTwo)).isFalse();
  }

  @Test
  public void arrayEqualsWithDifferentOrderOfArgumentsReturnsFalse() {

    Class<?>[] arrayOne = { String.class, Integer.class };
    Class<?>[] arrayTwo = { Integer.class, String.class };

    assertThat(Arrays.equals(arrayOne, arrayTwo)).isFalse();
  }

  @Test
  public void arrayEqualsWithDifferentTypeOfArgumentsReturnsFalse() {

    Class<?>[] arrayOne = { Integer.class };
    Class<?>[] arrayTwo = { Long.class };

    assertThat(Arrays.equals(arrayOne, arrayTwo)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void arrayEqualsWithNullArgumentsReturnsTrue() {
    assertThat(Arrays.equals((Object[]) null, null)).isTrue();
  }

  @Test
  public void classGetMethodReturningSameLogicalMethodAreEqualButNotTheSame() throws NoSuchMethodException {

    Method getAgeOne = Person.class.getMethod("getAge", LocalDate.class);
    Method getAgeTwo = Person.class.getMethod("getAge", LocalDate.class);
    Method getAgeNoArgsOne = Person.class.getMethod("getAge");
    Method getAgeNoArgsTwo = BirthdateCapable.class.getMethod("getAge");
    Method getAgeNoArgsThree = BirthdateCapable.class.getMethod("getAge");

    assertThat(getAgeOne).isEqualTo(getAgeTwo);
    assertThat(getAgeOne).isNotSameAs(getAgeTwo);
    assertThat(getAgeNoArgsOne).isNotEqualTo(getAgeOne);
    assertThat(getAgeNoArgsOne).isEqualTo(getAgeNoArgsTwo);
    assertThat(getAgeNoArgsOne).isNotSameAs(getAgeNoArgsTwo);
    assertThat(getAgeNoArgsThree).isNotSameAs(getAgeNoArgsTwo);
  }

  @Test
  public void declaringClassOfInheritedMethodIsSuperType() throws NoSuchMethodException {

    Method getAge = Customer.class.getMethod("getAge");

    assertThat(getAge).isNotNull();
    assertThat(getAge.getName()).isEqualTo("getAge");
    assertThat(getAge.getDeclaringClass()).isEqualTo(BirthdateCapable.class);

    Method getBirthdate = Customer.class.getMethod("getBirthdate");

    assertThat(getBirthdate).isNotNull();
    assertThat(getBirthdate.getName()).isEqualTo("getBirthdate");
    assertThat(getBirthdate.getReturnType()).isEqualTo(LocalDate.class);
    assertThat(getBirthdate.getDeclaringClass()).isEqualTo(Person.class);
  }

  // Additional ReflectionUtils Unit Tests

  @Test
  public void fromMethodResolverAndType() {

    MethodResolver methodResolver = MethodResolver.fromType(Person.class);

    assertThat(methodResolver).isNotNull();
    assertThat(methodResolver.getReferenceType()).isEqualTo(Person.class);
  }

  @Test
  public void fromMethodResolverAndNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() ->MethodResolver.fromType(null))
      .withMessage("Reference type is required")
      .withNoCause();
  }

  @Test
  public void fromMethodResolverHavingMethodName() {

    MethodReference methodReference = MethodResolver.fromType(Person.class)
      .havingName("getName");

    assertThat(methodReference).isNotNull();
    assertThat(methodReference.fromType()).isEqualTo(Person.class);
    assertThat(methodReference.getName()).isEqualTo("getName");
    assertThat(methodReference.getParameterTypes()).isEmpty();
  }

  @Test
  public void fromMethodResolverHavingMethodNameWithParameterTypes() {

    MethodReference methodReference = MethodResolver.fromType(Customer.class)
      .havingName("getAge")
      .withParameterTypes(LocalDate.class);

    assertThat(methodReference).isNotNull();
    assertThat(methodReference.fromType()).isEqualTo(Customer.class);
    assertThat(methodReference.getName()).isEqualTo("getAge");
    assertThat(methodReference.getParameterTypes()).containsExactly(LocalDate.class);
  }
  @Test
  public void fromMethodResolverHavingBlankMethodName() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MethodResolver.fromType(Person.class).havingName("  "))
      .withMessage("Method name [  ] is required")
      .withNoCause();
  }

  @Test
  public void fromMethodResolverHavingEmptyMethodName() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MethodResolver.fromType(Person.class).havingName(""))
      .withMessage("Method name [] is required")
      .withNoCause();
  }

  @Test
  public void fromMethodResolverHavingNullMethodName() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MethodResolver.fromType(Person.class).havingName(null))
      .withMessage("Method name [null] is required")
      .withNoCause();
  }

  @Test
  public void methodReferenceIsResolvableToDeclaredMethod() {

    Method method = MethodResolver.fromType(Person.class)
      .havingName("getAge")
      .withParameterTypes(LocalDate.class)
      .get();

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(Person.class);
    assertThat(method.getName()).isEqualTo("getAge");
    assertThat(method.getParameterTypes()).containsExactly(LocalDate.class);
    assertThat(method.getReturnType()).isEqualTo(Integer.TYPE);
  }

  @Test
  public void methodReferenceIsResolvableToInheritedMethod() {

    Method method = MethodResolver.fromType(Customer.class)
      .havingName("getBirthdate")
      .get();

    assertThat(method).isNotNull();
    assertThat(method.getName()).isEqualTo("getBirthdate");
    assertThat(method.getParameterTypes()).isEmpty();
    assertThat(method.getReturnType()).isEqualTo(LocalDate.class);
  }

  @Test
  public void methodReferenceIsNonResolvable() {

    assertThatThrowableOfType(MethodNotFoundException.class)
      .isThrownBy(args -> MethodResolver.fromType(Customer.class)
        .havingName("getNonExistingMethod")
        .withParameterTypes(Object.class)
        .get())
      .havingMessage("Method [getNonExistingMethod] with parameters of type [[java.lang.Object]] not found")
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  public void nullMethodIsNotOverloadedAndIsNullSafe() {
    assertThat(ReflectionUtils.isOverloaded(null)).isFalse();
  }

  @Test
  public void nullMethodIsNotOverriddenAndIsNullSafe() {
    assertThat(ReflectionUtils.isOverridden(null)).isFalse();
  }

  @Test
  public void overloadedMethodReturnsTrue() {

    MethodReference getAge = MethodResolver.fromType(Person.class)
      .havingName("getAge")
      .withParameterTypes(LocalDate.class);

    assertThat(ReflectionUtils.isOverloaded(getAge)).isTrue();
  }

  @Test
  public void nonOverloadedMethodReturnsFalse() {

    MethodReference getName = MethodResolver.fromType(Person.class)
        .havingName("getName");

    assertThat(ReflectionUtils.isOverloaded(getName)).isFalse();
  }

  @Test
  public void overriddenMethodReturnsTrue() {

    MethodReference getBirthdate = MethodResolver.fromType(Person.class)
        .havingName("getBirthdate");

    assertThat(ReflectionUtils.isOverridden(getBirthdate)).isTrue();
  }

  @Test
  public void nonOverriddenMethodReturnsFalse() {

    MethodReference getAge = MethodResolver.fromType(Person.class)
      .havingName("getAge")
      .withParameterTypes(LocalDate.class);

    assertThat(ReflectionUtils.isOverridden(getAge)).isFalse();
  }

  @Test
  public void subTypeOverloadedMethodReturnsTrue() throws NoSuchMethodException {

    MethodReference getAge = MethodResolver.fromType(Customer.class)
      .havingName("getAge");

    MethodReference mockGetAge = mock(MethodReference.class);

    Method testGetAge = Customer.class.getMethod("getAge");

    assertThat(testGetAge).isNotNull();
    assertThat(testGetAge.getName()).isEqualTo("getAge");
    assertThat(testGetAge.getDeclaringClass()).isEqualTo(BirthdateCapable.class);

    doReturn("getAge").when(mockGetAge).getName();
    doReturn(testGetAge).when(mockGetAge).get();

    assertThat(ReflectionUtils.isOverloaded(getAge)).isTrue();
    assertThat(ReflectionUtils.isOverloaded(mockGetAge)).isFalse();
  }

  @Test
  public void subTypeOverriddenMethodReturnsTrue() {

    MethodReference getBirthdate = MethodResolver.fromType(Customer.class)
      .havingName("getBirthdate");

    assertThat(ReflectionUtils.isOverridden(getBirthdate)).isTrue();
  }

  @FunctionalInterface
  @SuppressWarnings("unused")
  interface Base {
    Object getBirthdate();
  }

  @FunctionalInterface
  interface BirthdateCapable extends Base {

    //LocalDate getBirthdate();

    default int getAge() {
      return LocalDate.now().isAfter((LocalDate) getBirthdate())
        ? Period.between((LocalDate) getBirthdate(), LocalDate.now()).getYears()
        : 0;
    }
  }

  @Getter
  @ToString
  @EqualsAndHashCode
  @RequiredArgsConstructor(staticName = "as")
  @SuppressWarnings("unused")
  static class Person implements BirthdateCapable {

    @Setter
    private LocalDate birthdate;

    @lombok.NonNull
    private final String name;

    public int getAge(LocalDate date) {
      return date.isAfter(getBirthdate()) ? Period.between(getBirthdate(), date).getYears() : 0;
    }
  }

  @SuppressWarnings("unused")
  static class Customer extends Person {

    static Customer from(String name) {
      return new Customer(name);
    }

    static Customer from(Person person) {
      return new Customer(person.getName());
    }

    Customer(String name) {
      super(name);
    }
  }
}
