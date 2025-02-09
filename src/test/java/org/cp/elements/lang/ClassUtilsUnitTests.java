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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.test.TestUtils.jacocoFilter;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.TypeVariable;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.Socket;
import java.time.Instant;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.Id;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.factory.ObjectInstantiationException;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.ModifierUtils;
import org.cp.elements.lang.reflect.ReflectionUtils;
import org.cp.elements.test.AbstractTestSuite;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link ClassUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.reflect.Constructor
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see java.lang.reflect.ParameterizedType
 * @see java.lang.reflect.TypeVariable
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ClassUtils
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @see org.cp.elements.test.AbstractTestSuite
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
class ClassUtilsUnitTests extends AbstractTestSuite {

  private <T> void assertConstructor(Constructor<T> constructor, Class<T> declaringClass, Class<?>[] parameterTypes) {

    assertThat(constructor).isNotNull();
    assertThat(constructor.getDeclaringClass()).isEqualTo(declaringClass);
    TestUtils.assertEquals(parameterTypes, constructor.getParameterTypes());
  }

  @Test
  void assignableTo() {

    assertThat(ClassUtils.assignableTo(null, Object.class)).isTrue();
    assertThat(ClassUtils.assignableTo(null, Calendar.class)).isTrue();
    assertThat(ClassUtils.assignableTo(null, Number.class)).isTrue();
    assertThat(ClassUtils.assignableTo(null, String.class)).isTrue();
    assertThat(ClassUtils.assignableTo(String.class, Object.class)).isTrue();
    assertThat(ClassUtils.assignableTo(Number.class, Object.class)).isTrue();
    assertThat(ClassUtils.assignableTo(Double.class, Number.class)).isTrue();
    assertThat(ClassUtils.assignableTo(Integer.class, Number.class)).isTrue();
    assertThat(ClassUtils.assignableTo(java.sql.Date.class, Date.class)).isTrue();
  }

  @Test
  void notAssignableTo() {

    assertThat(ClassUtils.assignableTo(null, null)).isFalse();
    assertThat(ClassUtils.assignableTo(Object.class, null)).isFalse();
    assertThat(ClassUtils.assignableTo(String.class, Number.class)).isFalse();
    assertThat(ClassUtils.assignableTo(Character.class, String.class)).isFalse();
    assertThat(ClassUtils.assignableTo(Object.class, String.class)).isFalse();
  }

  @Test
  void castNullToInstantIsSafe() {

    Instant value = ClassUtils.castTo(null, Instant.class);

    assertThat(value).isNull();
  }

  @Test
  void castObjectToIntegerIsCorrect() {

    Object target = 2;
    Integer number = ClassUtils.castTo(target, Integer.class);

    assertThat(number).isEqualTo(target);
  }

  @Test
  void castObjectToStringIsCorrect() {

    Object target = "test";
    String value = ClassUtils.castTo(target, String.class);

    assertThat(value).isEqualTo(target);
  }

  @Test
  void castValueToInvalidTypeThrowsIllegalArgumentException() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> ClassUtils.castTo("test", Integer.class))
      .withMessage("Object [test] is not an instance of Class [java.lang.Integer]")
      .withNoCause();
  }

  @Test
  void castToWithNullTypeThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ClassUtils.castTo("test", null))
      .withMessage("The Class type used to cast is required")
      .withNoCause();
  }

  @Test
  void constructObjectWithPublicNoArgumentConstructorIsSuccessful() {
    assertThat(ClassUtils.construct(ObjectWithPublicNoArgConstructor.class)).isNotNull();
  }

  @Test
  void constructObjectWithPublicNoArgumentConstructorThrowingException() {

    ThrowableAssertions.assertThatThrowableOfType(ObjectInstantiationException.class)
      .isThrownBy(args -> ClassUtils.construct(ObjectWithPublicNoArgConstructorThrowingException.class))
      .havingMessage("Failed to construct object of type [%s]",
        ObjectWithPublicNoArgConstructorThrowingException.class.getName())
      .causedBy(InvocationTargetException.class)
      .causedBy(RuntimeException.class)
      .havingMessage("test")
      .withNoCause();
  }

  @Test
  void constructObjectWithNonPublicNoArgumentConstructorIsSuccessful() {
    assertThat(ClassUtils.construct(ObjectWithNonPublicNoArgConstructor.class)).isNotNull();
  }

  @Test
  void constructObjectWithNonPublicNoArgumentConstructorPassingArgumentsThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ObjectInstantiationException.class)
      .isThrownBy(args -> ClassUtils.construct(ObjectWithNonPublicNoArgConstructor.class, "MockValue"))
      .havingMessage("Failed to construct object of type [%s]",
        ObjectWithNonPublicNoArgConstructor.class.getName())
      .causedBy(ConstructorNotFoundException.class)
      .havingMessage("Failed to find a default, public no-argument constructor for type [%s]",
        ObjectWithNonPublicNoArgConstructor.class.getName())
      .withNoCause();
  }

  @Test
  void constructObjectWithNonPublicWithArgumentsConstructorIsSuccessful() {

    ObjectWithNonDefaultConstructor object =
      ClassUtils.construct(ObjectWithNonDefaultConstructor.class, "test");

    assertThat(object).isNotNull();
    assertThat(object.getValue()).isEqualTo("test");
  }

  @Test
  void constructObjectWithDefaultConstructorIsSuccessful() {
    assertThat(ClassUtils.construct(ObjectWithDefaultConstructor.class)).isNotNull();
  }

  @Test
  void getClassIsCorrect() {

    assertThat(ClassUtils.getClass(new Object())).isEqualTo(Object.class);
    assertThat(ClassUtils.getClass(true)).isEqualTo(Boolean.class);
    assertThat(ClassUtils.getClass('c')).isEqualTo(Character.class);
    assertThat(ClassUtils.getClass(Calendar.getInstance().getTime())).isEqualTo(Date.class);
    assertThat(ClassUtils.getClass(0)).isEqualTo(Integer.class);
    assertThat(ClassUtils.getClass(Math.PI)).isEqualTo(Double.class);
    assertThat(ClassUtils.getClass("null")).isEqualTo(String.class);
  }

  @Test
  @SuppressWarnings("all")
  void getClassWithNull() {
    assertThat(ClassUtils.getClass(null)).isNull();
  }

  @Test
  void getClassName() {

    assertThat(ClassUtils.getClassName(new Object())).isEqualTo("java.lang.Object");
    assertThat(ClassUtils.getClassName(Boolean.TRUE)).isEqualTo("java.lang.Boolean");
    assertThat(ClassUtils.getClassName('$')).isEqualTo("java.lang.Character");
    assertThat(ClassUtils.getClassName(Calendar.getInstance().getTime())).isEqualTo("java.util.Date");
    assertThat(ClassUtils.getClassName(2)).isEqualTo("java.lang.Integer");
    assertThat(ClassUtils.getClassName(0.0d)).isEqualTo("java.lang.Double");
    assertThat(ClassUtils.getClassName("test")).isEqualTo("java.lang.String");
  }

  @Test
  void getClassNameWithNull() {
    assertThat(ClassUtils.getClassName(null)).isNull();
  }

  @Test
  void getClassSimpleName() {

    assertThat(ClassUtils.getClassSimpleName(new Object())).isEqualTo("Object");
    assertThat(ClassUtils.getClassSimpleName(Boolean.TRUE)).isEqualTo("Boolean");
    assertThat(ClassUtils.getClassSimpleName('!')).isEqualTo("Character");
    assertThat(ClassUtils.getClassSimpleName(Calendar.getInstance().getTime())).isEqualTo("Date");
    assertThat(ClassUtils.getClassSimpleName(9)).isEqualTo("Integer");
    assertThat(ClassUtils.getClassSimpleName(1.0d)).isEqualTo("Double");
    assertThat(ClassUtils.getClassSimpleName("TEST")).isEqualTo("String");
  }

  @Test
  void getClassSimpleNameWithNull() {
    assertThat(ClassUtils.getClassSimpleName(null)).isNull();
  }

  @Test
  void getInterfaceForClass() {

    assertThat(ClassUtils.getInterfaces(ChildType.class)).containsAll(asIterable(InterfaceFive.class,
      InterfaceFour.class, InterfaceThree.class, InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(ParentType.class)).containsAll(asIterable(InterfaceFour.class,
      InterfaceThree.class, InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(GrandparentType.class)).containsAll(asIterable(InterfaceThree.class,
      InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));
  }

  @Test
  void getInterfacesForInterface() {

    assertThat(ClassUtils.getInterfaces(InterfaceFive.class)).containsAll(asIterable(
      InterfaceThree.class, InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(InterfaceFour.class)).containsAll(asIterable(InterfaceTwo.class));

    assertThat(ClassUtils.getInterfaces(InterfaceThree.class)).containsAll(asIterable(
      InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(InterfaceTwo.class)).isEmpty();

    assertThat(ClassUtils.getInterfaces(InterfaceOne.class)).contains(InterfaceZero.class);

    assertThat(ClassUtils.getInterfaces(InterfaceZero.class)).isEmpty();
  }

  @Test
  void getInterfacesForObject() {

    ChildType child = new ChildType();

    assertThat(ClassUtils.getInterfaces(child)).containsAll(asIterable(InterfaceFive.class, InterfaceFour.class,
      InterfaceThree.class, InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    ParentType parent = new ParentType();

    assertThat(ClassUtils.getInterfaces(parent)).containsAll(asIterable(InterfaceFour.class, InterfaceThree.class,
      InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    GrandparentType grandparent = new GrandparentType();

    assertThat(ClassUtils.getInterfaces(grandparent)).containsAll(asIterable(InterfaceThree.class, InterfaceTwo.class,
      InterfaceOne.class, InterfaceZero.class));

    InterfaceFour four = new InterfaceFour() { };

    assertThat(ClassUtils.getInterfaces(four)).containsAll(asIterable(InterfaceFour.class, InterfaceTwo.class));
  }

  @Test
  void getInterfacesForObjectClassHasNoInterfaces() {
    assertThat(ClassUtils.getInterfaces(new Object())).isEmpty();
  }

  @Test
  void getInterfacesForNullClassReturnsEmptySet() {
    assertThat(ClassUtils.getInterfaces(null)).isEmpty();
  }

  @Test
  void getInterfacesForNullObjectReturnsEmptySet() {
    assertThat(ClassUtils.getInterfaces((Object) null)).isEmpty();
  }

  @Test
  void findConstructor() {

    Constructor<SubType> constructor = ClassUtils.findConstructor(SubType.class, 1L);

    assertConstructor(constructor, SubType.class, ArrayUtils.<Class<?>>asArray(Long.class));
  }

  @Test
  void findCompatibleConstructor() {

    Constructor<SuperType> constructor = ClassUtils.findConstructor(SuperType.class, "test");

    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class<?>>asArray(Object.class));
  }

  @Test
  void findNonExistingConstructor() {
    assertThat(ClassUtils.findConstructor(SubType.class, 1)).isNull();
  }

  @Test
  void findNonExistingNonMatchingConstructor() {
    assertThat(ClassUtils.findConstructor(SubType.class, "test", 1L, false)).isNull();
  }

  @Test
  void findDefaultConstructorWithObjectHavingPublicNoArgConstructorIsSuccessful() {

    Constructor<ObjectWithPublicNoArgConstructor> constructor =
      ClassUtils.findDefaultConstructor(ObjectWithPublicNoArgConstructor.class);

    assertThat(constructor).isNotNull();
    assertThat(constructor.getDeclaringClass()).isEqualTo(ObjectWithPublicNoArgConstructor.class);
    assertThat(constructor.getParameterCount()).isZero();
    assertThat(ModifierUtils.isPublic(constructor)).isTrue();
  }

  @Test
  void findDefaultConstructorWithObjectHavingDefaultConstructorIsSuccessful() {

    Constructor<ObjectWithDefaultConstructor> constructor =
      ClassUtils.findDefaultConstructor(ObjectWithDefaultConstructor.class);

    assertThat(constructor).isNotNull();
    assertThat(constructor.getDeclaringClass()).isEqualTo(ObjectWithDefaultConstructor.class);
    assertThat(constructor.getParameterCount()).isZero();
    assertThat(ModifierUtils.isPublic(constructor)).isTrue();
  }

  @Test
  void findDefaultConstructorWithObjectHavingNonPublicNoArgConstructorThrowsException() {

    assertThatExceptionOfType(ConstructorNotFoundException.class)
      .isThrownBy(() -> ClassUtils.findDefaultConstructor(ObjectWithNonPublicNoArgConstructor.class))
      .withMessage("Failed to find a default, public no-argument constructor for type [%s]",
        ObjectWithNonPublicNoArgConstructor.class.getName())
      .withNoCause();
  }

  @Test
  void findDefaultConstructorWithObjectHavingPublicArgumentConstructorThrowsException() {

    assertThatExceptionOfType(ConstructorNotFoundException.class)
      .isThrownBy(() -> ClassUtils.findDefaultConstructor(ObjectWithPublicArgumentConstructor.class))
      .withMessage("Failed to find a default, public no-argument constructor for type [%s]",
        ObjectWithPublicArgumentConstructor.class.getName())
      .withNoCause();
  }

  @Test
  void getConstructor() {

    Constructor<SuperType> constructor = ClassUtils.getConstructor(SuperType.class, Object.class);

    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class<?>>asArray(Object.class));
  }

  @Test
  void getNonExistingConstructor() {

    ThrowableAssertions.assertThatThrowableOfType(ConstructorNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getConstructor(SuperType.class, String.class))
      .havingMessageContaining(NoSuchMethodException.class.getName().concat(":"))
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void resolveConstructor() {

    Constructor<SuperType> constructor = ClassUtils.resolveConstructor(SuperType.class,
      new Class[] { Object.class }, (Object[]) null);

    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class<?>>asArray(Object.class));
  }

  @Test
  void resolveMatchingConstructor() {

    Constructor<SubType> constructor = ClassUtils.resolveConstructor(SubType.class, new Class[0],
      true, 1L, "test");

    assertConstructor(constructor, SubType.class, ArrayUtils.<Class<?>>asArray(Boolean.class, Number.class, String.class));
  }

  @Test
  void resolveNonExistingNonMatchingConstructor() {

    ThrowableAssertions.assertThatThrowableOfType(ConstructorNotFoundException.class)
      .isThrownBy(args -> ClassUtils.resolveConstructor(SubType.class, new Class<?>[] { Integer.class }, 2))
      .havingMessageStartingWith("Failed to resolve constructor with signature")
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void getAllDeclaredFieldsForObject() {

    Field[] fields = ClassUtils.getAllDeclaredFields(new Object());

    assertThat(fields).isNotNull();
    assertThat(fields).isEmpty();
  }

  @Test
  void getAllDeclaredFieldsForNullObject() {

    Field[] fields = ClassUtils.getAllDeclaredFields((Object) null);

    assertThat(fields).isNotNull();
    assertThat(fields).isEmpty();
  }

  @Test
  void getAllDeclaredFieldsForSuperClassType() {

    Field[] fields = ClassUtils.getAllDeclaredFields(SuperType.class);

    assertThat(fields).isNotNull();
    assertThat(Arrays.stream(fields).map(Field::getName).filter(jacocoFilter()).collect(Collectors.toList()))
      .containsExactly("id", "stringValue");
  }

  @Test
  void getAllDeclaredFieldsForSubClassType() {

    Field[] fields = ClassUtils.getAllDeclaredFields(SubType.class);

    assertThat(fields).isNotNull();
    assertThat(Arrays.stream(fields).map(Field::getName).filter(jacocoFilter()).collect(Collectors.toList()))
      .containsExactly("charValue", "id", "nonAnnotatedField", "id", "stringValue");
  }

  @Test
  void getAllDeclaredFieldsForNullClassType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ClassUtils.getAllDeclaredFields(null))
      .withMessage("Class type is required")
      .withNoCause();
  }

  @Test
  void getField() {

    Field charValueField = ClassUtils.getField(SubType.class, "charValue");

    assertThat(charValueField).isNotNull();
    assertThat(charValueField.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(charValueField.getName()).isEqualTo("charValue");
    assertThat(charValueField.getType()).isEqualTo(Character.class);
  }

  @Test
  void getFieldOnSuperClassFromSubClass() {

    Field stringValue = ClassUtils.getField(SubType.class, "stringValue");

    assertThat(stringValue).isNotNull();
    assertThat(stringValue.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(stringValue.getName()).isEqualTo("stringValue");
    assertThat(stringValue.getType()).isEqualTo(String.class);
  }

  @Test
  void getFieldOnSubClassFromSuperClass() {

    ThrowableAssertions.assertThatThrowableOfType(FieldNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getField(SuperType.class, "charValue"))
      .causedBy(NoSuchFieldException.class)
      .withNoCause();
  }

  @Test
  void getNonExistingField() {

    ThrowableAssertions.assertThatThrowableOfType(FieldNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getField(SubType.class, "nonExistingField"))
      .causedBy(NoSuchFieldException.class)
      .withNoCause();
  }

  @Test
  void getOverriddenFieldOnSubClass() {

    Field idField = ClassUtils.getField(SubType.class, "id");

    assertThat(idField).isNotNull();
    assertThat(idField.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(idField.getName()).isEqualTo("id");
    assertThat(idField.getType()).isEqualTo(Long.class);
  }

  @Test
  void findMethod() {

    Method method = ClassUtils.findMethod(SubType.class, "methodTwo", true, 1, "test");

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");

    method = ClassUtils.findMethod(SubType.class, "methodTwo", false, Math.PI, "test");

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");

    method = ClassUtils.findMethod(SuperType.class, "methodTwo", true, 1L, "test");

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");

    method = ClassUtils.findMethod(SubType.class, "methodTwo", "test", 1L, false);

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");
  }

  @Test
  void findNonExistingMethod() {

    assertThat(ClassUtils.findMethod(SubType.class, "methodThree", true, 1L, "test")).isNull();
    assertThat(ClassUtils.findMethod(SuperType.class, "methodTwo", "test", 1, false)).isNull();
    assertThat(ClassUtils.findMethod(SubType.class, "methodTwo", true, "test", 1)).isNull();
    assertThat(ClassUtils.findMethod(SubType.class, "methodTwo", true, 1)).isNull();
    assertThat(ClassUtils.findMethod(SubType.class, "methodTwo", false, "1", 'C')).isNull();
  }

  @Test
  void getMethod() {

    Method getCharacterValueMethod = ClassUtils.getMethod(SubType.class, "getCharacterValue");

    assertThat(getCharacterValueMethod).isNotNull();
    assertThat(getCharacterValueMethod.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(getCharacterValueMethod.getName()).isEqualTo("getCharacterValue");
    assertThat(getCharacterValueMethod.getReturnType()).isEqualTo(Character.class);
  }

  @Test
  void getMethodOnSuperClassFromSubClass() {

    Method getStringValue = ClassUtils.getMethod(SubType.class, "getStringValue");

    assertThat(getStringValue).isNotNull();
    assertThat(getStringValue.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(getStringValue.getName()).isEqualTo("getStringValue");
    assertThat(getStringValue.getReturnType()).isEqualTo(String.class);
  }

  @Test
  void getMethodOnSubClassFromSuperClass() {

    ThrowableAssertions.assertThatThrowableOfType(MethodNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getMethod(SuperType.class, "getCharacterValue"))
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void getNonExistingMethod() {

    ThrowableAssertions.assertThatThrowableOfType(MethodNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getMethod(SubType.class, "nonExistingMethod"))
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void getOverloadedMethod() {

    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", Integer.class, Double.class);

    assertThat(methodOne).isNotNull();
    assertThat(methodOne.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(methodOne.getName()).isEqualTo("methodOne");
    assertThat(methodOne.getReturnType()).isEqualTo(Number.class);
  }

  @Test
  void getOverloadedMethodOnSuperClassFromSubClass() {

    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", Integer.class);

    assertThat(methodOne).isNotNull();
    assertThat(methodOne.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(methodOne.getName()).isEqualTo("methodOne");
    assertThat(methodOne.getReturnType()).isEqualTo(Object.class);
  }

  @Test
  void getOverriddenMethod() {

    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", String.class);

    assertThat(methodOne).isNotNull();
    assertThat(methodOne.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(methodOne.getName()).isEqualTo("methodOne");
    assertThat(methodOne.getReturnType()).isEqualTo(String.class);

    methodOne = ClassUtils.getMethod(SuperType.class, "methodOne", String.class);

    assertThat(methodOne).isNotNull();
    assertThat(methodOne.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(methodOne.getName()).isEqualTo("methodOne");
    assertThat(methodOne.getReturnType()).isEqualTo(Object.class);
  }

  @Test
  void getOverriddenMethodOnSubClassFromSuperClass() {

    ThrowableAssertions.assertThatThrowableOfType(MethodNotFoundException.class)
      .isThrownBy(args -> ClassUtils.getMethod(SuperType.class, "methodOne", Boolean.class))
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void getDeclaredMethod() throws NoSuchMethodException {

    Method nameableGetName = Nameable.class.getDeclaredMethod("getName");
    Method personGetName = ClassUtils.getDeclaredMethod(Person.class, nameableGetName);

    assertThat(personGetName).isNotNull();
    assertThat(personGetName).isNotEqualTo(nameableGetName);
    assertThat(personGetName.getName()).isEqualTo(nameableGetName.getName());
    assertThat(personGetName.getParameterTypes()).isEqualTo(nameableGetName.getParameterTypes());
    assertThat(nameableGetName.getReturnType()).isEqualTo(Object.class);
    assertThat(personGetName.getReturnType()).isEqualTo(String.class);
    assertThat(nameableGetName.getDeclaringClass()).isEqualTo(Nameable.class);
    assertThat(personGetName.getDeclaringClass()).isEqualTo(Person.class);
  }

  @Test
  void getDeclaredMethodReturnsGivenMethod() throws NoSuchMethodException {

    Method personGetName = Person.class.getDeclaredMethod("getName");
    Method declaredPersonGetName = ClassUtils.getDeclaredMethod(Person.class, personGetName);

    assertThat(declaredPersonGetName).isSameAs(personGetName);
  }

  @Test
  void getDeclaredMethodWithNullType() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> ClassUtils.getDeclaredMethod(null, Person.class.getDeclaredMethod("getName")))
      .withMessage("Class type is required")
      .withNoCause();
  }

  @Test
  void getDeclaredMethodWithNullMethod() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> ClassUtils.getDeclaredMethod(Person.class, null))
      .withMessage("Method is required")
      .withNoCause();
  }

  @Test
  void getDeclaredMethodWithIncompatibleTypes() throws NoSuchMethodException {

    Method personGetName = Person.class.getDeclaredMethod("getName");

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> ClassUtils.getDeclaredMethod(String.class, personGetName))
      .withMessage("The declared Class type [%s] of Method [getName] is not assignable from the given Class type [java.lang.String]",
        personGetName.getDeclaringClass().getName())
      .withNoCause();
  }

  @Test
  void resolveMethodToSubClass() {

    Method method = ClassUtils.resolveMethod(SubType.class, "methodTwo",
      ArrayUtils.<Class<?>>asArray(Boolean.class, Integer.class, String.class),
        ArrayUtils.asArray(true, 1, "test"), Void.class);

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SubType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");
  }

  @Test
  void resolveMethodToSuperClass() {

    Method method = ClassUtils.resolveMethod(SubType.class, "methodTwo",
      ArrayUtils.<Class<?>>asArray(Boolean.class, Long.class, String.class),
        ArrayUtils.asArray(true, 1L, "test"), Void.class);

    assertThat(method).isNotNull();
    assertThat(method.getDeclaringClass()).isEqualTo(SuperType.class);
    assertThat(method.getName()).isEqualTo("methodTwo");
  }

  @Test
  void unresolvableMethod() {

    ThrowableAssertions.assertThatThrowableOfType(MethodNotFoundException.class)
      .isThrownBy(args -> ClassUtils.resolveMethod(SubType.class, "methodTwo",
        ArrayUtils.<Class<?>>asArray(Boolean.class, Character.class, Integer.class, Double.class, String.class),
          ArrayUtils.asArray(false, 'c', 1, Math.PI, "test"), Void.class))
      .havingMessage("Failed to resolve method with signature [methodTwo(:Boolean, :Character, :Integer, :Double, :String):void] on class type [%s]",
        SubType.class.getName())
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  void getMethodSignature() {

    assertThat(ClassUtils.getMethodSignature("methodOne", ArrayUtils.<Class<?>>asArray(Boolean.class, Character.class,
      Integer.class, Double.class, String.class), Object.class)).isEqualTo(
      "methodOne(:Boolean, :Character, :Integer, :Double, :String):Object");
    assertThat(ClassUtils.getMethodSignature("methodTwo",
      ArrayUtils.<Class<?>>asArray(String.class), Void.class)).isEqualTo("methodTwo(:String):void");
    assertThat(ClassUtils.getMethodSignature("methodThree", new Class[0], null)).isEqualTo("methodThree():void");
    assertThat(ClassUtils.getMethodSignature("methodFour", null, null)).isEqualTo("methodFour():void");
    assertThat(ClassUtils.getMethodSignature("methodFive",
      ArrayUtils.<Class<?>>asArray(Object.class, null), String.class)).isEqualTo("methodFive(:Object, :null):String");
  }

  @Test
  void getMethodSignatureWithMethod() {

    assertThat(ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "methodOne", ArrayUtils.<Class<?>>asArray(Integer.class, Double.class)))).isEqualTo(
      "methodOne(:Integer, :Double):Number");
    assertThat(ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "methodTwo", ArrayUtils.<Class<?>>asArray(Boolean.class, Integer.class, String.class)))).isEqualTo(
      "methodTwo(:Boolean, :Integer, :String):void");
    assertThat(ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SuperType.class, "methodTwo",
      ArrayUtils.<Class<?>>asArray(Boolean.class, Number.class, String.class)))).isEqualTo(
      "methodTwo(:Boolean, :Number, :String):void");
    assertThat(ClassUtils.getMethodSignature(ClassUtils.getMethod(SubType.class, "getId"))).isEqualTo("getId():String");
    assertThat(ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "deprecatedMethod"))).isEqualTo("deprecatedMethod():void");
  }

  @Test
  void getName() {

    assertThat(ClassUtils.getName(Object.class)).isEqualTo("java.lang.Object");
    assertThat(ClassUtils.getName(Boolean.class)).isEqualTo("java.lang.Boolean");
    assertThat(ClassUtils.getName(Character.class)).isEqualTo("java.lang.Character");
    assertThat(ClassUtils.getName(Date.class)).isEqualTo("java.util.Date");
    assertThat(ClassUtils.getName(Integer.class)).isEqualTo("java.lang.Integer");
    assertThat(ClassUtils.getName(Double.class)).isEqualTo("java.lang.Double");
    assertThat(ClassUtils.getName(String.class)).isEqualTo("java.lang.String");
  }

  @Test
  @SuppressWarnings("all")
  void getNameWithNull() {
    assertThat(ClassUtils.getName(null)).isNull();
  }

  @Test
  void getResourceName() {

    assertThat(ClassUtils.getResourceName(Object.class)).isEqualTo("java/lang/Object.class");
    assertThat(ClassUtils.getResourceName(Thread.class)).isEqualTo("java/lang/Thread.class");
    assertThat(ClassUtils.getResourceName(InputStream.class)).isEqualTo("java/io/InputStream.class");
    assertThat(ClassUtils.getResourceName(OutputStream.class)).isEqualTo("java/io/OutputStream.class");
    assertThat(ClassUtils.getResourceName(Socket.class)).isEqualTo("java/net/Socket.class");
  }

  @Test
  void getResourceNameWithNull() {
    assertThat(ClassUtils.getResourceName(null)).isNull();
  }

  @Test
  void getSimpleName() {

    assertThat(ClassUtils.getSimpleName(Object.class)).isEqualTo("Object");
    assertThat(ClassUtils.getSimpleName(Boolean.class)).isEqualTo("Boolean");
    assertThat(ClassUtils.getSimpleName(Character.class)).isEqualTo("Character");
    assertThat(ClassUtils.getSimpleName(Date.class)).isEqualTo("Date");
    assertThat(ClassUtils.getSimpleName(Integer.class)).isEqualTo("Integer");
    assertThat(ClassUtils.getSimpleName(Double.class)).isEqualTo("Double");
    assertThat(ClassUtils.getSimpleName(String.class)).isEqualTo("String");
  }

  @Test
  @SuppressWarnings("all")
  void getSimpleNameWithNull() {
    assertThat(ClassUtils.getName(null)).isNull();
  }

  @Test
  void hasMainMethodIsNullSafe() {
    assertThat(ClassUtils.hasMainMethod(null)).isFalse();
  }

  @Test
  void implementsInterfacesWithClassReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(ChildType.class)).isTrue();
  }

  @Test
  void implementsInterfacesWithClassReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(Object.class)).isFalse();
  }

  @Test
  void implementsInterfacesWithInterfaceReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(InterfaceOne.class)).isTrue();
  }

  @Test
  void implementsInterfacesWithInterfaceReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(InterfaceTwo.class)).isFalse();
  }

  @Test
  void implementsInterfacesWithObjectReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(new ParentType())).isTrue();
  }

  @Test
  void implementsInterfacesWithObjectReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(new Object())).isFalse();
  }

  @Test
  void instanceOf() {

    assertThat(ClassUtils.instanceOf(new Object(), Object.class)).isTrue();
    assertThat(ClassUtils.instanceOf("test", Object.class)).isTrue();
    assertThat(ClassUtils.instanceOf(123, Object.class)).isTrue();
    assertThat(ClassUtils.instanceOf(true, Object.class)).isTrue();
    assertThat(ClassUtils.instanceOf("test", String.class)).isTrue();
    assertThat(ClassUtils.instanceOf("123", String.class)).isTrue();
    assertThat(ClassUtils.instanceOf("false", String.class)).isTrue();
    assertThat(ClassUtils.instanceOf(123, Integer.class)).isTrue();
    assertThat(ClassUtils.instanceOf(123, Number.class)).isTrue();
    assertThat(ClassUtils.instanceOf(Class.class, Object.class)).isTrue();
    assertThat(ClassUtils.instanceOf(Object.class, Class.class)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  void notInstanceOf() {

    assertThat(ClassUtils.instanceOf(false, Number.class)).isFalse();
    assertThat(ClassUtils.instanceOf(123.0, Integer.class)).isFalse();
    assertThat(ClassUtils.instanceOf("123", Number.class)).isFalse();
    assertThat(ClassUtils.instanceOf(new Object(), null)).isFalse();
    assertThat(ClassUtils.instanceOf(null, null)).isFalse();
    assertThat(ClassUtils.instanceOf(null, Object.class)).isFalse();
  }

  @Test
  void isAnnotation() {
    assertThat(ClassUtils.isAnnotation(Documented.class)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  void isNotAnnotation() {

    assertThat(ClassUtils.isAnnotation(null)).isFalse();
    assertThat(ClassUtils.isAnnotation(Object.class)).isFalse();
    assertThat(ClassUtils.isAnnotation(String.class)).isFalse();
    assertThat(ClassUtils.isAnnotation(Object[].class)).isFalse();
    assertThat(ClassUtils.isAnnotation(int[][].class)).isFalse();
    assertThat(ClassUtils.isAnnotation(Class.class)).isFalse();
    assertThat(ClassUtils.isAnnotation(Thread.State.class)).isFalse();
    assertThat(ClassUtils.isAnnotation(Cloneable.class)).isFalse();
    assertThat(ClassUtils.isAnnotation(Integer.TYPE)).isFalse();
  }

  @Test
  void isAnnotationPresent() {

    assertThat(ClassUtils.isAnnotationPresent(Id.class, ReflectionUtils.getField(SubType.class, "id"))).isTrue();
    assertThat(ClassUtils.isAnnotationPresent(Id.class,
      ReflectionUtils.getField(SubType.class, "nonAnnotatedField"),
      ReflectionUtils.getMethod(SubType.class, "getId"))).isTrue();
    assertThat(ClassUtils.isAnnotationPresent(Deprecated.class,
      ReflectionUtils.getMethod(SubType.class, "deprecatedMethod"))).isTrue();
    assertThat(ClassUtils.isAnnotationPresent(Resource.class, SubType.class)).isTrue();
  }

  @Test
  void isAnnotationNotPresent() {

    assertThat(ClassUtils.isAnnotationPresent(Id.class,
      ReflectionUtils.getField(SubType.class, "nonAnnotatedField"),
      ReflectionUtils.getMethod(SubType.class, "nonAnnotatedMethod"))).isFalse();
    assertThat(ClassUtils.isAnnotationPresent(Deprecated.class,
      ReflectionUtils.getMethod(SubType.class, "nonAnnotatedMethod"))).isFalse();
    assertThat(ClassUtils.isAnnotationPresent(Resource.class, SuperType.class)).isFalse();
    assertThat(ClassUtils.isAnnotationPresent(Id.class)).isFalse();
    assertThat(ClassUtils.isAnnotationPresent(Deprecated.class, null, null, null)).isFalse();
  }

  @Test
  void isArray() {

    assertThat(ClassUtils.isArray(Object[].class)).isTrue();
    assertThat(ClassUtils.isArray(Object[][].class)).isTrue();
    assertThat(ClassUtils.isArray(String[].class)).isTrue();
    assertThat(ClassUtils.isArray(int[].class)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  void isNotArray() {

    assertThat(ClassUtils.isArray(null)).isFalse();
    assertThat(ClassUtils.isArray(Object.class)).isFalse();
    assertThat(ClassUtils.isArray(Documented.class)).isFalse();
    assertThat(ClassUtils.isArray(Object.class)).isFalse();
    assertThat(ClassUtils.isArray(Thread.State.class)).isFalse();
    assertThat(ClassUtils.isArray(Cloneable.class)).isFalse();
    assertThat(ClassUtils.isArray(Integer.TYPE)).isFalse();
  }

  @Test
  void isClass() {
    assertThat(ClassUtils.isClass(Object.class)).isTrue(); // class!
  }

  @Test
  void isNotClass() {

    assertThat(ClassUtils.isClass(null)).isFalse();
    assertThat(ClassUtils.isClass(int[].class)).isFalse(); // array
    assertThat(ClassUtils.isClass(Documented.class)).isFalse(); // annotation
    assertThat(ClassUtils.isClass(Thread.State.class)).isFalse(); // enum
    assertThat(ClassUtils.isClass(Cloneable.class)).isFalse(); // interface
    assertThat(ClassUtils.isClass(Integer.TYPE)).isFalse(); // primitive
  }

  @Test
  void isConstructorWithArrayParameterOnConstructorWithObjectArrayParameterIsTrue() {

    Constructor<?>[] constructors = TypeWithObjectArrayParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  void isConstructorWithArrayParameterOnConstructorWithStringArrayParameterIsTrue() {

    Constructor<?>[] constructors = TypeWithStringArrayParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(String[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  void isConstructorWithArrayParameterOnVarargsConstructorIsTrue() {

    Constructor<?>[] constructors = TypeWithVarargsConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  void isConstructorWithArrayParameterOnNullIsFalse() {
    assertThat(ClassUtils.isConstructorWithArrayParameter(null)).isFalse();
  }

  @Test
  void isConstructorWithArrayParameterOnConstructorWithObjectParameterIsFalse() {

    Constructor<?>[] constructors = TypeWithObjectParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  void isConstructorWithArrayParameterOnConstructorWithObjectArrayAndStringParameterIsFalse() {

    Constructor<?>[] constructors = TypeWithObjectArrayAndStringParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(2);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(constructor.getParameterTypes()[1]).isEqualTo(String.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  void isConstructorWithArrayParameterOnConstructorWithListParameterIsFalse() {

    Constructor<?>[] constructors = TypeWithObjectListParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(List.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  void isConstructorWithArrayParameterOnTwoArgumentConstructorIsFalse() {

    Constructor<?>[] constructors = TypeWithTwoArgumentConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(2);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object.class);
    assertThat(constructor.getParameterTypes()[1]).isEqualTo(Object.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  void isDefaultConstructorWithDefaultConstructorIsTrue() {

    Constructor<?>[] constructors = TypeWithWithDefaultConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPublic(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isTrue();
  }

  @Test
  void isDefaultConstructorWithNullIsFalse() {
    assertThat(ClassUtils.isDefaultConstructor(null)).isFalse();
  }

  @Test
  void isDefaultConstructorWithPrivateNoArgConstructorIsFalse() {

    Constructor<?>[] constructors = TypeWithPrivateNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPrivate(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  void isDefaultConstructorWithPackagePrivateNoArgConstructorIsFalse() {

    Constructor<?>[] constructors = TypeWithPackagePrivateNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPrivate(constructor.getModifiers())).isFalse();
    assertThat(Modifier.isProtected(constructor.getModifiers())).isFalse();
    assertThat(Modifier.isPublic(constructor.getModifiers())).isFalse();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  void isDefaultConstructorWithProtectedNoArgConstructorIsFalse() {

    Constructor<?>[] constructors = TypeWithProtectedNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isProtected(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  void isDefaultConstructorWithPublicArgConstructorIsFalse() {

    Constructor<?>[] constructors = TypeWithPublicArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(Modifier.isPublic(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  void isEnum() {
    assertThat(ClassUtils.isEnum(Thread.State.class)).isTrue();
  }

  @Test
  void isNotEnum() {

    assertThat(ClassUtils.isEnum(null)).isFalse();
    assertThat(ClassUtils.isEnum(int[].class)).isFalse();
    assertThat(ClassUtils.isEnum(Documented.class)).isFalse();
    assertThat(ClassUtils.isEnum(Object.class)).isFalse();
    assertThat(ClassUtils.isEnum(Runnable.class)).isFalse();
    assertThat(ClassUtils.isEnum(Integer.TYPE)).isFalse();
  }

  @Test
  void isInterface() {

    assertThat(ClassUtils.isInterface(Documented.class)).isTrue(); // true, even an enum type!
    assertThat(ClassUtils.isInterface(Runnable.class)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  void isNotInterface() {

    assertThat(ClassUtils.isInterface(null)).isFalse();
    assertThat(ClassUtils.isInterface(int[].class)).isFalse();
    assertThat(ClassUtils.isInterface(Object.class)).isFalse();
    assertThat(ClassUtils.isInterface(Thread.State.class)).isFalse();
    assertThat(ClassUtils.isInterface(Integer.TYPE)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  void isUnclassifiedReturnsTrue() {

    assertThat(ClassUtils.isUnclassified(null)).isTrue();
    assertThat(ClassUtils.isUnclassified(Object.class)).isTrue();
  }

  @Test
  void isUnclassifiedReturnsFalse() {

    Arrays.asList(Boolean.class, Character.class, Integer.class, Double.class, String.class, Enum.class, Instant.class)
      .forEach(type -> assertThat(ClassUtils.isUnclassified(type)).isFalse());
  }

  @Test
  void classWithMainMethodIsTrue() {
    assertThat(ClassUtils.hasMainMethod(ClassWithMainMethod.class)).isTrue();
  }

  @Test
  void classWithIncorrectlyNamedMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithMaineMethod.class)).isFalse();
  }

  @Test
  void classWithPrivateMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithPrivateMainMethod.class)).isFalse();
  }

  @Test
  void classWithNonStaticMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithNonStaticMainMethod.class)).isFalse();
  }

  @Test
  void classWithMainMethodReturningValueIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithMainMethodReturningValue.class)).isFalse();
  }

  @Test
  void classWithMultiArgumentMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithMultiArgumentMainMethod.class)).isFalse();
  }

  @Test
  void classWithObjectArrayParameterMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithObjectArrayParameterMainMethod.class)).isFalse();
  }

  @Test
  void classWithNoMainMethodIsFalse() {
    assertThat(ClassUtils.hasMainMethod(ClassWithNoMainMethod.class)).isFalse();
  }

  @Test
  void isMainMethodIsNullSafe() {
    assertThat(ClassUtils.isMainMethod(null)).isFalse();
  }

  @Test
  void isPresent() {
    assertThat(ClassUtils.isPresent("java.lang.Object")).isTrue();
  }

  @Test
  void isNotPresent() {
    assertThat(ClassUtils.isPresent("com.company.non.existing.Class")).isFalse();
  }

  @Test
  void isPrimitive() {

    assertThat(ClassUtils.isPrimitive(Boolean.TYPE)).isTrue();
    assertThat(ClassUtils.isPrimitive(Character.TYPE)).isTrue();
    assertThat(ClassUtils.isPrimitive(Double.TYPE)).isTrue();
    assertThat(ClassUtils.isPrimitive(Integer.TYPE)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  void isNotPrimitive() {

    assertThat(ClassUtils.isPrimitive(null)).isFalse();
    assertThat(ClassUtils.isPrimitive(int[].class)).isFalse();
    assertThat(ClassUtils.isPrimitive(double[][].class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Documented.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Object.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Boolean.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Character.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Integer.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Double.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(String.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Thread.State.class)).isFalse();
    assertThat(ClassUtils.isPrimitive(Runnable.class)).isFalse();
  }

  @Test
  void loadClass() {
    assertThat(ClassUtils.loadClass("java.lang.Object")).isEqualTo(Object.class);
  }

  @Test
  void loadNonExistingClass() {

    ThrowableAssertions.assertThatThrowableOfType(RuntimeException.class)
      .isThrownBy(args -> ClassUtils.loadClass("example.non.existing.Class"))
      .havingMessage("Class [example.non.existing.Class] was not found")
      .causedBy(ClassNotFoundException.class)
      .withNoCause();
  }

  @Test
  void locateClass() throws MalformedURLException {
    assertThat(ClassUtils.locateClass(ClassUtils.class.getName())).isEqualTo(new File(getClassesDirectory(),
      ClassUtils.class.getName().replaceAll("\\.", "/").concat(".class")).toURI().toURL());
  }

  @Test
  void nonInstanceOf() {

    assertThat(ClassUtils.notInstanceOf(new Object(), (Class<?>[]) null)).isTrue();
    assertThat(ClassUtils.notInstanceOf(new Object(), Boolean.class, Number.class, String.class)).isTrue();
    assertThat(ClassUtils.notInstanceOf(123.0, Boolean.class, BigDecimal.class, String.class)).isTrue();
  }

  @Test
  void notNonInstanceOf() {
    assertThat(ClassUtils.notInstanceOf(123, Boolean.class, Number.class, String.class)).isFalse();
  }

  @Test
  void toRawTypeWithObject() {
    assertThat(ClassUtils.toRawType(Object.class)).isEqualTo(Object.class);
  }

  @Test
  void toRawTypeWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ClassUtils.toRawType(null))
      .withMessage("[null] is not resolvable as a java.lang.Class")
      .withNoCause();
  }

  @Test
  void toRawTypeWithParameterizedType() {

    ParameterizedType mockParameterizedType = mock(ParameterizedType.class);

    when(mockParameterizedType.getRawType()).thenReturn(String.class);

    assertThat(ClassUtils.toRawType(mockParameterizedType)).isEqualTo(String.class);

    verify(mockParameterizedType, times(1)).getRawType();
  }

  @Test
  void toRawTypeWithTypeVariableHavingResolvableClassType() {

    TypeVariable<?> mockTypeVariable = mock(TypeVariable.class);

    when(mockTypeVariable.getName()).thenReturn("java.time.Instant");

    assertThat(ClassUtils.toRawType(mockTypeVariable)).isEqualTo(Instant.class);

    verify(mockTypeVariable, times(1)).getName();
  }

  @Test
  void toRawTypeWithTypeVariableHavingUnresolvableClassTypeReturnsObjectClass() {

    TypeVariable<?> mockTypeVariable = mock(TypeVariable.class);

    when(mockTypeVariable.getName()).thenReturn("T");

    assertThat(ClassUtils.toRawType(mockTypeVariable)).isEqualTo(Object.class);

    verify(mockTypeVariable, times(1)).getName();
  }

  public static class ClassWithMainMethod {
    public static void main(String[] args) { }
  }

  public static class ClassWithMaineMethod {
    public static void maine(String[] args) { }
  }

  @SuppressWarnings("all")
  public static class ClassWithPrivateMainMethod {
    private static void main(String[] args) { }
  }

  @SuppressWarnings("all")
  public static class ClassWithNonStaticMainMethod {
    void main(String[] args) { }
  }

  @SuppressWarnings("all")
  public static class ClassWithMainMethodReturningValue {
    public static Object main(String[] args) {
      return null;
    }
  }

  @SuppressWarnings("all")
  public static class ClassWithMultiArgumentMainMethod {
    public static void main(String[] args, Object argument) { }
  }

  @SuppressWarnings("all")
  public static class ClassWithObjectArrayParameterMainMethod {
    public static void main(Object[] args) { }
  }

  public static class ClassWithNoMainMethod { }

  @SuppressWarnings("unused")
  public static class SuperType {

    @Id
    private String id;

    private String stringValue;

    public SuperType() { }

    public SuperType(Object value) {
      this.stringValue = String.valueOf(value);
    }

    @Id
    public String getId() {
      return id;
    }

    @SuppressWarnings("all")
    public String getStringValue() {
      return this.stringValue;
    }

    public Object methodOne(String value) {
      return value;
    }

    public Object methodOne(Integer value) {
      return value;
    }

    void methodTwo(Boolean conditional, Number number, String string) { }

  }

  @Resource
  @SuppressWarnings("all")
  public static class SubType extends SuperType {

    private Character charValue;

    @Id
    private Long id;

    private Object nonAnnotatedField;

    public SubType(Long id) {
      this.id = id;
    }

    public SubType(Boolean condition, Number number, String string) { }

    public Character getCharacterValue() {
      return this.charValue;
    }

    void setCharacterValue(Character charValue) {
      this.charValue = charValue;
    }

    /**
     * @deprecated
     */
    @Deprecated
    void deprecatedMethod() { }

    @Override
    public String methodOne(String value) {
      return "$"+value;
    }

    public Object methodOne(Character value) {
      return value;
    }

    public Number methodOne(Integer wholeNumber, Double floatingPointNumber) {
      return Math.max(wholeNumber, floatingPointNumber);
    }

    void methodTwo(Boolean conditional, Integer number, String string) { }

    void methodTwo(String string, Number number, Boolean conditional) { }

    void nonAnnotatedMethod() { }

  }

  private static class TypeWithWithDefaultConstructor {
    public TypeWithWithDefaultConstructor() { }
  }

  private static class TypeWithPublicArgConstructor {
    public TypeWithPublicArgConstructor(@SuppressWarnings("unused") Object argument) { }
  }

  private static class TypeWithProtectedNoArgConstructor {
    protected TypeWithProtectedNoArgConstructor() { }
  }

  private static class TypeWithPackagePrivateNoArgConstructor {
    TypeWithPackagePrivateNoArgConstructor() { }
  }

  private static class TypeWithPrivateNoArgConstructor {
    private TypeWithPrivateNoArgConstructor() { }
  }

  private static class TypeWithObjectParameterConstructor {
    public TypeWithObjectParameterConstructor(@SuppressWarnings("unused") Object argument) { }
  }

  private static class TypeWithObjectArrayParameterConstructor {
    private TypeWithObjectArrayParameterConstructor(@SuppressWarnings("unused") Object[] args) { }
  }

  private static class TypeWithObjectArrayAndStringParameterConstructor {
    @SuppressWarnings("unused")
    public TypeWithObjectArrayAndStringParameterConstructor(Object[] args, String argument) { }
  }

  private static class TypeWithObjectListParameterConstructor {
    public TypeWithObjectListParameterConstructor(@SuppressWarnings("unused") List<Object> args) { }
  }

  private static class TypeWithStringArrayParameterConstructor {
    public TypeWithStringArrayParameterConstructor(@SuppressWarnings("unused") String[] args) {
    }
  }

  private static class TypeWithTwoArgumentConstructor {
    @SuppressWarnings("unused")
    public TypeWithTwoArgumentConstructor(Object argumentOne, Object argumentTwo) { }
  }

  private static class TypeWithVarargsConstructor {
    public TypeWithVarargsConstructor(@SuppressWarnings("unused") Object... args) { }
  }

  private interface InterfaceZero { }

  private interface InterfaceOne extends InterfaceZero { }

  private interface InterfaceTwo { }

  private interface InterfaceThree extends InterfaceOne, InterfaceZero { }

  private interface InterfaceFour extends InterfaceTwo { }

  private interface InterfaceFive extends InterfaceThree { }

  private static class GrandparentType implements InterfaceThree, InterfaceTwo { }

  private static class ParentType extends GrandparentType implements InterfaceFour { }

  private static class ChildType extends ParentType implements InterfaceFive { }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  private static class Person implements Nameable<String> {

    @lombok.NonNull
    private final String name;

  }

  @Documented
  @Inherited
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  private @interface Resource { }

  public static class ObjectWithPublicNoArgConstructor {

    public ObjectWithPublicNoArgConstructor() { }

  }

  public static class ObjectWithPublicNoArgConstructorThrowingException {

    public ObjectWithPublicNoArgConstructorThrowingException() {
      throw new RuntimeException("test");
    }
  }

  @Getter
  public static class ObjectWithPublicArgumentConstructor {

    private final Object value;

    public ObjectWithPublicArgumentConstructor(@Nullable Object value) {
      this.value = value;
    }
  }

  static class ObjectWithNonPublicNoArgConstructor {

    ObjectWithNonPublicNoArgConstructor() { }

  }

  @Getter
  protected static class ObjectWithNonDefaultConstructor {

    private final Object value;

    protected ObjectWithNonDefaultConstructor(@Nullable Object value) {
      this.value = value;
    }
  }

  public static class ObjectWithDefaultConstructor { }

}
