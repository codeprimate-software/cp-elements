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

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.hamcrest.Matchers.isA;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Documented;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.TypeVariable;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.Socket;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.cp.elements.lang.annotation.Id;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.ReflectionUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link ClassUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.reflect.Constructor
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.cp.elements.lang.ClassUtils
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ClassUtilsTests extends AbstractBaseTestSuite {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected <T> void assertConstructor(Constructor<T> constructor, Class<T> declaringClass, Class<?>[] parameterTypes) {
    assertThat(constructor).isNotNull();
    assertThat(constructor.getDeclaringClass()).isEqualTo(declaringClass);
    TestUtils.assertEquals(parameterTypes, constructor.getParameterTypes());
  }

  @Test
  public void assignableTo() {
    assertTrue(ClassUtils.assignableTo(null, Object.class));
    assertTrue(ClassUtils.assignableTo(null, Calendar.class));
    assertTrue(ClassUtils.assignableTo(null, Number.class));
    assertTrue(ClassUtils.assignableTo(null, String.class));
    assertTrue(ClassUtils.assignableTo(String.class, Object.class));
    assertTrue(ClassUtils.assignableTo(Number.class, Object.class));
    assertTrue(ClassUtils.assignableTo(Double.class, Number.class));
    assertTrue(ClassUtils.assignableTo(Integer.class, Number.class));
    assertTrue(ClassUtils.assignableTo(java.sql.Date.class, java.util.Date.class));
  }

  @Test
  public void notAssignableTo() {
    assertFalse(ClassUtils.assignableTo(null, null));
    assertFalse(ClassUtils.assignableTo(Object.class, null));
    assertFalse(ClassUtils.assignableTo(String.class, Number.class));
    assertFalse(ClassUtils.assignableTo(Character.class, String.class));
    assertFalse(ClassUtils.assignableTo(Object.class, String.class));
  }

  @Test
  public void getCla$$() {
    assertEquals(Object.class, ClassUtils.getClass(new Object()));
    assertEquals(Boolean.class, ClassUtils.getClass(true));
    assertEquals(Character.class, ClassUtils.getClass('c'));
    assertEquals(Date.class, ClassUtils.getClass(Calendar.getInstance().getTime()));
    assertEquals(Integer.class, ClassUtils.getClass(0));
    assertEquals(Double.class, ClassUtils.getClass(Math.PI));
    assertEquals(String.class, ClassUtils.getClass("null"));
  }

  @Test
  public void getClassWithNull() {
    assertThat(ClassUtils.getClass(null)).isNull();
  }

  @Test
  public void getClassName() {
    assertEquals("java.lang.Object", ClassUtils.getClassName(new Object()));
    assertEquals("java.lang.Boolean", ClassUtils.getClassName(Boolean.TRUE));
    assertEquals("java.lang.Character", ClassUtils.getClassName('$'));
    assertEquals("java.util.Date", ClassUtils.getClassName(Calendar.getInstance().getTime()));
    assertEquals("java.lang.Integer", ClassUtils.getClassName(2));
    assertEquals("java.lang.Double", ClassUtils.getClassName(0.0d));
    assertEquals("java.lang.String", ClassUtils.getClassName("test"));
  }

  @Test
  public void getClassNameWithNull() {
    assertThat(ClassUtils.getClassName(null)).isNull();
  }

  @Test
  public void getClassSimpleName() {
    assertEquals("Object", ClassUtils.getClassSimpleName(new Object()));
    assertEquals("Boolean", ClassUtils.getClassSimpleName(Boolean.TRUE));
    assertEquals("Character", ClassUtils.getClassSimpleName('!'));
    assertEquals("Date", ClassUtils.getClassSimpleName(Calendar.getInstance().getTime()));
    assertEquals("Integer", ClassUtils.getClassSimpleName(9));
    assertEquals("Double", ClassUtils.getClassSimpleName(1.0d));
    assertEquals("String", ClassUtils.getClassSimpleName("TEST"));
  }

  @Test
  public void getClassSimpleNameWithNull() {
    assertThat(ClassUtils.getClassSimpleName(null)).isNull();
  }

  @Test
  public void getInterfaceForClass() {
    assertThat(ClassUtils.getInterfaces(ChildType.class)).containsAll(asIterable(InterfaceFive.class,
      InterfaceFour.class, InterfaceThree.class, InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(ParentType.class)).containsAll(asIterable(InterfaceFour.class,
      InterfaceThree.class, InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));

    assertThat(ClassUtils.getInterfaces(GrandparentType.class)).containsAll(asIterable(InterfaceThree.class,
      InterfaceTwo.class, InterfaceOne.class, InterfaceZero.class));
  }

  @Test
  public void getInterfacesForInterface() {
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
  public void getInterfacesForObject() {
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
  public void getInterfacesForObjectClassHasNoInterfaces() {
    assertThat(ClassUtils.getInterfaces(new Object())).isEmpty();
  }

  @Test
  public void getInterfacesForNullClassReturnsEmptySet() {
    assertThat(ClassUtils.getInterfaces(null)).isEmpty();
  }

  @Test
  public void getInterfacesForNullObjectReturnsEmptySet() {
    assertThat(ClassUtils.getInterfaces((Object) null)).isEmpty();
  }

  @Test
  public void findConstructor() {
    Constructor<SubType> constructor = ClassUtils.findConstructor(SubType.class, 1L);
    assertConstructor(constructor, SubType.class, ArrayUtils.<Class>asArray(Long.class));
  }

  @Test
  public void findCompatibleConstructor() {
    Constructor<SuperType> constructor = ClassUtils.findConstructor(SuperType.class, "test");
    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class>asArray(Object.class));
  }

  @Test
  public void findNonExistingConstructor() {
    assertNull(ClassUtils.findConstructor(SubType.class, 1));
  }

  @Test
  public void findNonExistingNonMatchingConstructor() {
    assertNull(ClassUtils.findConstructor(SubType.class, "test", 1L, false));
  }

  @Test
  public void getConstructor() {
    Constructor<SuperType> constructor = ClassUtils.getConstructor(SuperType.class, Object.class);
    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class>asArray(Object.class));
  }

  @Test
  public void getNonExistingConstructor() {
    exception.expect(ConstructorNotFoundException.class);
    exception.expectCause(isA(NoSuchMethodException.class));

    ClassUtils.getConstructor(SuperType.class, String.class);
  }

  @Test
  public void resolveConstructor() {
    Constructor<SuperType> constructor = ClassUtils.resolveConstructor(SuperType.class,
      new Class[] { Object.class }, (Object[]) null);

    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class>asArray(Object.class));
  }

  @Test
  public void resolveMatchingConstructor() {
    Constructor<SubType> constructor = ClassUtils.resolveConstructor(SubType.class, new Class[0],
      true, 1L, "test");

    assertConstructor(constructor, SubType.class, ArrayUtils.<Class>asArray(Boolean.class, Number.class, String.class));
  }

  @Test
  public void resolveNonExistingNonMatchingConstructor() {
    exception.expect(ConstructorNotFoundException.class);
    exception.expectMessage(startsWith("Failed to resolve constructor with signature"));
    exception.expectCause(isA(NoSuchMethodException.class));

    ClassUtils.resolveConstructor(SubType.class, new Class<?>[] { Integer.class }, 2);
  }

  @Test
  public void getField() {
    Field charValueField = ClassUtils.getField(SubType.class, "charValue");

    assertNotNull(charValueField);
    assertEquals(SubType.class, charValueField.getDeclaringClass());
    assertEquals("charValue", charValueField.getName());
    assertEquals(Character.class, charValueField.getType());
  }

  @Test
  public void getFieldOnSuperClassFromSubClass() {
    Field stringValue = ClassUtils.getField(SubType.class, "stringValue");

    assertNotNull(stringValue);
    assertEquals(SuperType.class, stringValue.getDeclaringClass());
    assertEquals("stringValue", stringValue.getName());
    assertEquals(String.class, stringValue.getType());
  }

  @Test(expected = FieldNotFoundException.class)
  public void getFieldOnSubClassFromSuperClass() {
    ClassUtils.getField(SuperType.class, "charValue");
  }

  @Test(expected = FieldNotFoundException.class)
  public void getNonExistingField() {
    ClassUtils.getField(SubType.class, "nonExistingField");
  }

  @Test
  public void getOverriddenFieldOnSubClass() {
    Field idField = ClassUtils.getField(SubType.class, "id");

    assertNotNull(idField);
    assertEquals(SubType.class, idField.getDeclaringClass());
    assertEquals("id", idField.getName());
    assertEquals(Long.class, idField.getType());
  }

  @Test
  public void findMethod() {
    Method method = ClassUtils.findMethod(SubType.class, "methodTwo", true, 1, "test");

    assertNotNull(method);
    assertEquals(SubType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());

    method = ClassUtils.findMethod(SubType.class, "methodTwo", false, Math.PI, "test");

    assertNotNull(method);
    assertEquals(SuperType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());

    method = ClassUtils.findMethod(SuperType.class, "methodTwo", true, 1L, "test");

    assertNotNull(method);
    assertEquals(SuperType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());

    method = ClassUtils.findMethod(SubType.class, "methodTwo", "test", 1L, false);

    assertNotNull(method);
    assertEquals(SubType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());
  }

  @Test
  public void findNonExistingMethod() {
    assertNull(ClassUtils.findMethod(SubType.class, "methodThree", true, 1L, "test"));
    assertNull(ClassUtils.findMethod(SuperType.class, "methodTwo", "test", 1, false));
    assertNull(ClassUtils.findMethod(SubType.class, "methodTwo", true, "test", 1));
    assertNull(ClassUtils.findMethod(SubType.class, "methodTwo", true, 1));
    assertNull(ClassUtils.findMethod(SubType.class, "methodTwo", false, "1", 'C'));
  }

  @Test
  public void getMethod() {
    Method getCharacterValueMethod = ClassUtils.getMethod(SubType.class, "getCharacterValue");

    assertNotNull(getCharacterValueMethod);
    assertEquals(SubType.class, getCharacterValueMethod.getDeclaringClass());
    assertEquals("getCharacterValue", getCharacterValueMethod.getName());
    assertEquals(Character.class, getCharacterValueMethod.getReturnType());
  }

  @Test
  public void getMethodOnSuperClassFromSubClass() {
    Method getStringValue = ClassUtils.getMethod(SubType.class, "getStringValue");

    assertNotNull(getStringValue);
    assertEquals(SuperType.class, getStringValue.getDeclaringClass());
    assertEquals("getStringValue", getStringValue.getName());
    assertEquals(String.class, getStringValue.getReturnType());
  }

  @Test(expected = MethodNotFoundException.class)
  public void getMethodOnSubClassFromSuperClass() {
    ClassUtils.getMethod(SuperType.class, "getCharacterValue");
  }

  @Test(expected = MethodNotFoundException.class)
  public void getNonExistingMethod() {
    ClassUtils.getMethod(SubType.class, "nonExistingMethod");
  }

  @Test
  public void getOverloadedMethod() {
    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", Integer.class, Double.class);

    assertNotNull(methodOne);
    assertEquals(SubType.class, methodOne.getDeclaringClass());
    assertEquals("methodOne", methodOne.getName());
    assertEquals(Number.class, methodOne.getReturnType());
  }

  @Test
  public void getOverloadedMethodOnSuperClassFromSubClass() {
    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", Integer.class);

    assertNotNull(methodOne);
    assertEquals(SuperType.class, methodOne.getDeclaringClass());
    assertEquals("methodOne", methodOne.getName());
    assertEquals(Object.class, methodOne.getReturnType());
  }

  @Test
  public void getOverriddenMethod() {
    Method methodOne = ClassUtils.getMethod(SubType.class, "methodOne", String.class);

    assertNotNull(methodOne);
    assertEquals(SubType.class, methodOne.getDeclaringClass());
    assertEquals("methodOne", methodOne.getName());
    assertEquals(String.class, methodOne.getReturnType());

    methodOne = ClassUtils.getMethod(SuperType.class, "methodOne", String.class);

    assertNotNull(methodOne);
    assertEquals(SuperType.class, methodOne.getDeclaringClass());
    assertEquals("methodOne", methodOne.getName());
    assertEquals(Object.class, methodOne.getReturnType());
  }

  @Test(expected = MethodNotFoundException.class)
  public void getOverriddenMethodOnSubClassFromSuperClass() {
    ClassUtils.getMethod(SuperType.class, "methodOne", Boolean.class);
  }

  @Test
  public void resolveMethodToSubClass() {
    Method method = ClassUtils.resolveMethod(SubType.class, "methodTwo",
      ArrayUtils.<Class<?>>asArray(Boolean.class, Integer.class, String.class),
        ArrayUtils.asArray(true, 1, "test"), Void.class);

    assertNotNull(method);
    assertEquals(SubType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());
  }

  @Test
  public void resolveMethodToSuperClass() {
    Method method = ClassUtils.resolveMethod(SubType.class, "methodTwo",
      ArrayUtils.<Class<?>>asArray(Boolean.class, Long.class, String.class),
        ArrayUtils.asArray(true, 1L, "test"), Void.class);

    assertNotNull(method);
    assertEquals(SuperType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());
  }

  @Test(expected = MethodNotFoundException.class)
  public void unresolvableMethod() {
    try {
      ClassUtils.resolveMethod(SubType.class, "methodTwo",
        ArrayUtils.<Class<?>>asArray(Boolean.class, Character.class, Integer.class, Double.class, String.class),
          ArrayUtils.asArray(false, 'c', 1, Math.PI, "test"), Void.class);
    }
    catch (MethodNotFoundException expected) {
      assertEquals(String.format("Failed to resolve method with signature [methodTwo(:Boolean, :Character, :Integer, :Double, :String):void] on class type [%s]",
        SubType.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void getMethodSignature() {
    assertEquals("methodOne(:Boolean, :Character, :Integer, :Double, :String):Object",
      ClassUtils.getMethodSignature("methodOne", ArrayUtils.<Class<?>>asArray(Boolean.class, Character.class,
        Integer.class, Double.class, String.class), Object.class));
    assertEquals("methodTwo(:String):void", ClassUtils.getMethodSignature("methodTwo",
      ArrayUtils.<Class<?>>asArray(String.class), Void.class));
    assertEquals("methodThree():void", ClassUtils.getMethodSignature("methodThree", new Class[0], null));
    assertEquals("methodFour():void", ClassUtils.getMethodSignature("methodFour", null, null));
    assertEquals("methodFive(:Object, :null):String", ClassUtils.getMethodSignature("methodFive",
      ArrayUtils.<Class<?>>asArray(Object.class, null), String.class));
  }

  @Test
  public void getMethodSignatureWithMethod() {
    assertEquals("methodOne(:Integer, :Double):Number", ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "methodOne", ArrayUtils.<Class<?>>asArray(Integer.class, Double.class))));
    assertEquals("methodTwo(:Boolean, :Integer, :String):void", ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "methodTwo", ArrayUtils.<Class<?>>asArray(Boolean.class, Integer.class, String.class))));
    assertEquals("methodTwo(:Boolean, :Number, :String):void", ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SuperType.class, "methodTwo", ArrayUtils.<Class<?>>asArray(Boolean.class, Number.class, String.class))));
    assertEquals("getId():String", ClassUtils.getMethodSignature(ClassUtils.getMethod(SubType.class, "getId")));
    assertEquals("deprecatedMethod():void", ClassUtils.getMethodSignature(ClassUtils.getMethod(
      SubType.class, "deprecatedMethod")));
  }

  @Test
  public void getName() {
    assertEquals("java.lang.Object", ClassUtils.getName(Object.class));
    assertEquals("java.lang.Boolean", ClassUtils.getName(Boolean.class));
    assertEquals("java.lang.Character", ClassUtils.getName(Character.class));
    assertEquals("java.util.Date", ClassUtils.getName(Date.class));
    assertEquals("java.lang.Integer", ClassUtils.getName(Integer.class));
    assertEquals("java.lang.Double", ClassUtils.getName(Double.class));
    assertEquals("java.lang.String", ClassUtils.getName(String.class));
  }

  @Test
  public void getNameWithNull() {
    assertThat(ClassUtils.getName(null)).isNull();
  }

  @Test
  public void getResourceName() {
    assertThat(ClassUtils.getResourceName(Object.class)).isEqualTo("java/lang/Object.class");
    assertThat(ClassUtils.getResourceName(Thread.class)).isEqualTo("java/lang/Thread.class");
    assertThat(ClassUtils.getResourceName(InputStream.class)).isEqualTo("java/io/InputStream.class");
    assertThat(ClassUtils.getResourceName(OutputStream.class)).isEqualTo("java/io/OutputStream.class");
    assertThat(ClassUtils.getResourceName(Socket.class)).isEqualTo("java/net/Socket.class");
  }

  @Test
  public void getResourceNameWithNull() {
    assertThat(ClassUtils.getResourceName(null)).isNull();
  }

  @Test
  public void getSimpleName() {
    assertEquals("Object", ClassUtils.getSimpleName(Object.class));
    assertEquals("Boolean", ClassUtils.getSimpleName(Boolean.class));
    assertEquals("Character", ClassUtils.getSimpleName(Character.class));
    assertEquals("Date", ClassUtils.getSimpleName(Date.class));
    assertEquals("Integer", ClassUtils.getSimpleName(Integer.class));
    assertEquals("Double", ClassUtils.getSimpleName(Double.class));
    assertEquals("String", ClassUtils.getSimpleName(String.class));
  }

  @Test
  public void getSimpleNameWithNull() {
    assertNull(ClassUtils.getName(null));
  }

  @Test
  public void hasMainMethodIsNullSafe() {
    assertFalse(ClassUtils.hasMainMethod(null));
  }

  @Test
  public void implementsInterfacesWithClassReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(ChildType.class)).isTrue();
  }

  @Test
  public void implementsInterfacesWithClassReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(Object.class)).isFalse();
  }

  @Test
  public void implementsInterfacesWithInterfaceReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(InterfaceOne.class)).isTrue();
  }

  @Test
  public void implementsInterfacesWithInterfaceReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(InterfaceTwo.class)).isFalse();
  }

  @Test
  public void implementsInterfacesWithObjectReturnsTrue() {
    assertThat(ClassUtils.implementsInterfaces(new ParentType())).isTrue();
  }

  @Test
  public void implementsInterfacesWithObjectReturnsFalse() {
    assertThat(ClassUtils.implementsInterfaces(new Object())).isFalse();
  }

  @Test
  public void instanceOf() {
    assertTrue(ClassUtils.instanceOf(new Object(), Object.class));
    assertTrue(ClassUtils.instanceOf("test", Object.class));
    assertTrue(ClassUtils.instanceOf(123, Object.class));
    assertTrue(ClassUtils.instanceOf(true, Object.class));
    assertTrue(ClassUtils.instanceOf("test", String.class));
    assertTrue(ClassUtils.instanceOf("123", String.class));
    assertTrue(ClassUtils.instanceOf("false", String.class));
    assertTrue(ClassUtils.instanceOf(123, Integer.class));
    assertTrue(ClassUtils.instanceOf(123, Number.class));
    assertTrue(ClassUtils.instanceOf(Class.class, Object.class));
    assertTrue(ClassUtils.instanceOf(Object.class, Class.class));
  }

  @Test
  public void notInstanceOf() {
    assertFalse(ClassUtils.instanceOf(false, Number.class));
    assertFalse(ClassUtils.instanceOf(123.0, Integer.class));
    assertFalse(ClassUtils.instanceOf("123", Number.class));
    assertFalse(ClassUtils.instanceOf(new Object(), null));
    assertFalse(ClassUtils.instanceOf(null, null));
    assertFalse(ClassUtils.instanceOf(null, Object.class));
  }

  @Test
  public void isAnnotation() {
    assertTrue(ClassUtils.isAnnotation(Documented.class));
  }

  @Test
  public void isNotAnnotation() {
    assertFalse(ClassUtils.isAnnotation(null));
    assertFalse(ClassUtils.isAnnotation(Object.class));
    assertFalse(ClassUtils.isAnnotation(String.class));
    assertFalse(ClassUtils.isAnnotation(Object[].class));
    assertFalse(ClassUtils.isAnnotation(int[][].class));
    assertFalse(ClassUtils.isAnnotation(Class.class));
    assertFalse(ClassUtils.isAnnotation(Thread.State.class));
    assertFalse(ClassUtils.isAnnotation(Cloneable.class));
    assertFalse(ClassUtils.isAnnotation(Integer.TYPE));
  }

  @Test
  public void isAnnotationPresent() {
    assertTrue(ClassUtils.isAnnotationPresent(Id.class, ReflectionUtils.getField(SubType.class, "id")));
    assertTrue(ClassUtils.isAnnotationPresent(Id.class,
      ReflectionUtils.getField(SubType.class, "nonAnnotatedField"),
        ReflectionUtils.getMethod(SubType.class, "getId")));
    assertTrue(ClassUtils.isAnnotationPresent(Deprecated.class,
      ReflectionUtils.getMethod(SubType.class, "deprecatedMethod")));
    assertTrue(ClassUtils.isAnnotationPresent(Resource.class, SubType.class));
  }

  @Test
  public void isAnnotationNotPresent() {
    assertFalse(ClassUtils.isAnnotationPresent(Id.class,
      ReflectionUtils.getField(SubType.class, "nonAnnotatedField"),
      ReflectionUtils.getMethod(SubType.class, "nonAnnotatedMethod")));
    assertFalse(ClassUtils.isAnnotationPresent(Deprecated.class,
      ReflectionUtils.getMethod(SubType.class, "nonAnnotatedMethod")));
    assertFalse(ClassUtils.isAnnotationPresent(Resource.class, SuperType.class));
    assertFalse(ClassUtils.isAnnotationPresent(Id.class));
    assertFalse(ClassUtils.isAnnotationPresent(Deprecated.class, null, null, null));
  }

  @Test
  public void isArray() {
    assertTrue(ClassUtils.isArray(Object[].class));
    assertTrue(ClassUtils.isArray(Object[][].class));
    assertTrue(ClassUtils.isArray(String[].class));
    assertTrue(ClassUtils.isArray(int[].class));
  }

  @Test
  public void isNotArray() {
    assertFalse(ClassUtils.isArray(null));
    assertFalse(ClassUtils.isArray(Object.class));
    assertFalse(ClassUtils.isArray(Documented.class));
    assertFalse(ClassUtils.isArray(Object.class));
    assertFalse(ClassUtils.isArray(Thread.State.class));
    assertFalse(ClassUtils.isArray(Cloneable.class));
    assertFalse(ClassUtils.isArray(Integer.TYPE));
  }

  @Test
  public void isClass() {
    assertTrue(ClassUtils.isClass(Object.class)); // class!
  }

  @Test
  public void isNotClass() {
    assertFalse(ClassUtils.isClass(null));
    assertFalse(ClassUtils.isClass(int[].class)); // array
    assertFalse(ClassUtils.isClass(Documented.class)); // annotation
    assertFalse(ClassUtils.isClass(Thread.State.class)); // enum
    assertFalse(ClassUtils.isClass(Cloneable.class)); // interface
    assertFalse(ClassUtils.isClass(Integer.TYPE)); // primitive
  }

  @Test
  public void isConstructorWithArrayParameterOnConstructorWithObjectArrayParameterIsTrue() {
    Constructor[] constructors = TypeWithObjectArrayParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  public void isConstructorWithArrayParameterOnConstructorWithStringArrayParameterIsTrue() {
    Constructor[] constructors = TypeWithStringArrayParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(String[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  public void isConstructorWithArrayParameterOnVarargsConstructorIsTrue() {
    Constructor[] constructors = TypeWithVarargsConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isTrue();
  }

  @Test
  public void isConstructorWithArrayParameterOnNullIsFalse() {
    assertThat(ClassUtils.isConstructorWithArrayParameter(null)).isFalse();
  }

  @Test
  public void isConstructorWithArrayParameterOnConstructorWithObjectParameterIsFalse() {
    Constructor[] constructors = TypeWithObjectParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  public void isConstructorWithArrayParameterOnConstructorWithObjectArrayAndStringParameterIsFalse() {
    Constructor[] constructors = TypeWithObjectArrayAndStringParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(2);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object[].class);
    assertThat(constructor.getParameterTypes()[1]).isEqualTo(String.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  public void isConstructorWithArrayParameterOnConstructorWithListParameterIsFalse() {
    Constructor[] constructors = TypeWithObjectListParameterConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(List.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  public void isConstructorWithArrayParameterOnTwoArgumentConstructorIsFalse() {
    Constructor[] constructors = TypeWithTwoArgumentConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor constructor = constructors[0];

    assertThat(constructor).isNotNull();
    assertThat(constructor.getParameterCount()).isEqualTo(2);
    assertThat(constructor.getParameterTypes()[0]).isEqualTo(Object.class);
    assertThat(constructor.getParameterTypes()[1]).isEqualTo(Object.class);
    assertThat(ClassUtils.isConstructorWithArrayParameter(constructor)).isFalse();
  }

  @Test
  public void isDefaultConstructorWithDefaultConstructorIsTrue() {
    Constructor[] constructors = TypeWithWithDefaultConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPublic(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isTrue();
  }

  @Test
  public void isDefaultConstructorWithNullIsFalse() {
    assertThat(ClassUtils.isDefaultConstructor(null)).isFalse();
  }

  @Test
  public void isDefaultConstructorWithPrivateNoArgConstructorIsFalse() {
    Constructor[] constructors = TypeWithPrivateNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPrivate(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  public void isDefaultConstructorWithPackagePrivateNoArgConstructorIsFalse() {
    Constructor[] constructors = TypeWithPackagePrivateNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isPrivate(constructor.getModifiers())).isFalse();
    assertThat(Modifier.isProtected(constructor.getModifiers())).isFalse();
    assertThat(Modifier.isPublic(constructor.getModifiers())).isFalse();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  public void isDefaultConstructorWithProtectedNoArgConstructorIsFalse() {
    Constructor[] constructors = TypeWithProtectedNoArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(0);
    assertThat(Modifier.isProtected(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  public void isDefaultConstructorWithPublicArgConstructorIsFalse() {
    Constructor[] constructors = TypeWithPublicArgConstructor.class.getDeclaredConstructors();

    assertThat(constructors).hasSize(1);

    Constructor<?> constructor = constructors[0];

    assertThat(constructor.getParameterCount()).isEqualTo(1);
    assertThat(Modifier.isPublic(constructor.getModifiers())).isTrue();
    assertThat(ClassUtils.isDefaultConstructor(constructor)).isFalse();
  }

  @Test
  public void isEnum() {
    assertTrue(ClassUtils.isEnum(Thread.State.class));
  }

  @Test
  public void isNotEnum() {
    assertFalse(ClassUtils.isEnum(null));
    assertFalse(ClassUtils.isEnum(int[].class));
    assertFalse(ClassUtils.isEnum(Documented.class));
    assertFalse(ClassUtils.isEnum(Object.class));
    assertFalse(ClassUtils.isEnum(Runnable.class));
    assertFalse(ClassUtils.isEnum(Integer.TYPE));
  }

  @Test
  public void isInterface() {
    assertTrue(ClassUtils.isInterface(Documented.class)); // true, even an enum type!
    assertTrue(ClassUtils.isInterface(Runnable.class));
  }

  @Test
  public void isNotInterface() {
    assertFalse(ClassUtils.isInterface(null));
    assertFalse(ClassUtils.isInterface(int[].class));
    assertFalse(ClassUtils.isInterface(Object.class));
    assertFalse(ClassUtils.isInterface(Thread.State.class));
    assertFalse(ClassUtils.isInterface(Integer.TYPE));
  }

  @Test
  public void classWithMainMethodIsTrue() {
    assertTrue(ClassUtils.hasMainMethod(ClassWithMainMethod.class));
  }

  @Test
  public void classWithIncorrectlyNamedMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithMaintMethod.class));
  }

  @Test
  public void classWithPrivateMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithPrivateMainMethod.class));
  }

  @Test
  public void classWithNonStaticMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithNonStaticMainMethod.class));
  }

  @Test
  public void classWithMainMethodReturningValueIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithMainMethodReturningValue.class));
  }

  @Test
  public void classWithMultiArgumentMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithMultiArgumentMainMethod.class));
  }

  @Test
  public void classWithObjectArrayParameterMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithObjectArrayParameterMainMethod.class));
  }

  @Test
  public void classWithNoMainMethodIsFalse() {
    assertFalse(ClassUtils.hasMainMethod(ClassWithNoMainMethod.class));
  }

  @Test
  public void isMainMethodIsNullSafe() {
    assertFalse(ClassUtils.isMainMethod(null));
  }

  @Test
  public void isPresent() {
    assertTrue(ClassUtils.isPresent("java.lang.Object"));
  }

  @Test
  public void isNotPresent() {
    assertFalse(ClassUtils.isPresent("com.company.non.existing.Class"));
  }

  @Test
  public void isPrimitive() {
    assertTrue(ClassUtils.isPrimitive(Boolean.TYPE));
    assertTrue(ClassUtils.isPrimitive(Character.TYPE));
    assertTrue(ClassUtils.isPrimitive(Double.TYPE));
    assertTrue(ClassUtils.isPrimitive(Integer.TYPE));
  }

  @Test
  public void isNotPrimitive() {
    assertFalse(ClassUtils.isPrimitive(null));
    assertFalse(ClassUtils.isPrimitive(int[].class));
    assertFalse(ClassUtils.isPrimitive(double[][].class));
    assertFalse(ClassUtils.isPrimitive(Documented.class));
    assertFalse(ClassUtils.isPrimitive(Object.class));
    assertFalse(ClassUtils.isPrimitive(Boolean.class));
    assertFalse(ClassUtils.isPrimitive(Character.class));
    assertFalse(ClassUtils.isPrimitive(Integer.class));
    assertFalse(ClassUtils.isPrimitive(Double.class));
    assertFalse(ClassUtils.isPrimitive(String.class));
    assertFalse(ClassUtils.isPrimitive(Thread.State.class));
    assertFalse(ClassUtils.isPrimitive(Runnable.class));
  }

  @Test
  public void loadClass() {
    assertThat(ClassUtils.loadClass("java.lang.Object")).isEqualTo(Object.class);
  }

  @Test(expected = RuntimeException.class)
  public void loadNonExistingClass() {
    try {
      ClassUtils.loadClass("example.non.existing.Class");
    }
    catch (RuntimeException expected) {
      assertThat(expected).hasMessage("Class [example.non.existing.Class] was not found");
      assertThat(expected).hasCauseInstanceOf(ClassNotFoundException.class);

      throw expected;
    }
  }

  @Test
  public void locateClass() throws MalformedURLException {
    assertThat(ClassUtils.locateClass(ClassUtils.class.getName())).isEqualTo(new File(getClassesDirectory(),
      ClassUtils.class.getName().replaceAll("\\.", "/").concat(".class")).toURI().toURL());
  }

  @Test
  public void nonInstanceOf() {
    assertTrue(ClassUtils.notInstanceOf(new Object(), (Class[]) null));
    assertTrue(ClassUtils.notInstanceOf(new Object(), Boolean.class, Number.class, String.class));
    assertTrue(ClassUtils.notInstanceOf(123.0, Boolean.class, BigDecimal.class, String.class));
  }

  @Test
  public void notNonInstanceOf() {
    assertFalse(ClassUtils.notInstanceOf(123, Boolean.class, Number.class, String.class));
  }

  @Test
  public void toRawTypeWithObject() {
    assertThat(ClassUtils.toRawType(Object.class)).isEqualTo(Object.class);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toRawTypeWithNullThrowsIllegalArgumentException() {

    try {
      ClassUtils.toRawType(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[null] is not resolvable as a java.lang.Class");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void toRawTypeWithParameterizedType() {

    ParameterizedType mockParameterizedType = mock(ParameterizedType.class);

    when(mockParameterizedType.getRawType()).thenReturn(String.class);

    assertThat(ClassUtils.toRawType(mockParameterizedType)).isEqualTo(String.class);

    verify(mockParameterizedType, times(1)).getRawType();
  }

  @Test
  public void toRawTypeWithTypeVariableHavingResolvableClassType() {

    TypeVariable<?> mockTypeVariable = mock(TypeVariable.class);

    when(mockTypeVariable.getName()).thenReturn("java.time.Instant");

    assertThat(ClassUtils.toRawType(mockTypeVariable)).isEqualTo(Instant.class);

    verify(mockTypeVariable, times(1)).getName();
  }

  @Test
  public void toRawTypeWithTypeVariableHavingUnresolvableClassTypeReturnsObjectClass() {

    TypeVariable<?> mockTypeVariable = mock(TypeVariable.class);

    when(mockTypeVariable.getName()).thenReturn("T");

    assertThat(ClassUtils.toRawType(mockTypeVariable)).isEqualTo(Object.class);

    verify(mockTypeVariable, times(1)).getName();
  }

  public static class ClassWithMainMethod {
    public static void main(String[] args) { }
  }

  public static class ClassWithMaintMethod {
    public static void maint(String[] args) { }
  }

  public static class ClassWithPrivateMainMethod {
    private static void main(String[] args) { }
  }

  public static class ClassWithNonStaticMainMethod {
    public void main(String[] args) { }
  }

  public static class ClassWithMainMethodReturningValue {
    public static Object main(String[] args) {
      return null;
    }
  }

  public static class ClassWithMultiArgumentMainMethod {
    public static void main(String[] args, Object argument) { }
  }

  public static class ClassWithObjectArrayParameterMainMethod {
    public static void main(Object[] args) { }
  }

  public static class ClassWithNoMainMethod { }

  @SuppressWarnings("unused")
  public static class SuperType {

    @Id
    private String id;

    private String stringValue;

    public SuperType() {
    }

    public SuperType(Object value) {
      this.stringValue = String.valueOf(value);
    }

    @Id
    public String getId() {
      return id;
    }

    public String getStringValue() {
      return stringValue;
    }

    public Object methodOne(String value) {
      return value;
    }

    public Object methodOne(Integer value) {
      return value;
    }

    public void methodTwo(Boolean conditional, Number number, String string) {
    }
  }

  @Resource
  @SuppressWarnings("unused")
  public static class SubType extends SuperType {

    private Character charValue;

    @Id
    private Long id;

    private Object nonAnnotatedField;

    public SubType(Long id) {
      this.id = id;
    }

    public SubType(Boolean condition, Number number, String string) {
    }

    public Character getCharacterValue() {
      return charValue;
    }

    public void setCharacterValue(Character charValue) {
      this.charValue = charValue;
    }

    /**
     * @deprecated
     */
    @Deprecated
    public void deprecatedMethod() {
    }

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

    public void methodTwo(Boolean conditional, Integer number, String string) {
    }

    public void methodTwo(String string, Number number, Boolean conditional) {
    }

    public void nonAnnotatedMethod() {
    }
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

}
