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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.lang.annotation.Documented;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import javax.annotation.Resource;

import org.cp.elements.lang.annotation.Id;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.ReflectionUtils;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The ClassUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ClassUtils class.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see org.cp.elements.lang.ClassUtils
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Rule
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ClassUtilsTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  protected <T> void assertConstructor(final Constructor<T> constructor, final Class<T> declaringClass, final Class<?>[] parameterTypes) {
    assertThat(constructor, is(not(nullValue())));
    assertThat(constructor.getDeclaringClass(), is(equalTo(declaringClass)));
    TestUtils.assertEquals(parameterTypes, constructor.getParameterTypes());
  }

  @Test
  public void assignableTo() {
    assertFalse(ClassUtils.assignableTo(null, null));
    assertFalse(ClassUtils.assignableTo(Object.class, null));
    assertTrue(ClassUtils.assignableTo(null, Object.class));
    assertTrue(ClassUtils.assignableTo(null, Calendar.class));
    assertTrue(ClassUtils.assignableTo(null, Number.class));
    assertTrue(ClassUtils.assignableTo(null, String.class));
    assertTrue(ClassUtils.assignableTo(String.class, Object.class));
    assertTrue(ClassUtils.assignableTo(Number.class, Object.class));
    assertTrue(ClassUtils.assignableTo(Double.class, Number.class));
    assertTrue(ClassUtils.assignableTo(Integer.class, Number.class));
    assertTrue(ClassUtils.assignableTo(java.sql.Date.class, java.util.Date.class));
    assertFalse(ClassUtils.assignableTo(String.class, Number.class));
    assertFalse(ClassUtils.assignableTo(Character.class, String.class));
    assertFalse(ClassUtils.assignableTo(Object.class, String.class));
  }

  @Test
  public void getClass$() {
    assertNull(ClassUtils.getClass(null));
    assertEquals(Object.class, ClassUtils.getClass(new Object()));
    assertEquals(Boolean.class, ClassUtils.getClass(true));
    assertEquals(Character.class, ClassUtils.getClass('c'));
    assertEquals(Date.class, ClassUtils.getClass(Calendar.getInstance().getTime()));
    assertEquals(Integer.class, ClassUtils.getClass(0));
    assertEquals(Double.class, ClassUtils.getClass(Math.PI));
    assertEquals(String.class, ClassUtils.getClass("null"));
  }

  @Test
  public void getClassName() {
    assertNull(ClassUtils.getClassName(null));
    assertEquals("java.lang.Object", ClassUtils.getClassName(new Object()));
    assertEquals("java.lang.Boolean", ClassUtils.getClassName(Boolean.TRUE));
    assertEquals("java.lang.Character", ClassUtils.getClassName('$'));
    assertEquals("java.util.Date", ClassUtils.getClassName(Calendar.getInstance().getTime()));
    assertEquals("java.lang.Integer", ClassUtils.getClassName(2));
    assertEquals("java.lang.Double", ClassUtils.getClassName(0.0d));
    assertEquals("java.lang.String", ClassUtils.getClassName("test"));
  }

  @Test
  public void getClassSimpleName() {
    assertNull(ClassUtils.getClassSimpleName(null));
    assertEquals("Object", ClassUtils.getClassSimpleName(new Object()));
    assertEquals("Boolean", ClassUtils.getClassSimpleName(Boolean.TRUE));
    assertEquals("Character", ClassUtils.getClassSimpleName('!'));
    assertEquals("Date", ClassUtils.getClassSimpleName(Calendar.getInstance().getTime()));
    assertEquals("Integer", ClassUtils.getClassSimpleName(9));
    assertEquals("Double", ClassUtils.getClassSimpleName(1.0d));
    assertEquals("String", ClassUtils.getClassSimpleName("TEST"));
  }

  @Test
  public void findConstructor() {
    Constructor<SubType> constructor = ClassUtils.findConstructor(SubType.class, 1l);
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
    assertNull(ClassUtils.findConstructor(SubType.class, "test", 1l, false));
  }

  @Test
  public void getConstructor() {
    Constructor<SuperType> constructor = ClassUtils.getConstructor(SuperType.class, Object.class);
    assertConstructor(constructor, SuperType.class, ArrayUtils.<Class>asArray(Object.class));
  }

  @Test
  public void getNonExistingConstructor() {
    expectedException.expect(ConstructorNotFoundException.class);
    expectedException.expectCause(isA(NoSuchMethodException.class));
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
    Constructor<SubType> constructor = ClassUtils.resolveConstructor(SubType.class, new Class[0], true, 1l, "test");

    assertConstructor(constructor, SubType.class, ArrayUtils.<Class>asArray(Boolean.class, Number.class, String.class));
  }

  @Test
  public void resolveNonExistingNonMatchingConstructor() {
    expectedException.expect(ConstructorNotFoundException.class);
    expectedException.expectMessage(startsWith("Failed to resolve constructor with signature"));
    expectedException.expectCause(isA(NoSuchMethodException.class));
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

    method = ClassUtils.findMethod(SuperType.class, "methodTwo", true, 1l, "test");

    assertNotNull(method);
    assertEquals(SuperType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());

    method = ClassUtils.findMethod(SubType.class, "methodTwo", "test", 1l, false);

    assertNotNull(method);
    assertEquals(SubType.class, method.getDeclaringClass());
    assertEquals("methodTwo", method.getName());
  }

  @Test
  public void findNonExistingMethod() {
    assertNull(ClassUtils.findMethod(SubType.class, "methodThree", true, 1l, "test"));
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
        ArrayUtils.asArray(true, 1l, "test"), Void.class);

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
      assertEquals(String.format("Failed to resolve method with signature (methodTwo(:Boolean, :Character, :Integer, :Double, :String):void) on class type (%1$s)!",
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
    assertNull(ClassUtils.getName(null));
    assertEquals("java.lang.Object", ClassUtils.getName(Object.class));
    assertEquals("java.lang.Boolean", ClassUtils.getName(Boolean.class));
    assertEquals("java.lang.Character", ClassUtils.getName(Character.class));
    assertEquals("java.util.Date", ClassUtils.getName(Date.class));
    assertEquals("java.lang.Integer", ClassUtils.getName(Integer.class));
    assertEquals("java.lang.Double", ClassUtils.getName(Double.class));
    assertEquals("java.lang.String", ClassUtils.getName(String.class));
  }

  @Test
  public void getSimpleName() {
    assertNull(ClassUtils.getName(null));
    assertEquals("Object", ClassUtils.getSimpleName(Object.class));
    assertEquals("Boolean", ClassUtils.getSimpleName(Boolean.class));
    assertEquals("Character", ClassUtils.getSimpleName(Character.class));
    assertEquals("Date", ClassUtils.getSimpleName(Date.class));
    assertEquals("Integer", ClassUtils.getSimpleName(Integer.class));
    assertEquals("Double", ClassUtils.getSimpleName(Double.class));
    assertEquals("String", ClassUtils.getSimpleName(String.class));
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
    assertFalse(ClassUtils.instanceOf(false, Number.class));
    assertFalse(ClassUtils.instanceOf(123.0, Integer.class));
    assertFalse(ClassUtils.instanceOf("123", Number.class));
    assertFalse(ClassUtils.instanceOf(new Object(), null));
    assertFalse(ClassUtils.instanceOf(null, null));
    assertFalse(ClassUtils.instanceOf(null, Object.class));
  }

  @Test
  public void isAnnotation() {
    assertFalse(ClassUtils.isAnnotation(null));
    assertFalse(ClassUtils.isAnnotation(Object.class));
    assertFalse(ClassUtils.isAnnotation(String.class));
    assertFalse(ClassUtils.isAnnotation(Object[].class));
    assertFalse(ClassUtils.isAnnotation(int[][].class));
    assertFalse(ClassUtils.isAnnotation(Class.class));
    assertFalse(ClassUtils.isAnnotation(Thread.State.class));
    assertFalse(ClassUtils.isAnnotation(Cloneable.class));
    assertFalse(ClassUtils.isAnnotation(Integer.TYPE));
    assertTrue(ClassUtils.isAnnotation(Documented.class));
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
    assertFalse(ClassUtils.isArray(null));
    assertFalse(ClassUtils.isArray(Object.class));
    assertTrue(ClassUtils.isArray(Object[].class));
    assertTrue(ClassUtils.isArray(Object[][].class));
    assertTrue(ClassUtils.isArray(String[].class));
    assertTrue(ClassUtils.isArray(int[].class));
    assertFalse(ClassUtils.isArray(Documented.class));
    assertFalse(ClassUtils.isArray(Object.class));
    assertFalse(ClassUtils.isArray(Thread.State.class));
    assertFalse(ClassUtils.isArray(Cloneable.class));
    assertFalse(ClassUtils.isArray(Integer.TYPE));
  }

  @Test
  public void isClass() {
    assertFalse(ClassUtils.isClass(null));
    assertFalse(ClassUtils.isClass(int[].class)); // array
    assertFalse(ClassUtils.isClass(Documented.class)); // annotation
    assertTrue(ClassUtils.isClass(Object.class)); // class!
    assertFalse(ClassUtils.isClass(Thread.State.class)); // enum
    assertFalse(ClassUtils.isClass(Cloneable.class)); // interface
    assertFalse(ClassUtils.isClass(Integer.TYPE)); // primitive
  }

  @Test
  public void isEnum() {
    assertFalse(ClassUtils.isEnum(null));
    assertFalse(ClassUtils.isEnum(int[].class));
    assertFalse(ClassUtils.isEnum(Documented.class));
    assertFalse(ClassUtils.isEnum(Object.class));
    assertTrue(ClassUtils.isEnum(Thread.State.class));
    assertFalse(ClassUtils.isEnum(Runnable.class));
    assertFalse(ClassUtils.isEnum(Integer.TYPE));
  }

  @Test
  public void isInterface() {
    assertFalse(ClassUtils.isInterface(null));
    assertFalse(ClassUtils.isInterface(int[].class));
    assertTrue(ClassUtils.isInterface(Documented.class)); // true, even an enum type!
    assertFalse(ClassUtils.isInterface(Object.class));
    assertFalse(ClassUtils.isInterface(Thread.State.class));
    assertTrue(ClassUtils.isInterface(Runnable.class));
    assertFalse(ClassUtils.isInterface(Integer.TYPE));
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
    assertEquals(Object.class, ClassUtils.loadClass("java.lang.Object"));
  }

  @Test(expected = RuntimeException.class)
  public void loadNonExistingClass() {
    try {
      ClassUtils.loadClass("com.company.non.existing.Class");
    }
    catch (RuntimeException expected) {
      assertEquals("Class (com.company.non.existing.Class) was not found!", expected.getMessage());
      assertTrue(expected.getCause() instanceof ClassNotFoundException);
      throw expected;
    }
  }

  @Test
  public void nonInstanceOf() {
    assertTrue(ClassUtils.notInstanceOf(new Object(), (Class[]) null));
    assertTrue(ClassUtils.notInstanceOf(new Object(), Boolean.class, Number.class, String.class));
    assertFalse(ClassUtils.notInstanceOf(123, Boolean.class, Number.class, String.class));
    assertTrue(ClassUtils.notInstanceOf(123.0, Boolean.class, BigDecimal.class, String.class));
  }

  @SuppressWarnings("unused")
  public static class SuperType {

    @Id
    private String id;

    private String stringValue;

    public SuperType() {
    }

    public SuperType(final Object value) {
      this.stringValue = String.valueOf(value);
    }

    @Id
    public String getId() {
      return id;
    }

    public String getStringValue() {
      return stringValue;
    }

    public Object methodOne(final String value) {
      return value;
    }

    public Object methodOne(final Integer value) {
      return value;
    }

    public void methodTwo(final Boolean conditional, final Number number, final String string) {
    }
  }

  @Resource
  @SuppressWarnings("unused")
  public static class SubType extends SuperType {

    private Character charValue;

    @Id
    private Long id;

    private Object nonAnnotatedField;

    public SubType(final Long id) {
      this.id = id;
    }

    public SubType(final Boolean condition, final Number number, final String string) {
    }

    public Character getCharacterValue() {
      return charValue;
    }

    public void setCharacterValue(final Character charValue) {
      this.charValue = charValue;
    }

    @Deprecated
    public void deprecatedMethod() {
    }

    @Override
    public String methodOne(final String value) {
      return "$"+value;
    }

    public Object methodOne(final Character value) {
      return value;
    }

    public Number methodOne(final Integer wholeNumber, final Double floatingPointNumber) {
      return Math.max(wholeNumber, floatingPointNumber);
    }

    public void methodTwo(final Boolean conditional, final Integer number, final String string) {
    }

    public void methodTwo(final String string, final Number number, final Boolean conditional) {
    }

    public void nonAnnotatedMethod() {
    }
  }

}
