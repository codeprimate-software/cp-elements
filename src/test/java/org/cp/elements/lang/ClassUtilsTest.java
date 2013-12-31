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

import java.lang.annotation.Documented;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

/**
 * The ClassUtilsTest class is a test suite of test cases testing the contract and functionality of the ClassUtils class.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Class
 * @see org.cp.elements.lang.ClassUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ClassUtilsTest {

  @Test
  public void testAssignableTo() {
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
  public void testGetClass() {
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
  public void testGetClassName() {
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
  public void testGetClassSimpleName() {
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
  public void testGetField() throws NoSuchFieldException {
    final Field charValueField = ClassUtils.getField(DerivedClass.class, "charValue");

    assertNotNull(charValueField);
    assertEquals(DerivedClass.class, charValueField.getDeclaringClass());
    assertEquals("charValue", charValueField.getName());
    assertEquals(Character.class, charValueField.getType());
  }

  @Test
  public void testGetFieldFromSuperClass() throws NoSuchFieldException {
    final Field strValueField = ClassUtils.getField(DerivedClass.class, "strValue");

    assertNotNull(strValueField);
    assertEquals(SuperClass.class, strValueField.getDeclaringClass());
    assertEquals("strValue", strValueField.getName());
    assertEquals(String.class, strValueField.getType());
  }

  @Test(expected = NoSuchFieldException.class)
  public void testGetFieldThrowsNoSuchFieldException() throws NoSuchFieldException {
    ClassUtils.getField(SuperClass.class, "charValue");
  }

  @Test
  public void testGetMethod() throws NoSuchMethodException {
    final Method getCharacterValueMethod = ClassUtils.getMethod(DerivedClass.class, "getCharacterValue");

    assertNotNull(getCharacterValueMethod);
    assertEquals(DerivedClass.class, getCharacterValueMethod.getDeclaringClass());
    assertEquals("getCharacterValue", getCharacterValueMethod.getName());
    assertEquals(Character.class, getCharacterValueMethod.getReturnType());
  }

  @Test
  public void testGetMethodFromSuperClass() throws NoSuchMethodException {
    final Method getStringValueMethod = ClassUtils.getMethod(DerivedClass.class, "getStringValue");

    assertNotNull(getStringValueMethod);
    assertEquals(SuperClass.class, getStringValueMethod.getDeclaringClass());
    assertEquals("getStringValue", getStringValueMethod.getName());
    assertEquals(String.class, getStringValueMethod.getReturnType());
  }

  @Test(expected = NoSuchMethodException.class)
  public void testGetMethodThrowsNoSuchMethodException() throws NoSuchMethodException {
    ClassUtils.getMethod(SuperClass.class, "getCharacterValue");
  }

  @Test
  public void testGetName() {
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
  public void testGetSimpleName() {
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
  public void testInstanceOf() {
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
  public void testIsArray() {
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
  public void testIsClass() {
    assertFalse(ClassUtils.isClass(null));
    assertFalse(ClassUtils.isClass(int[].class)); // array
    assertFalse(ClassUtils.isClass(Documented.class)); // annotation
    assertTrue(ClassUtils.isClass(Object.class)); // class!
    assertFalse(ClassUtils.isClass(Thread.State.class)); // enum
    assertFalse(ClassUtils.isClass(Cloneable.class)); // interface
    assertFalse(ClassUtils.isClass(Integer.TYPE)); // primitive
  }

  @Test
  public void testIsEnum() {
    assertFalse(ClassUtils.isEnum(null));
    assertFalse(ClassUtils.isEnum(int[].class));
    assertFalse(ClassUtils.isEnum(Documented.class));
    assertFalse(ClassUtils.isEnum(Object.class));
    assertTrue(ClassUtils.isEnum(Thread.State.class));
    assertFalse(ClassUtils.isEnum(Runnable.class));
    assertFalse(ClassUtils.isEnum(Integer.TYPE));
  }

  @Test
  public void testIsInterface() {
    assertFalse(ClassUtils.isInterface(null));
    assertFalse(ClassUtils.isInterface(int[].class));
    assertTrue(ClassUtils.isInterface(Documented.class)); // true, even an enum type!
    assertFalse(ClassUtils.isInterface(Object.class));
    assertFalse(ClassUtils.isInterface(Thread.State.class));
    assertTrue(ClassUtils.isInterface(Runnable.class));
    assertFalse(ClassUtils.isInterface(Integer.TYPE));
  }

  @Test
  public void testIsPrimitive() {
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
    assertTrue(ClassUtils.isPrimitive(Boolean.TYPE));
    assertTrue(ClassUtils.isPrimitive(Character.TYPE));
    assertTrue(ClassUtils.isPrimitive(Double.TYPE));
    assertTrue(ClassUtils.isPrimitive(Integer.TYPE));
  }

  @Test
  public void testIsPresent() {
    assertTrue(ClassUtils.isPresent("java.lang.Object"));
  }

  @Test
  public void testIsNotPresent() {
    assertFalse(ClassUtils.isPresent("com.company.non.existing.Class"));
  }

  @Test
  public void testLoadClass() {
    assertEquals(Object.class, ClassUtils.loadClass("java.lang.Object"));
  }

  @Test(expected = RuntimeException.class)
  public void testLoadClassForNonExistingClass() {
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
  public void testNonInstanceOf() {
    assertTrue(ClassUtils.notInstanceOf(new Object(), (Class[]) null));
    assertTrue(ClassUtils.notInstanceOf(new Object(), Boolean.class, Number.class, String.class));
    assertFalse(ClassUtils.notInstanceOf(123, Boolean.class, Number.class, String.class));
    assertTrue(ClassUtils.notInstanceOf(123.0, Boolean.class, BigDecimal.class, String.class));
  }

  @SuppressWarnings("unused")
  public static class SuperClass {

    private String strValue;

    public String getStringValue() {
      return strValue;
    }

    public void setStringValue(final String stringValue) {
      this.strValue = stringValue;
    }
  }

  @SuppressWarnings("unused")
  public static class DerivedClass extends SuperClass {

    public Character charValue;

    public Character getCharacterValue() {
      return charValue;
    }

    public void setCharacterValue(final Character charValue) {
      this.charValue = charValue;
    }
  }

}
