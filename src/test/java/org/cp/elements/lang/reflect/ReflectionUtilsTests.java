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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.annotation.Id;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link ReflectionUtils}.
 *
 * @author John J. Blum
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see org.junit.Test
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class ReflectionUtilsTests extends AbstractBaseTestSuite {

  private static final AtomicReference<String> METHOD_NAME = new AtomicReference<>();

  private static final List<Line> reflectionUtilsLines = new ArrayList<>();

  @BeforeClass
  public static void setupBeforeTests() {

    try (LineNumberReader reader = new LineNumberReader(new InputStreamReader(newInputStream(ReflectionUtils.class)))) {
      String line;
      while ((line = reader.readLine()) != null) {
        reflectionUtilsLines.add(Line.with(reader.getLineNumber(), line));
      }
    }
    catch (IOException ignore) { }
  }

  private static InputStream newInputStream(Class<?> type) throws FileNotFoundException {

    String sourceFilename = type.getSimpleName().concat(FileSystemUtils.JAVA_FILE_EXTENSION);

    File sourceFile = FileSystemUtils.search(sourceFilename);

    //System.err.printf("File [%s]%n", sourceFile);

    return new FileInputStream(sourceFile);
  }

  @SuppressWarnings("unused")
  private static void printReflectionUtilsClassSource() {

    System.err.println("*****");

    reflectionUtilsLines.stream()
      .map(Line::toString)
      .forEach(System.out::println);

    System.err.println("*****");
  }

  @Before
  public void setup() {
    setLogLevel(Level.INFO);
  }

  @After
  public void tearDown() {
    METHOD_NAME.set(null);
    SuperType.stringField = null;
  }

  @SuppressWarnings("unused")
  private int getLineNumberForLine(String line) {

    //printReflectionUtilsClassSource();

    return reflectionUtilsLines.stream()
      .filter(reflectionUtilsLine -> reflectionUtilsLine.getContent().contains(line))
      .findFirst()
      .map(Line::getNumber)
      .orElse(0);
  }

  @Test
  public void getArgumentTypesWithArguments() {
    TestUtils.assertEquals(new Class[] { Boolean.class, Character.class, Integer.class, Double.class, String.class },
      ReflectionUtils.getArgumentTypes(true, 'c', 123, Math.PI, "test"));
  }

  @Test
  public void getArgumentTypesWithArgumentsContainingNulls() {
    TestUtils.assertEquals(new Class[] { Boolean.class, null, Integer.class, Double.class, null },
      ReflectionUtils.getArgumentTypes(false, null, 1, 3.14159, null));
  }

  @Test
  public void getArgumentTypesWithEmptyArguments() {
    TestUtils.assertEquals(new Class[0], ReflectionUtils.getArgumentTypes());
  }

  @Test
  public void getArgumentTypesWithNullArguments() {
    assertThat(ReflectionUtils.getArgumentTypes((Object[]) null)).isEmpty();
  }

  @Test
  public void getValueOfClassField() {

    assertThat(ReflectionUtils.getValue(SuperType.class, "serialVersionUID", Long.class).longValue()).isEqualTo(42L);
    assertThat(ReflectionUtils.getValue(DerivedType.class, "serialVersionUID", Long.class).longValue()).isEqualTo(42L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getValueOfNonExistingClassField() {

    try {
      ReflectionUtils.getValue(SuperType.class, "DEFAULT_ID", Long.class);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Field with name [DEFAULT_ID] does not exist on class type [%s]",
        SuperType.class.getName());

      assertThat(expected).hasCauseInstanceOf(FieldNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchFieldException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getValueOfObjectField() {
    assertThat(ReflectionUtils.getValue(new DerivedType(), "id", Long.class).longValue()).isEqualTo(1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getValueOfNonExistingObjectField() {

    try {
      ReflectionUtils.getValue(new DerivedType(), "name", String.class);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Field with name [name] does not exist on object of type [%s]",
        DerivedType.class.getName());

      assertThat(expected).hasCauseInstanceOf(FieldNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchFieldException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = FieldAccessException.class)
  public void getValueThrowsFieldAccessException() {

    try {
      ReflectionUtils.getValue(new Object(), ReflectionUtils.getField(DerivedType.class, "id"), Object.class);
    }
    catch (FieldAccessException expected) {

      assertThat(expected).hasMessage("Failed to get value of field [id] from object of type [%s]",
        Object.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);

      throw expected;
    }
  }

  @Test
  public void getValueWithNullFieldReferenceThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ReflectionUtils.getValue(new Object(), (Field) null, Object.class))
      .withMessage("Field is required")
      .withNoCause();
  }

  @Test
  public void getValueWithNullFieldTypeThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ReflectionUtils.getValue(new DerivedType(), "id", null))
      .withMessage("Field type is required")
      .withNoCause();
  }

  @Test
  public void setClassField() {

    assertThat(SuperType.stringField).isNull();

    ReflectionUtils.setField(SuperType.class, "stringField", "test");

    assertThat(SuperType.stringField).isEqualTo("test");
  }

  @Test(expected = FieldAccessException.class)
  public void setFinalClassField() {

    try {
      assertThat(SuperType.serialVersionUID).isEqualTo(42L);
      ReflectionUtils.setField(SuperType.class, "serialVersionUID", 24L);
    }
    catch (FieldAccessException expected) {

      assertThat(expected)
        .hasMessage("Cannot set the value of a final field [serialVersionUID] on class type [%s]",
          SuperType.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(SuperType.serialVersionUID).isEqualTo(42L);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setNonExistingClassField() {

    try {
      ReflectionUtils.setField(SuperType.class, "nonExistingField", "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Field with name [nonExistingField] does not exist on class type [%s]",
        SuperType.class.getName());

      assertThat(expected).hasCauseInstanceOf(FieldNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchFieldException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setObjectField() {

    DerivedType derivedType = new DerivedType();

    assertThat(derivedType.booleanField).isNull();

    ReflectionUtils.setField(derivedType, "booleanField", true);

    assertThat(derivedType.booleanField).isTrue();
  }

  @Test(expected = FieldAccessException.class)
  public void setFinalObjectField() {

    DerivedType derivedType = new DerivedType();

    try {
      assertThat(derivedType.magicNumber.intValue()).isEqualTo(0xCAFEBABE);

      ReflectionUtils.setField(derivedType, "magicNumber", 0x0);
    }
    catch (FieldAccessException expected) {

      assertThat(expected)
        .hasMessage("Cannot set the value of a final field [magicNumber] on object of type [%s]",
          derivedType.getClass().getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(derivedType.magicNumber.intValue()).isEqualTo(0xCAFEBABE);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setNonExistingObjectField() {

    try {
      ReflectionUtils.setField(new DerivedType(), "nonExistingField", "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Field with name [nonExistingField] does not exist on object of type [%s]",
        DerivedType.class.getName());

      assertThat(expected).hasCauseInstanceOf(FieldNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchFieldException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = FieldAccessException.class)
  public void setObjectFieldWithIllegalValue() {

    DerivedType derivedType = new DerivedType();

    try {
      assertThat(derivedType.booleanField).isNull();

      ReflectionUtils.setField(derivedType, "booleanField", 1);
    }
    catch (FieldAccessException expected) {

      assertThat(expected).hasMessage("Failed to set field [booleanField] to value [1] on object of type [%s]",
        derivedType.getClass().getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(derivedType.booleanField).isNull();
    }
  }

  @Test
  public void setFieldWithNullFieldReferenceThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ReflectionUtils.setField(new Object(), (Field) null, "test"))
      .withMessage("Field is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void nullCastToVoid() {
    assertThat(Void.class.cast(null)).isNull();
  }

  @Test
  public void invokeClassMethod() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(SuperType.class, "methodOne");

    assertThat(METHOD_NAME.get()).isEqualTo("methodOne");
  }

  @Test
  public void invokeClassMethodWithArguments() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(SuperType.class, "methodTwo", "test");

    assertThat(METHOD_NAME.get()).isEqualTo("methodTwo(test)");
  }

  @Test
  public void invokeClassMethodWithReturnValue() {

    assertThat(METHOD_NAME.get()).isNull();

    String returnValue = ReflectionUtils.invoke(SuperType.class, "methodThree", String.class);

    assertThat(METHOD_NAME.get()).isEqualTo("methodThree():test");
    assertThat(returnValue).isEqualTo("test");
  }

  @Test
  public void invokeClassMethodWithArgumentsAndReturnValue() {

    assertThat(METHOD_NAME.get()).isNull();

    String greeting = ReflectionUtils.invoke(SuperType.class, "methodFour",
      ArrayUtils.asArray("John"), String.class);

    assertThat(greeting).isEqualTo("Hello John");
    assertThat(METHOD_NAME.get()).isEqualTo("methodFour");
  }

  @Test
  public void invokeClassMethodWithArgumentsAndArgumentTypes() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(SuperType.class, "methodFour", ArrayUtils.asArray(Integer.TYPE), 1);

    assertThat(METHOD_NAME.get()).isEqualTo("methodFour(1)");
  }

  @Test
  public void invokeClassMethodWithArgumentsArgumentTypesAndReturnValue() {

    assertThat(METHOD_NAME.get()).isNull();

    Integer sum = ReflectionUtils.invoke(SuperType.class, "methodFour",
      ArrayUtils.<Class<?>>asArray(Integer.TYPE, Integer.TYPE), ArrayUtils.asArray(5, 7), Integer.class);

    assertThat(NumberUtils.valueOf(sum)).isEqualTo(12);
    assertThat(METHOD_NAME.get()).isEqualTo("methodFour");
  }

  @Test(expected = IllegalArgumentException.class)
  public void invokeNoSuchClassMethod() {

    try {
      assertThat(METHOD_NAME.get()).isNull();

      ReflectionUtils.invoke(SuperType.class, "methodSix", "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected)
        .hasMessage("No method with signature [methodSix(:String):void] exists on class type [%s]",
          SuperType.class.getName());

      assertThat(expected).hasCauseInstanceOf(MethodNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchMethodException.class);

      throw expected;
    }
    finally {
      assertThat(METHOD_NAME.get()).isNull();
    }
  }

  @Test
  public void invokeObjectMethod() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(new DerivedType(), "methodFive");

    assertThat(METHOD_NAME.get()).isEqualTo("methodFive");
  }

  @Test
  public void invokeObjectMethodWithArguments() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(new DerivedType(), "methodSix", "string");

    assertThat(METHOD_NAME.get()).isEqualTo("methodSix(string)");
  }

  @Test
  public void invokeObjectMethodWithReturnValue() {

    assertThat(METHOD_NAME.get()).isNull();

    String returnValue = ReflectionUtils.invoke(new DerivedType(), "methodSeven", String.class);

    assertThat(returnValue).isEqualTo("string");
    assertThat(METHOD_NAME.get()).isEqualTo("methodSeven():string");
  }

  @Test
  public void invokeObjectWithArgumentsAndArgumentTypes() {

    assertThat(METHOD_NAME.get()).isNull();

    ReflectionUtils.invoke(new DerivedType(), "methodEight",
      ArrayUtils.asArray(Integer.TYPE), 1);

    assertThat(METHOD_NAME.get()).isEqualTo("methodEight(1)");
  }

  @Test
  public void invokeObjectMethodWithArgumentsAndReturnValue() {

    assertThat(METHOD_NAME.get()).isNull();

    String returnValue = ReflectionUtils.invoke(new DerivedType(), "methodEight",
      ArrayUtils.asArray("test", "ing"), String.class);

    assertThat(returnValue).isEqualTo("testing");
    assertThat(METHOD_NAME.get()).isEqualTo("methodEight");
  }

  @Test(expected = IllegalArgumentException.class)
  public void invokeNoSuchObjectMethod() {

    try {
      assertThat(METHOD_NAME.get()).isNull();

      ReflectionUtils.invoke(new DerivedType(), "methodEight", ArrayUtils.asArray('A', 'C'), String.class);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected)
        .hasMessage("No method with signature [methodEight(:Character, :Character):String] exists on object of type [%s]",
          DerivedType.class.getName());

      assertThat(expected).hasCauseInstanceOf(MethodNotFoundException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(METHOD_NAME.get()).isNull();
    }
  }

  @Test(expected = MethodInvocationException.class)
  public void invokeMethodWithWrongNumberOfArguments() {

    try {
      assertThat(METHOD_NAME.get()).isNull();

      ReflectionUtils.invoke(SuperType.class, "methodFour", ArrayUtils.<Class<?>>asArray(String.class),
        ArrayUtils.asArray("Jon", "Doe"), String.class);
    }
    catch (MethodInvocationException expected) {

      assertThat(expected)
        .hasMessage("Failed to invoke method [methodFour(:String):String] on class type [%s]",
          SuperType.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasMessageStartingWith("wrong number of arguments");

      throw expected;
    }
    finally {
      assertThat(METHOD_NAME.get()).isNull();
    }
  }

  @Test(expected = MethodInvocationException.class)
  public void invokeMethodWithArgumentParameterTypeMismatch() {

    try {
      assertThat(METHOD_NAME.get()).isNull();

      ReflectionUtils.invoke(SuperType.class, "methodFour",
        ArrayUtils.<Class<?>>asArray(Integer.TYPE, Integer.TYPE), ArrayUtils.asArray(5.4d, 4.5d), Integer.TYPE);
    }
    catch (MethodInvocationException expected) {

      assertThat(expected)
        .hasMessage("Failed to invoke method [methodFour(:int, :int):int] on class type [%s]",
          SuperType.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasMessage("argument type mismatch");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(METHOD_NAME.get()).isNull();
    }
  }

  @Test(expected = MethodInvocationException.class)
  public void invokeMethodThrowsMethodInvocationException() {

    try {
      assertThat(METHOD_NAME.get()).isNull();
      ReflectionUtils.invoke(new DerivedType(), "methodNine",
        ArrayUtils.asArray("test"), Object.class);
    }
    catch (MethodInvocationException expected) {

      assertThat(expected)
        .hasMessage("Failed to invoke method [methodNine(:String):Object] on object of type [%s]",
          DerivedType.class.getName());

      assertThat(expected).hasCauseInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause().getCause()).hasMessage("test");

      throw expected;
    }
    finally {
      assertThat(METHOD_NAME.get()).isEqualTo("methodNine");
    }
  }

  @Test
  public void invokeMethodWithNullMethodReferenceThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ReflectionUtils.invoke(new Object(), (Method) null, ArrayUtils.emptyArray(), Void.class))
      .withMessage("Method is required")
      .withNoCause();
  }

  @Test
  public void invokeMethodWithNullMethodReturnTypeThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ReflectionUtils.invoke(new DerivedType(), "methodFive", (Class<?>) null))
      .withMessage("Return type is required")
      .withNoCause();
  }

  @Test
  public void withFieldsOnClass() {

    Set<String> fieldNames = new HashSet<>(2);

    ReflectionUtils.withFields().on(SuperType.class).call((field) -> fieldNames.add(field.getName()))
      .throwing(new FieldNotFoundException());

    assertThat(fieldNames.size()).isGreaterThanOrEqualTo(2);
    assertThat(fieldNames.containsAll(Arrays.asList("serialVersionUID", "stringField"))).isTrue();
  }

  @Test
  public void withFieldsOnObject() {

    Set<String> fieldNames = new HashSet<>(5);

    ReflectionUtils.withFields().on(new DerivedType()).call((field) -> fieldNames.add(field.getName()))
      .throwing(new FieldNotFoundException());

    assertThat(fieldNames.size()).isGreaterThanOrEqualTo(5);
    assertThat(fieldNames.containsAll(Arrays.asList("serialVersionUID", "stringField",
      "booleanField", "magicNumber", "id"))).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void withPublicInstanceFieldsOnly() {

    Set<String> fieldNames = new HashSet<>(2);

    ReflectionUtils.withFields().on(DerivedType.class).matching((field) -> Modifier.isPublic(field.getModifiers()))
      .call((field) -> fieldNames.add(field.getName())).throwing(new FieldNotFoundException());

    assertThat(fieldNames).isNotNull();
    assertThat(fieldNames.isEmpty()).isFalse();
    assertThat(fieldNames.size()).isEqualTo(1);
    assertThat(fieldNames.containsAll(Collections.singletonList("magicNumber"))).isTrue();
  }

  @Test(expected = FieldNotFoundException.class)
  public void withPublicProtectedNonFinalInstanceFields() {

    AtomicInteger count = new AtomicInteger(0);

    try {
      ReflectionUtils.withFields().on(new DerivedType()).matching((field) -> {
          int fieldModifiers = field.getModifiers();
          return ((Modifier.isPublic(fieldModifiers) || Modifier.isProtected(fieldModifiers))
            && !Modifier.isFinal(fieldModifiers) && !Modifier.isStatic(fieldModifiers));
        }).call((field) -> {
          logDebug(field.getName());
          count.incrementAndGet();
      }).throwing(new FieldNotFoundException());
    }
    finally {
      assertThat(count.get()).isEqualTo(0);
    }
  }

  @Test
  @SuppressWarnings("all")
  public void withNullFields() {
    ReflectionUtils.withFields(null, null, null).call(field -> new NullPointerException("The Field must not be null!"));
  }

  @Test
  public void withMethodsOnClass() {

    Set<String> methods = new HashSet<>(5);

    ReflectionUtils.withMethods().on(SuperType.class).matching((method) -> SuperType.class.equals(method.getDeclaringClass()))
      .call((method) -> methods.add(String.format("%1$s(%2$d)", method.getName(), method.getParameterTypes().length)))
        .throwing(new MethodNotFoundException());

    assertThat(methods.size()).isGreaterThanOrEqualTo(5);
    assertThat(methods.containsAll(Arrays.asList("methodOne(0)", "methodTwo(1)", "methodThree(0)",
      "methodFour(1)", "methodFour(2)"))).isTrue();
  }

  @Test
  public void withMethodsOnObject() {

    Set<String> methods = new HashSet<>(10);

    ReflectionUtils.withMethods().on(new DerivedType()).matching((method) ->
      DerivedType.class.equals(method.getDeclaringClass()) && method.getName().startsWith("method"))
      .call((method) -> {
        logDebug(method.getName());
        methods.add(String.format("%1$s(%2$d)", method.getName(), method.getParameterTypes().length));
      }).throwing(new MethodNotFoundException());

    assertThat(methods.size()).isGreaterThanOrEqualTo(5);
    assertThat(methods.containsAll(Arrays.asList("methodFive(0)", "methodSix(1)", "methodSeven(0)", "methodEight(2)",
      "methodNine(1)"))).isTrue();
  }

  @Test
  public void withMethodTenOnObject() {

    AtomicInteger count = new AtomicInteger(0);

    ReflectionUtils.withMethods().on(new DerivedType()).matching((method) ->  "methodTen".equals(method.getName()))
      .call((method) -> count.incrementAndGet());

    assertThat(count.get()).isEqualTo(0);
  }

  @Test(expected = MethodNotFoundException.class)
  public void withNonMatchingMethodOnObjectThrowsMethodNotFoundException() {

    AtomicInteger count = new AtomicInteger(0);

    try {
      ReflectionUtils.withMethods().on(new DerivedType()).matching((method) ->  false)
        .call((method) ->  count.incrementAndGet()).throwing(new MethodNotFoundException());
    }
    finally {
      assertThat(count.get()).isEqualTo(0);
    }
  }

  @Test
  @SuppressWarnings("all")
  public void withNullMethods() {
    ReflectionUtils.withMethods(null, null, null).call((method) -> new NullPointerException("Method must not be null"));
  }

  // TODO: Write more tests!

  @SuppressWarnings("all")
  protected static class SuperType {

    private static final long serialVersionUID = 42L;

    protected static String stringField;

    public static void methodOne() {
      METHOD_NAME.compareAndSet(null, "methodOne");
    }

    public static void methodTwo(Object arg) {
      METHOD_NAME.compareAndSet(null, String.format("methodTwo(%1$s)", arg));
    }

    public static Object methodThree() {
      METHOD_NAME.compareAndSet(null, "methodThree():test");
      return "test";
    }

    public static void methodFour(int value) {
      METHOD_NAME.compareAndSet(null, String.format("methodFour(%s)", value));
    }

    public static String methodFour(String name) {
      METHOD_NAME.compareAndSet(null, "methodFour");
      return String.format("Hello %1$s", name);
    }

    public static int methodFour(int valueOne, int valueTwo) {
      METHOD_NAME.compareAndSet(null, "methodFour");
      return (valueOne + valueTwo);
    }
  }

  @SuppressWarnings("all")
  protected static class DerivedType extends SuperType {

    private Boolean booleanField;

    public final Integer magicNumber = 0xCAFEBABE;

    @Id
    private Long id = 1L;

    public void methodFive() {
      METHOD_NAME.compareAndSet(null, "methodFive");
    }

    public void methodSix(Object arg) {
      METHOD_NAME.compareAndSet(null, String.format("methodSix(%1$s)", arg));
    }

    public Object methodSeven() {
      METHOD_NAME.compareAndSet(null, "methodSeven():string");
      return "string";
    }

    public void methodEight(int value) {
      METHOD_NAME.compareAndSet(null, String.format("methodEight(%s)", value));
    }

    public String methodEight(String left, String right) {
      METHOD_NAME.compareAndSet(null, "methodEight");
      return (left + right);
    }

    public Object methodNine(String message) {
      METHOD_NAME.compareAndSet(null, "methodNine");
      throw new IllegalArgumentException(message);
    }
  }

  @Getter
  @EqualsAndHashCode
  @RequiredArgsConstructor(staticName = "with")
  static class Line {

    private final int number;

    @lombok.NonNull
    private final String content;

    @Override
    public String toString() {
      return String.format("%d: %s", getNumber(), getContent());
    }
  }
}
