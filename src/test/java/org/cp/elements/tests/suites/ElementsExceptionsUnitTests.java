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
package org.cp.elements.tests.suites;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileFilter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import org.assertj.core.api.InstanceOfAssertFactories;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.RunnableUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.reflect.ModifierUtils;
import org.cp.elements.test.AbstractTestSuite;

/**
 * Test Suite of Unit Tests for all Elements {@link Exception Exceptions}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @since 3.0.0
 */
class ElementsExceptionsUnitTests extends AbstractTestSuite {

  static final String ORG_CP_ELEMENTS_PACKAGE_NAME = String.join(File.separator, "org", "cp", "elements");

  static final Set<String> EXCEPTION_CLASS_NAME_EXCLUDES = Set.of("ChainedPropertyVetoException");

  static final Set<Class<? extends Exception>> EXCEPTION_CLASS_INCLUDES =
    Set.of(RunnableUtils.SleepDeprivedException.class);

  static final Exception CAUSE = new RuntimeException("CAUSE");

  static Stream<Class<? extends Exception>> elementsExceptions() {

    File sourceDirectory = new ElementsExceptionsUnitTests().getSourceDirectory();
    File mainJavaSourceDirectory = new File(sourceDirectory, String.join(File.separator, "main", "java"));

    Set<String> elementsExceptionClassNames = findExceptionClassNames(mainJavaSourceDirectory);

    Set<Class<? extends Exception>> exceptionClasses = elementsExceptionClassNames.stream()
      .map(ElementsExceptionsUnitTests::toClass)
      .filter(type -> !ModifierUtils.isAbstract(type))
      .filter(type -> EXCEPTION_CLASS_NAME_EXCLUDES.stream()
        .noneMatch(typeName -> type.getName().contains(typeName)))
      .collect(Collectors.toSet());

    exceptionClasses.addAll(EXCEPTION_CLASS_INCLUDES);

    return exceptionClasses.stream();
  }

  private static Set<String> findExceptionClassNames(File directory) {

    assertThat(directory)
      .describedAs("Expected directory [%s] to exist", directory)
      .isDirectory();

    Set<String> exceptionClassNames = new HashSet<>();

    for (File file : nullSafeFiles(directory.listFiles(ExceptionFileFilter.INSTANCE))) {
      if (ExceptionFileFilter.INSTANCE.isDirectory(file)) {
        exceptionClassNames.addAll(findExceptionClassNames(file));
      }
      else {
        exceptionClassNames.add(toExceptionClassName(file));
      }
    }

    return exceptionClassNames;
  }


  private static File[] nullSafeFiles(File[] array) {
    return array != null ? array : FileSystemUtils.NO_FILES;
  }

  private static Class<Exception> toClass(String className) {
    return ClassUtils.loadClass(className);
  }

  @SuppressWarnings("all")
  private static String toExceptionClassName(File file) {

    String pathname = file.getAbsolutePath();

    int index = pathname.indexOf(ORG_CP_ELEMENTS_PACKAGE_NAME);

    String classResourceName = pathname.substring(index);
    String className = classResourceName.replaceAll("[\\/]", StringUtils.DOT_SEPARATOR);

    className = className.endsWith(".java")
      ? className.substring(0, className.lastIndexOf(StringUtils.DOT_SEPARATOR))
      : className;

    return className;
  }

  @ParameterizedTest
  @MethodSource("elementsExceptions")
  void constructExceptionUsingDefaultConstructor(Class<Exception> exceptionClass) throws Exception {

    Constructor<Exception> exceptionDefaultConstructor = exceptionClass.getConstructor();

    Exception exception = exceptionDefaultConstructor.newInstance();

    assertThat(exception).isNotNull();
    assertThat(exception.getCause()).isNull();
    assertThat(exception.getMessage()).isNull();
  }

  @ParameterizedTest
  @MethodSource("elementsExceptions")
  void constructExceptionWithMessage(Class<Exception> exceptionClass) throws Exception {

    Constructor<Exception> exceptionConstructorWithMessage = exceptionClass.getConstructor(String.class);

    Exception exception = exceptionConstructorWithMessage.newInstance("TEST MESSAGE");

    assertThat(exception).isNotNull();
    assertThat(exception.getCause()).isNull();
    assertThat(exception.getMessage()).isEqualTo("TEST MESSAGE");
  }

  @ParameterizedTest
  @MethodSource("elementsExceptions")
  void constructExceptionWithCause(Class<Exception> exceptionClass) throws Exception {

    Constructor<Exception> exceptionConstructorWithMessage = exceptionClass.getConstructor(Throwable.class);

    Exception exception = exceptionConstructorWithMessage.newInstance(CAUSE);

    assertThat(exception).isNotNull();
    assertThat(exception.getCause()).isEqualTo(CAUSE);
    assertThat(exception.getMessage()).containsSequence("CAUSE");
  }

  @ParameterizedTest
  @MethodSource("elementsExceptions")
  void constructExceptionWithMessageAndCause(Class<Exception> exceptionClass) throws Exception {

    Constructor<Exception> exceptionConstructorWithMessage =
      exceptionClass.getConstructor(String.class, Throwable.class);

    Exception exception = exceptionConstructorWithMessage.newInstance("MOCK MESSAGE", CAUSE);

    assertThat(exception).isNotNull();
    assertThat(exception.getCause()).isEqualTo(CAUSE);
    assertThat(exception.getMessage()).isEqualTo("MOCK MESSAGE");
  }

  @ParameterizedTest
  @MethodSource("elementsExceptions")
  void constructExceptionWithBecauseFactoryMethod(Class<Exception> exceptionClass) throws Exception {

    Method because = ObjectUtils.findMethod(exceptionClass, "because", CAUSE);

    if (because != null) {

      Object exceptionInstance = because.invoke(null, CAUSE);

      assertThat(exceptionInstance).isNotNull()
        .isInstanceOf(exceptionClass)
        .asInstanceOf(InstanceOfAssertFactories.type(Exception.class))
        .extracting(Exception::getCause)
        .isEqualTo(CAUSE);
    }
  }

  static final class ExceptionFileFilter implements FileFilter {

    static final ExceptionFileFilter INSTANCE = new ExceptionFileFilter();

    @Override
    @SuppressWarnings("all")
    public boolean accept(File pathname) {
      boolean isDirectoryOrException = isDirectory(pathname) || isException(pathname);
      return isDirectoryOrException;
    }

    boolean isDirectory(File path) {
      return path != null && path.isDirectory();
    }

    boolean isException(File path) {
      return path != null && path.isFile() && path.getName().endsWith("Exception.java");
    }
  }
}
