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

package org.cp.elements.process.java;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.cp.elements.process.java.EmbeddedJavaProcessExecutor.newEmbeddedJavaProcessExecutor;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Executable;
import org.cp.elements.process.EmbeddedProcessExecutionException;
import org.cp.elements.util.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link EmbeddedJavaProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.process.java.EmbeddedJavaProcessExecutor
 * @since 1.0.0
 */
public class EmbeddedJavaProcessExecutorTests {

  @Test
  public void newEmbeddedJavaProcessExecutorIsNotNull() {
    assertThat(newEmbeddedJavaProcessExecutor()).isNotNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithNonNullNonWorkingDirectoryThrowsIllegalArgumentException() {

    try {
      newEmbeddedJavaProcessExecutor().execute(FileSystemUtils.USER_HOME_DIRECTORY);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("The Java class can only be ran in the same working directory [%1$s]"
        + " as the containing process; directory was [%2$s]", FileSystemUtils.WORKING_DIRECTORY,
          FileSystemUtils.USER_HOME_DIRECTORY);

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithUnresolvableJavaClass() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar",
      "example.of.non.resolvable.ApplicationClass", "argOne", "argTwo"
    };

    try {
      newEmbeddedJavaProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY, commandLine);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("The Java class to execute could not be resolved from the given command-line [%s]",
        Arrays.toString(commandLine));

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void resolveArgumentsFromArgsWhenClassTypeIsPresentReturnsArguments() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", TestApplication.class.getName(),
      "argOne", "argTwo", "argThree"
    };

    String[] arguments = newEmbeddedJavaProcessExecutor().resolveArgumentsFrom(TestApplication.class, commandLine);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(3);
    assertThat(arguments).contains("argOne", "argTwo", "argThree");
  }

  @Test
  public void resolveArgumentsFromArgsWhenClassIsNotPresentReturnsNoArguments() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", "non.existing.ApplicationClass",
      "argOne", "argTwo", "argThree"
    };

    String[] arguments = newEmbeddedJavaProcessExecutor().resolveArgumentsFrom(TestApplication.class, commandLine);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  public void resolveArgumentsFromNoArgsWhenClassTypeIsPresentReturnsNoArguments() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", TestApplication.class.getName()
    };

    String[] arguments = newEmbeddedJavaProcessExecutor().resolveArgumentsFrom(TestApplication.class, commandLine);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  public void resolveArgumentsFromNullArgsReturnsNoArguments() {

    String[] arguments = newEmbeddedJavaProcessExecutor().resolveArgumentsFrom(TestApplication.class, (String[]) null);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  public void resolveJavaClassFromArgumentsReturnsJavaClass() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", TestApplication.class.getName(),
      "argOne", "argTwo", "argThree"
    };

    Class<?> javaClass = newEmbeddedJavaProcessExecutor().resolveJavaClassFrom(commandLine);

    assertThat(javaClass).isEqualTo(TestApplication.class);
  }

  @Test
  public void resolveJavaClassFromArgumentsReturnsNull() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", "non.existing.ApplicationClass",
      "argOne", "argTwo", "argThree"
    };

    Class<?> javaClass = newEmbeddedJavaProcessExecutor().resolveJavaClassFrom(commandLine);

    assertThat(javaClass).isNull();
  }

  @Test
  public void resolveJavaClassFromEmptyArgumentsReturnsNull() {
    assertThat(newEmbeddedJavaProcessExecutor().resolveJavaClassFrom()).isNull();
  }

  @Test
  public void resolveJavaClassFromNullArgumentsReturnsNull() {
    assertThat(newEmbeddedJavaProcessExecutor().resolveJavaClassFrom((String[]) null)).isNull();
  }

  @Test
  public void executeInvokesCallableJavaClass() {

    Optional<String> returnValue = newEmbeddedJavaProcessExecutor().execute(TestCallable.class);

    assertThat(returnValue.orElse(null)).isEqualTo("I was called!");
  }

  @Test(expected = EmbeddedProcessExecutionException.class)
  public void executeInvokesCallableJavaClassHandlesException() {

    try {
      newEmbeddedJavaProcessExecutor().execute(TestCallableThrowsException.class);
    }
    catch (EmbeddedProcessExecutionException expected) {

      assertThat(expected).hasMessage("Failed to call Java class [%s]", TestCallableThrowsException.class.getName());
      assertThat(expected).hasCauseInstanceOf(RuntimeException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void executeInvokesExecutableJavaClass() {

    Optional<String> returnValue = newEmbeddedJavaProcessExecutor().execute(TestExecutable.class);

    assertThat(returnValue.orElse(null)).isEqualTo("I was executed!");
  }

  @Test
  public void executeInvokesRunnableJavaClass() {

    Optional<Object> returnValue = newEmbeddedJavaProcessExecutor().execute(TestRunnable.class);

    assertThat(returnValue.orElse(null)).isNull();
    assertThat(TestRunnable.runCalled.get()).isTrue();
  }

  @Test
  public void executeInvokesRunnableJavaClassWithArguments() {

    Optional<Object> returnValue = newEmbeddedJavaProcessExecutor()
      .execute(TestRunnableWithArguments.class, "argOne", "argTwo");

    assertThat(returnValue.orElse(null)).isNull();
    assertThat(TestRunnableWithArguments.arguments.get()).contains("argOne", "argTwo");
    assertThat(TestRunnableWithArguments.runCalled.get()).isTrue();
  }

  @Test
  public void executeInvokesMainMethodOnJavaClass() {

    Optional<Object> returnValue = newEmbeddedJavaProcessExecutor().execute(TestApplication.class,
      "argOne", "argTwo");

    assertThat(returnValue.orElse(null)).isNull();
    assertThat(TestApplication.arguments.get()).contains("argOne", "argTwo");
  }

  @Test(expected = EmbeddedProcessExecutionException.class)
  public void executeInvokesMainMethodOnJavaClassHandlesException() {

    try {
      newEmbeddedJavaProcessExecutor().execute(TestApplicationThrowsException.class, "argument");
    }
    catch (EmbeddedProcessExecutionException expected) {

      assertThat(expected).hasMessage("Failed to execute Java class [%s] using main method",
        TestApplicationThrowsException.class.getName());

      assertThat(expected).hasCauseInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(RuntimeException.class);
      assertThat(expected.getCause().getCause()).hasMessage("test");
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EmbeddedProcessExecutionException.class)
  public void executeNonConstructableJavaClass() {

    try {
      newEmbeddedJavaProcessExecutor().execute(NonConstructableApplication.class, "argOne");
    }
    catch (EmbeddedProcessExecutionException expected) {

      assertThat(expected).hasMessage(
        "No default constructor or constructor with arguments (%1$s(:Object[]) for type [%2$s] was found",
          NonConstructableApplication.class.getSimpleName(), NonConstructableApplication.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EmbeddedProcessExecutionException.class)
  public void executeNonExecutableJavaClass() {

    try {
      newEmbeddedJavaProcessExecutor().execute(NonExecutableApplication.class, "argOne", "argTwo");
    }
    catch (EmbeddedProcessExecutionException expected) {

      assertThat(expected).hasMessage("Unable to execute Java class [%s];"
        + " Please verify that your class either implements Runnable, Callable, Executable or has a main method",
          NonExecutableApplication.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @SuppressWarnings("unchecked")
  @Test(expected = EmbeddedProcessExecutionException.class)
  public void javaClassExecutorFindsConstructorWithMostArgumentsHandlesException() {

    EmbeddedJavaProcessExecutor.JavaClassExecutor mockJavaClassExecutor =
      mock(EmbeddedJavaProcessExecutor.JavaClassExecutor.class);

    when(mockJavaClassExecutor.constructInstance(any(), any())).thenCallRealMethod();
    when(mockJavaClassExecutor.findConstructor(any(Class.class))).thenCallRealMethod();
    when(mockJavaClassExecutor.isTargetConstructor(any(Constructor.class))).thenCallRealMethod();

    try {
      mockJavaClassExecutor.constructInstance(DestructableType.class, ArrayUtils.asArray("test", 1));
    }
    catch (EmbeddedProcessExecutionException expected) {

      assertThat(expected).hasMessage("Failed to construct an instance of Java class [%s]",
        DestructableType.class.getName());

      assertThat(expected).hasCauseInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause().getCause()).hasMessage("test");
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockJavaClassExecutor, times(1)).constructInstance(eq(DestructableType.class),
        eq(ArrayUtils.asArray("test", 1)));

      verify(mockJavaClassExecutor, times(1)).findConstructor(eq(DestructableType.class));
    }
  }

  @SuppressWarnings("unused")
  public static class DestructableType {

    public DestructableType(Object[] arguments) {
      throw newIllegalArgumentException("test");
    }

    public DestructableType() {
      throw newIllegalArgumentException("default");
    }
  }

  private static class NonConstructableApplication implements Runnable {

    @SuppressWarnings("unused")
    private NonConstructableApplication(Object arg) {
    }

    @Override
    public void run() {
    }
  }

  private static class NonExecutableApplication {
  }

  public static class TestApplication {

    static final AtomicReference<String[]> arguments = new AtomicReference<>(null);

    public static void main(String[] args) {
      arguments.set(args);
    }
  }

  public static class TestApplicationThrowsException {

    public static void main(String[] args) {
      throw newRuntimeException("test");
    }
  }

  public static class TestCallable implements Callable<String> {

    @Override
    public String call() throws Exception {
      return "I was called!";
    }
  }

  public static class TestCallableThrowsException implements Callable<Object> {

    @Override
    public Object call() throws Exception {
      throw newRuntimeException("test");
    }
  }

  public static class TestExecutable implements Executable<String> {

    @Override
    public boolean isRunning() {
      return false;
    }

    @Override
    public String execute(Object... args) {
      return "I was executed!";
    }
  }

  public static class TestRunnable implements Runnable {

    static final AtomicBoolean runCalled = new AtomicBoolean(false);

    @Override
    public void run() {
      runCalled.set(true);
    }
  }

  public static class TestRunnableWithArguments implements Runnable {

    static final AtomicBoolean runCalled = new AtomicBoolean(false);
    static final AtomicReference<String[]> arguments = new AtomicReference<>(null);

    public TestRunnableWithArguments(String[] args) {
      arguments.set(args);
    }

    @Override
    public void run() {
      run(arguments.get());
    }

    @SuppressWarnings("unused")
    void run(String... args) {
      runCalled.set(true);
    }
  }
}
