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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

import org.cp.elements.test.CheckedTestException;

import org.mockito.InOrder;

/**
 * Unit Tests for {@link ThrowableOperation}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ThrowableOperation
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class ThrowableOperationUnitTests {

  @Test
  public void fromCallable() throws Throwable {

    Callable<Object> mockCallable = mock(Callable.class);

    doReturn("test").when(mockCallable).call();

    ThrowableOperation<Object> operation = ThrowableOperation.fromCallable(mockCallable);

    assertThat(operation).isNotNull();
    assertThat(operation.run("mock")).isEqualTo("test");

    verify(mockCallable, times(1)).call();
    verifyNoMoreInteractions(mockCallable);
  }

  @Test
  public void fromCallableThrowingException() throws Throwable {

    Callable<Object> mockCallable = mock(Callable.class);

    doThrow(new CheckedTestException("TEST")).when(mockCallable).call();

    ThrowableOperation<Object> operation = ThrowableOperation.fromCallable(mockCallable);

    assertThat(operation).isNotNull();

    assertThatExceptionOfType(CheckedTestException.class)
      .isThrownBy(operation::run)
      .withMessage("TEST")
      .withNoCause();

    verify(mockCallable, times(1)).call();
    verifyNoMoreInteractions(mockCallable);
  }

  @Test
  @SuppressWarnings("all")
  public void fromNullCallable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableOperation.fromCallable((Callable<?>) null))
      .withMessage("Callable is required")
      .withNoCause();
  }

  @Test
  public void fromConsumer() throws Throwable {

    Consumer<Object> mockConsumer = mock(Consumer.class);

    ThrowableOperation<Object> operation = ThrowableOperation.fromConsumer(mockConsumer);

    assertThat(operation).isNotNull();
    assertThat(operation.run("mock", "test")).isNull();

    verify(mockConsumer, times(1)).accept(eq(new Object[] { "mock", "test" }));
    verifyNoMoreInteractions(mockConsumer);
  }

  @Test
  @SuppressWarnings("all")
  public void fromNullConsumer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableOperation.fromConsumer((Consumer<Object>) null))
      .withMessage("Consumer is required")
      .withNoCause();
  }

  @Test
  public void fromFunction() throws Throwable {

    Function<Object, Object> mockFunction = mock(Function.class);

    doReturn("test").when(mockFunction).apply(any());

    ThrowableOperation<Object> operation = ThrowableOperation.fromFunction(mockFunction);

    assertThat(operation).isNotNull();
    assertThat(operation.run("mockOne", "mockTwo")).isEqualTo("test");

    verify(mockFunction, times(1)).apply(eq(new Object[] { "mockOne", "mockTwo" }));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test
  public void fromNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableOperation.fromFunction(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  public void fromRunnable() throws Throwable {

    Runnable mockRunnable = mock(Runnable.class);

    ThrowableOperation<Object> operation = ThrowableOperation.fromRunnable(mockRunnable);

    assertThat(operation).isNotNull();
    assertThat(operation.run("mock")).isNull();

    verify(mockRunnable, times(1)).run();
    verifyNoMoreInteractions(mockRunnable);
  }

  @Test
  @SuppressWarnings("all")
  public void fromNullRunnable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableOperation.fromRunnable((Runnable) null))
      .withMessage("Runnable is required")
      .withNoCause();
  }

  @Test
  public void fromSupplier() throws Throwable {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    doReturn("test").when(mockSupplier).get();

    ThrowableOperation<Object> operation = ThrowableOperation.fromSupplier(mockSupplier);

    assertThat(operation).isNotNull();
    assertThat(operation.run("mock")).isEqualTo("test");

    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(mockSupplier);
  }

  @Test
  @SuppressWarnings("all")
  public void fromNullSupplier() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableOperation.fromSupplier((Supplier<?>) null))
      .withMessage("Supplier is required")
      .withNoCause();
  }

  @Test
  public void asCallable() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doReturn("test").when(operation).safeRun(any());
    doCallRealMethod().when(operation).asCallable();

    Callable<Object> callable = operation.asCallable();

    assertThat(callable).isNotNull();
    assertThat(callable.call()).isEqualTo("test");

    verify(operation, times(1)).asCallable();
    verify(operation, times(1)).safeRun();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asCallableThrowingException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(new CheckedTestException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());
    doCallRealMethod().when(operation).asCallable();

    Callable<Object> callable = operation.asCallable();

    assertThat(callable).isNotNull();

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(callable::call)
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(CheckedTestException.class);

    verify(operation, times(1)).asCallable();
    verify(operation, times(1)).safeRun();
    verify(operation, times(1)).run();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asConsumer() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doCallRealMethod().when(operation).asConsumer();

    //Object arguments = new Object[] { "mock", "test" };
    Object arguments = "test";

    Consumer<Object> consumer = operation.asConsumer();

    assertThat(consumer).isNotNull();

    consumer.accept(arguments);

    verify(operation, times(1)).asConsumer();
    verify(operation, times(1)).safeRun(eq(arguments));
    //verify(operation, times(1)).safeRun(eq("mock"), eq("test"));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asConsumerThrowingException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(new CheckedTestException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());
    doCallRealMethod().when(operation).asConsumer();

    Object arguments = "mock";

    Consumer<Object> consumer = operation.asConsumer();

    assertThat(consumer).isNotNull();

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(() -> consumer.accept(arguments))
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(CheckedTestException.class);

    verify(operation, times(1)).asConsumer();
    verify(operation, times(1)).safeRun(eq(arguments));
    verify(operation, times(1)).run(eq(arguments));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asFunction() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doReturn("test").when(operation).safeRun(any());
    doCallRealMethod().when(operation).asFunction();

    //Object arguments = new Object[] { "mockOne", "mockTwo" };
    Object arguments = "rogueOne";

    Function<Object, Object> function = operation.asFunction();

    assertThat(function).isNotNull();
    assertThat(function.apply(arguments)).isEqualTo("test");

    verify(operation, times(1)).asFunction();
    verify(operation, times(1)).safeRun(eq(arguments));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asFunctionThrowingException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(new CheckedTestException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());
    doCallRealMethod().when(operation).asFunction();

    //Object arguments = new Object[] { "mockOne", "mockTwo" };
    Object arguments = "rogueOne";

    Function<Object, Object> function = operation.asFunction();

    assertThat(function).isNotNull();

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(() -> function.apply(arguments))
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(CheckedTestException.class);


    verify(operation, times(1)).asFunction();
    verify(operation, times(1)).run(eq(arguments));
    verify(operation, times(1)).safeRun(eq(arguments));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asRunnable() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doCallRealMethod().when(operation).asRunnable();

    Runnable runnable = operation.asRunnable();

    assertThat(runnable).isNotNull();

    runnable.run();

    verify(operation, times(1)).asRunnable();
    verify(operation, times(1)).safeRun();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asRunnableThrowingException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(new CheckedTestException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());
    doCallRealMethod().when(operation).asRunnable();

    Runnable runnable = operation.asRunnable();

    assertThat(runnable).isNotNull();

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(runnable::run)
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(CheckedTestException.class);

    verify(operation, times(1)).asRunnable();
    verify(operation, times(1)).safeRun();
    verify(operation, times(1)).run();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asSupplier() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doReturn("test").when(operation).safeRun(any());
    doCallRealMethod().when(operation).asSupplier();

    Supplier<Object> supplier = operation.asSupplier();

    assertThat(supplier).isNotNull();
    assertThat(supplier.get()).isEqualTo("test");

    verify(operation, times(1)).asSupplier();
    verify(operation, times(1)).safeRun();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void asSupplierThrowingException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(new CheckedTestException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());
    doCallRealMethod().when(operation).asSupplier();

    Supplier<Object> supplier = operation.asSupplier();

    assertThat(supplier).isNotNull();

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(supplier::get)
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(CheckedTestException.class);

    verify(operation, times(1)).asSupplier();
    verify(operation, times(1)).safeRun();
    verify(operation, times(1)).run();
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void safeRunCallsRun() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doReturn("test").when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());

    assertThat(operation.safeRun("mockOne", "mockTwo")).isEqualTo("test");

    verify(operation, times(1)).safeRun(eq("mockOne"), eq("mockTwo"));
    verify(operation, times(1)).run(eq("mockOne"), eq("mockTwo"));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void safeRunHandlesThrowableThrownByRun() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doThrow(newRuntimeException("TEST")).when(operation).run(any());
    doCallRealMethod().when(operation).safeRun(any());

    assertThatExceptionOfType(ThrowableOperationException.class)
      .isThrownBy(() -> operation.safeRun("mock"))
      .withMessage("Failed to run ThrowableOperation [%s]", operation)
      .withCauseInstanceOf(RuntimeException.class);

    verify(operation, times(1)).safeRun(eq("mock"));
    verify(operation, times(1)).run(eq("mock"));
    verifyNoMoreInteractions(operation);
  }

  @Test
  public void andThenWithNonNullThrowableOperation() throws Throwable {

    ThrowableOperation<Object> mockOperationOne = mock(ThrowableOperation.class);
    ThrowableOperation<Object> mockOperationTwo = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperationOne).andThen(eq(mockOperationTwo));

    ThrowableOperation<Object> composedOperation = mockOperationOne.andThen(mockOperationTwo);

    doReturn("1").when(mockOperationOne).run(eq("test"));
    doReturn("2").when(mockOperationTwo).run(eq("1"));

    assertThat(composedOperation).isNotNull();
    assertThat(composedOperation.run("test")).isEqualTo("2");

    InOrder order = inOrder(mockOperationOne, mockOperationTwo);

    order.verify(mockOperationOne, times(1)).andThen(eq(mockOperationTwo));
    order.verify(mockOperationOne, times(1)).run(eq("test"));
    order.verify(mockOperationTwo, times(1)).run(eq("1"));
    verifyNoMoreInteractions(mockOperationOne, mockOperationTwo);
  }

  @Test
  public void andThenWithNull() {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).andThen(any());

    assertThat(mockOperation.andThen(null)).isSameAs(mockOperation);
  }
}
