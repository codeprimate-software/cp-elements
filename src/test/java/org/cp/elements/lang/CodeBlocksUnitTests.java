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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Function;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.mockito.InOrder;

/**
 * Unit Tests for {@link CodeBlocks}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.CodeBlocks
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class CodeBlocksUnitTests {

  @Test
  public void ifElseExecutesIfBlock() {

    Predicate<Object> ifCondition = mock(Predicate.class);

    Function<Object, Object> ifBlock = mock(Function.class);
    Function<Object, Object> elseBlock = mock(Function.class);

    doReturn(true).when(ifCondition).test(eq("mock"));
    doReturn("test").when(ifBlock).apply(eq("mock"));

    assertThat(CodeBlocks.ifElse("mock", ifCondition, ifBlock, elseBlock)).isEqualTo("test");

    verify(ifCondition, times(1)).test(eq("mock"));
    verify(ifBlock, times(1)).apply(eq("mock"));
    verifyNoMoreInteractions(ifCondition, ifBlock);
    verifyNoInteractions(elseBlock);
  }

  @Test
  public void ifElseExecutesElseBlock() {

    Predicate<Object> ifCondition = mock(Predicate.class);

    Function<Object, Object> ifBlock = mock(Function.class);
    Function<Object, Object> elseBlock = mock(Function.class);

    doReturn(false).when(ifCondition).test(eq("mock"));
    doReturn("test").when(elseBlock).apply(eq("mock"));

    assertThat(CodeBlocks.ifElse("mock", ifCondition, ifBlock, elseBlock)).isEqualTo("test");

    verify(ifCondition, times(1)).test(eq("mock"));
    verify(elseBlock, times(1)).apply(eq("mock"));
    verifyNoMoreInteractions(ifCondition, elseBlock);
    verifyNoInteractions(ifBlock);
  }

  @Test(expected = IllegalArgumentException.class)
  public void ifElseWithNullIfCondition() {

    Function<Object, Object> ifBlock = mock(Function.class);
    Function<Object, Object> elseBlock = mock(Function.class);

    try {
      CodeBlocks.ifElse("test", null, ifBlock, elseBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Predicate used in the condition of the if statement is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(ifBlock, elseBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void ifElseWithNullIfBlock() {

    Predicate<Object> ifCondition = mock(Predicate.class);

    Function<Object, Object> elseBlock = mock(Function.class);

    try {
      CodeBlocks.ifElse("test", ifCondition, null, elseBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function for the if block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(ifCondition, elseBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void ifElseWithNullElseBlock() {

    Predicate<Object> ifCondition = mock(Predicate.class);

    Function<Object, Object> ifBlock = mock(Function.class);

    try {
      CodeBlocks.ifElse("test", ifCondition, ifBlock, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function for the else block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(ifCondition, ifBlock);
    }
  }

  @Test
  public void ifElseWithNullTargetIsNullSafe() {

    Predicate<Object> ifCondition = mock(Predicate.class);

    Function<Object, Object> ifBlock = mock(Function.class);
    Function<Object, Object> elseBlock = mock(Function.class);

    doReturn(true).when(ifCondition).test(any());
    doReturn("test").when(ifBlock).apply(any());

    assertThat(CodeBlocks.ifElse(null, ifCondition, ifBlock, elseBlock)).isEqualTo("test");

    verify(ifCondition, times(1)).test(isNull());
    verify(ifBlock, times(1)).apply(isNull());
    verifyNoMoreInteractions(ifCondition, ifBlock);
    verifyNoInteractions(elseBlock);
  }

  @Test
  public void tryCatchIsSuccessful() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Function<Throwable, RuntimeException> catchBlock = mock(Function.class);

    doReturn("test").when(operation).run(any(Object[].class));

    assertThat(CodeBlocks.tryCatch(operation, catchBlock)).isEqualTo("test");

    verify(operation, times(1)).run(any(Object[].class));
    verifyNoMoreInteractions(operation);
    verifyNoInteractions(catchBlock);
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryCatchThrowsRuntimeException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Function<Throwable, RuntimeException> catchBlock = mock(Function.class);

    doThrow(new TestException("test")).when(operation).run(any(Object[].class));

    doAnswer(invocation -> {
      throw new IllegalArgumentException(invocation.getArgument(0, Throwable.class).getMessage());
    }).when(catchBlock).apply(any(Throwable.class));

    try {
      CodeBlocks.tryCatch(operation, catchBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      InOrder order = inOrder(operation, catchBlock);

      order.verify(operation, times(1)).run(any(Object[].class));
      order.verify(catchBlock, times(1)).apply(isA(TestException.class));

      verifyNoMoreInteractions(operation, catchBlock);
    }
  }

  @Test(expected = IllegalStateException.class)
  public void tryCatchFinallyThrowsRuntimeException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Runnable finallyBlock = mock(Runnable.class);

    Function<Throwable, RuntimeException> catchBlock = mock(Function.class);

    doThrow(new TestError("test")).when(operation).run(any(Object[].class));

    doAnswer(invocation -> {
      throw new IllegalStateException(invocation.getArgument(0, Throwable.class).getMessage());
    }).when(catchBlock).apply(any(Throwable.class));

    try {
      CodeBlocks.tryCatchFinally(operation, catchBlock, finallyBlock);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      InOrder order = inOrder(operation, catchBlock, finallyBlock);

      order.verify(operation, times(1)).run(any(Object[].class));
      order.verify(catchBlock, times(1)).apply(isA(TestError.class));
      order.verify(finallyBlock, times(1)).run();

      verifyNoMoreInteractions(operation, finallyBlock, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryCatchFinallyWithNullTryBlock() {

    Runnable finallyBlock = mock(Runnable.class);

    Function<Throwable, RuntimeException> catchBlock = mock(Function.class);

    try {
      CodeBlocks.tryCatchFinally(null, catchBlock, finallyBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The ThrowableOperation invoked in the try block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(finallyBlock, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryCatchFinallyWithNullFinallyBlock() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Function<Throwable, RuntimeException> catchBlock = mock(Function.class);

    try {
      CodeBlocks.tryCatchFinally(operation, catchBlock, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Runnable invoked in the finally block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(operation, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryCatchFinallyWithNullCatchBlock() {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Runnable finallyBlock = mock(Runnable.class);

    try {
      CodeBlocks.tryCatchFinally(operation, null, finallyBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function converter invoked in the catch block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(operation, finallyBlock);
    }
  }

  @Test
  public void tryHandleIsSuccessful() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    doReturn("test").when(operation).run(any(Object[].class));

    Function<Throwable, Object> catchBlock = mock(Function.class);

    CodeBlocks.tryHandle(operation, catchBlock);

    verify(operation, times(1)).run(any(Object[].class));
    verifyNoMoreInteractions(operation);
    verifyNoInteractions(catchBlock);
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryHandleThrowsRuntimeException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Function<Throwable, Object> catchBlock = mock(Function.class);

    doThrow(new TestException("test")).when(operation).run(any(Object[].class));

    doAnswer(invocation -> {
      throw new IllegalArgumentException(invocation.getArgument(0, Throwable.class).getMessage());
    }).when(catchBlock).apply(isA(Throwable.class));

    try {
      CodeBlocks.tryHandle(operation, catchBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      InOrder order = inOrder(operation, catchBlock);

      order.verify(operation, times(1)).run(any(Object[].class));
      order.verify(catchBlock, times(1)).apply(isA(TestException.class));

      verifyNoMoreInteractions(operation, catchBlock);
    }
  }

  @Test(expected = IllegalStateException.class)
  public void tryHandleFinallyThrowsRuntimeException() throws Throwable {

    ThrowableOperation<Object> operation = mock(ThrowableOperation.class);

    Runnable finallyBlock = mock(Runnable.class);

    Function<Throwable, Object> catchBlock = mock(Function.class);

    doThrow(new TestError("test")).when(operation).run(any(Object[].class));

    doAnswer(invocation -> {
      throw new IllegalStateException(invocation.getArgument(0, Throwable.class).getMessage());
    }).when(catchBlock).apply(isA(Throwable.class));

    try {
      CodeBlocks.tryHandleFinally(operation, catchBlock, finallyBlock);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      InOrder order = inOrder(operation, catchBlock, finallyBlock);

      order.verify(operation, times(1)).run(any(Object[].class));
      order.verify(catchBlock, times(1)).apply(isA(TestError.class));
      order.verify(finallyBlock, times(1)).run();

      verifyNoMoreInteractions(operation, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryHandleFinallyWithNullTryBlock() {

    Runnable finallyBlock = mock(Runnable.class);

    Function<Throwable, Object> catchBlock = mock(Function.class);

    try {
      CodeBlocks.tryHandleFinally(null, catchBlock, finallyBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The ThrowableOperation invoked in the try block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(finallyBlock, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryHandleFinallyWithNullFinallyBlock() {

    ThrowableOperation<Object> tryBlock = mock(ThrowableOperation.class);

    Function<Throwable, Object> catchBlock = mock(Function.class);

    try {
      CodeBlocks.tryHandleFinally(tryBlock, catchBlock, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Runnable invoked in the finally block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(tryBlock, catchBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void tryHandleFinallyWithNullCatchBlock() {

    ThrowableOperation<Object> tryBlock = mock(ThrowableOperation.class);

    Runnable finallyBlock = mock(Runnable.class);

    try {
      CodeBlocks.tryHandleFinally(tryBlock, null, finallyBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function handler invoked in the catch block is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(tryBlock, finallyBlock);
    }
  }

  @Test
  public void whileLoopIsSuccessful() {

    Predicate<Integer> whileCondition = number ->  number < 200;

    Function<Integer, Integer> powerOfTwo = number -> number * number;

    assertThat(CodeBlocks.whileLoop(2, whileCondition, powerOfTwo)).isEqualTo(256);
  }

  @Test(expected = IllegalArgumentException.class)
  public void whileLoopWithNullWhileCondition() {

    Function<Object, Object> whileBlock = mock(Function.class);

    try {
      CodeBlocks.whileLoop("test", null, whileBlock);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The condition for the while-loop is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(whileBlock);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void whileLoopWithNullWhileBlock() {

    Predicate<Object> whileCondition = mock(Predicate.class);

    try {
      CodeBlocks.whileLoop("test", whileCondition, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function of the while-loop is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(whileCondition);
    }
  }

  @SuppressWarnings("unused")
  private static final class TestError extends Error {

    public TestError() { }

    public TestError(String message) {
      super(message);
    }

    public TestError(Throwable cause) {
      super(cause);
    }

    public TestError(String message, Throwable cause) {
      super(message, cause);
    }
  }

  @SuppressWarnings("unused")
  private static final class TestException extends Exception {

    public TestException() { }

    public TestException(String message) {
      super(message);
    }

    public TestException(Throwable cause) {
      super(cause);
    }

    public TestException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
