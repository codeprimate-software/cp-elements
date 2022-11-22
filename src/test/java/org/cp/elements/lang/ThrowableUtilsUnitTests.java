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
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.InvocationTargetException;

import org.junit.Test;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link ThrowableUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ThrowableUtils
 * @since 1.0.0
 */
public class ThrowableUtilsUnitTests {

  private @NotNull String toString(@NotNull Throwable throwable) {

    StringBuilder builder = new StringBuilder(throwable.getClass().getName());

    if (throwable.getMessage() != null) {
      builder.append(":").append(" ").append(throwable.getMessage());
      builder.append(StringUtils.LINE_SEPARATOR);
    }

    for (StackTraceElement element : throwable.getStackTrace()) {
      builder.append("\tat ");
      builder.append(element);
      builder.append(StringUtils.LINE_SEPARATOR);
    }

    return builder.toString();
  }

  @Test
  public void getCauseFromThrowableWithCause() {

    IllegalArgumentException expectedCause = new IllegalArgumentException("Illegal Argument");

    assertThat(ThrowableUtils.getCause(new Throwable(expectedCause))).isSameAs(expectedCause);
  }

  @Test
  @SuppressWarnings("all")
  public void getCauseFromThrowableWithNoCause() {

    Throwable throwable = spy(new Throwable((Throwable) null));

    assertThat(ThrowableUtils.getCause(throwable)).isNull();

    verify(throwable, times(1)).getCause();
    verifyNoMoreInteractions(throwable);
  }

  @Test
  public void getCauseFromNullIsNullSafe() {
    assertThat(ThrowableUtils.getCause(null)).isNull();
  }

  @Test
  public void getCauseOfInvocationTargetExceptionFromInvocationTargetExceptionHavingCause() {

    IllegalStateException expectedCause = new IllegalStateException("Illegal State");

    assertThat(ThrowableUtils.getCauseOfInvocationTargetException(new InvocationTargetException(expectedCause)))
      .isSameAs(expectedCause);
  }

  @Test
  public void getCauseOfInvocationTargetExceptionFromInvocationTargetExceptionHavingNoCause() {
    assertThat(ThrowableUtils.getCauseOfInvocationTargetException(new InvocationTargetException(null))).isNull();
  }

  @Test
  public void getCauseOfInvocationTargetExceptionFromNullIsNullSafe() {
    assertThat(ThrowableUtils.getCauseOfInvocationTargetException(null)).isNull();
  }

  @Test
  public void getCauseOfInvocationTargetExceptionFromRuntimeExceptionHavingCause() {

    IllegalStateException cause = new IllegalStateException("Illegal State");
    RuntimeException expectedException = new RuntimeException("test", cause);

    assertThat(ThrowableUtils.getCauseOfInvocationTargetException(expectedException)).isSameAs(expectedException);
  }

  @Test
  public void getMessageFromThrowableWithMessage() {
    assertThat(ThrowableUtils.getMessage(new Throwable("test"))).isEqualTo("test");
  }

  @Test
  @SuppressWarnings("all")
  public void getMessageFromThrowableWithNoMessage() {

    Throwable throwableSpy = spy(new Throwable());

    assertThat(ThrowableUtils.getMessage(throwableSpy)).isNull();

    verify(throwableSpy, times(1)).getMessage();
  }

  @Test
  public void getMessageFromNullIsNullSafe() {
    assertThat(ThrowableUtils.getMessage(null)).isNull();
  }

  @Test
  public void getRootCauseFromThrowableHavingNoCause() {

    Throwable expectedRootCause = new Throwable();

    assertThat(ThrowableUtils.getRootCause(expectedRootCause)).isSameAs(expectedRootCause);
  }

  @Test
  public void getRootCauseFromThrowableWithCause() {

    IllegalArgumentException expectedRootCause = new IllegalArgumentException("Illegal Argument");

    assertThat(ThrowableUtils.getRootCause(new Throwable(expectedRootCause))).isSameAs(expectedRootCause);
  }

  @Test
  public void getRootCauseFromThrowableWithDeeplyRootedCause() {

    NullPointerException expectedRootCause = new NullPointerException("NPE");
    IllegalStateException illegalState = new IllegalStateException("Illegal State", expectedRootCause);
    IllegalArgumentException illegalArgument = new IllegalArgumentException("Illegal Argument", illegalState);
    RuntimeException runtime = new RuntimeException("Runtime", illegalArgument);

    assertThat(ThrowableUtils.getRootCause(new Throwable(runtime))).isSameAs(expectedRootCause);
  }

  @Test
  public void getRootCauseWithFromNullIsNullSafe() {
    assertThat(ThrowableUtils.getRootCause(null)).isNull();
  }

  @Test
  public void getStackTraceFromThrowable() {

    Throwable throwable = new Throwable("test");

    assertThat(ThrowableUtils.getStackTrace(throwable)).isEqualTo(toString(throwable));
  }

  @Test
  public void getStackTraceFromNullIsNullSafe() {
    assertThat(ThrowableUtils.getStackTrace(null)).isNull();
  }
}
