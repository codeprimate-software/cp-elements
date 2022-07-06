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
import static org.cp.elements.lang.ElementsExceptionsFactory.newTestException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalStateException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIndexOutOfBoundsException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatInterruptedException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatNullPointerException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatRuntimeException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatSecurityException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.cp.elements.lang.ThrowableAssertions.assertThatUnsupportedOperationException;

import org.assertj.core.api.Assertions;
import org.cp.elements.lang.ThrowableAssertions.AssertThatThrowableExpression;
import org.cp.elements.lang.ThrowableAssertions.ThrowableSource;
import org.cp.elements.lang.ThrowableAssertions.ThrowableSourceExpression;
import org.cp.elements.security.AuthenticationException;
import org.cp.elements.security.AuthorizationException;
import org.cp.elements.util.ApplicationException;
import org.junit.Test;

/**
 * Unit Tests for {@link ThrowableAssertions}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.ThrowableAssertions
 * @since 1.0.0
 */
public class ThrowableAssertionsUnitTests {

  private static ThrowableSource assertThatApplicationException() {
    return assertThatThrowableOfType(ApplicationException.class);
  }

  private static Object codeReturningValue() {
    return "test";
  }

  private static Object codeThrowingApplicationException(String message, Throwable cause) throws ApplicationException {
    throw new ApplicationException(message, cause);
  }

  private static Object codeThrowingIllegalArgumentException(String message) {
    throw new IllegalArgumentException(message);
  }

  private static Object codeThrowingIllegalStateException(String message) {
    throw new IllegalStateException(message);
  }

  private static Object codeThrowingIndexOutOfBoundsException(String message) {
    throw new IndexOutOfBoundsException(message);
  }

  private static Object codeThrowingInterruptedException(String message) throws InterruptedException {
    throw new InterruptedException(message);
  }

  private static Object codeThrowingNullPointerException(String message) {
    throw new NullPointerException(message);
  }

  private static Object codeThrowingRuntimeException(String message) {
    return codeThrowingRuntimeException(message, null);
  }

  private static Object codeThrowingRuntimeException(String message, Throwable cause) {
    throw new RuntimeException(message, cause);
  }

  private static Object codeThrowingSecurityException(String message) {
    return codeThrowingSecurityException(message, null);
  }

  private static Object codeThrowingSecurityException(String message, Throwable cause) {
    throw new SecurityException(message, cause);
  }

  private static Object codeThrowingUnsupportedOperationException(String message) {
    throw new UnsupportedOperationException(message);
  }

  @Test
  public void constructAssertThatThrowable() {

    AuthenticationException exception = new AuthenticationException("test");

    AssertThatThrowableExpression expression = new AssertThatThrowableExpression(RuntimeException.class, exception);

    assertThat(expression).isNotNull();
    assertThat(expression.getThrowable()).isSameAs(exception);
    assertThat(expression.getType()).isEqualTo(RuntimeException.class);
  }

  @Test
  public void constructAssertThatThrowableWithIncompatibleThrowable() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> AssertThatThrowableExpression.from(SecurityException.class, new AuthenticationException("test")))
      .withMessage("Expected Throwable [%s] to be an instance of [%s]",
        AuthenticationException.class.getName(), SecurityException.class.getName())
      .withNoCause();
  }

  @Test
  public void constructAssertThatThrowableWithNullThrowable() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> AssertThatThrowableExpression.from(RuntimeException.class, null))
      .withMessage("Expected Throwable [null] to be an instance of [%s]", RuntimeException.class.getName())
      .withNoCause();
  }

  @Test
  public void constructAssertThatThrowableWithNullType() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> AssertThatThrowableExpression.from(null, new AuthorizationException("test")))
      .withMessage("The type of Throwable is required")
      .withNoCause();
  }

  @Test
  public void constructThrowableSource() {

    ThrowableSourceExpression expression = new ThrowableSourceExpression(IllegalArgumentException.class);

    assertThat(expression).isNotNull();
    assertThat(expression.getType()).isEqualTo(IllegalArgumentException.class);
    assertThat(expression.getDescription())
      .isEqualTo("Expected Throwable of type [%s] to be thrown by operation",
        IllegalArgumentException.class.getName());
  }

  @Test
  public void constructThrowableSourceDescribedAs() {

    ThrowableSourceExpression expression =
      (ThrowableSourceExpression) ThrowableSourceExpression.from(IllegalStateException.class)
        .describedAs("mock description");

    assertThat(expression).isNotNull();
    assertThat(expression.getType()).isEqualTo(IllegalStateException.class);
    assertThat(expression.getDescription()).isEqualTo("mock description");

    ThrowableSourceExpression modifiedExpression = (ThrowableSourceExpression) expression.describedAs(null);

    assertThat(modifiedExpression).isSameAs(expression);
    assertThat(modifiedExpression.getDescription())
      .isEqualTo("Expected Throwable of type [%s] to be thrown by operation",
        IllegalStateException.class.getName());
  }

  @Test
  public void constructThrowableSourceWithNullType() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableSource.from(null))
      .withMessage("The type of Throwable is required")
      .withNoCause();
  }

  @Test
  public void assertApplicationExceptionThrowByIsCorrect() {

    assertThatApplicationException()
      .isThrownBy(args -> codeThrowingApplicationException("mock", new SecurityException("test security error")))
      .havingMessage("mock")
      .causedBy(SecurityException.class)
      .havingMessage("test security error")
      .withNoCause();
  }

  @Test
  public void assertRuntimeExceptionThrownByIsCorrect() {

    assertThatRuntimeException()
      .isThrownBy(args -> codeThrowingRuntimeException("test message"))
      .havingMessage("test message")
      .withNoCause();
  }

  @Test
  public void assertRuntimeExceptionCausedByIsCorrect() {

    assertThatRuntimeException()
      .isThrownBy(args -> codeThrowingRuntimeException("test message",
        new IllegalStateException("mock message")))
      .havingMessage("%s message", "test")
      .causedBy(IllegalStateException.class)
      .havingMessage("%s message", "mock")
      .withNoCause();
  }

  @Test(expected = AssertionException.class)
  public void assertRuntimeExceptionWithUnexpectedCauseThrowsAssertionException() {

    try {
      assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException("mock", new IllegalStateException("test")))
        .havingMessage("mock")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected Throwable [%s] to have no cause; but was caused by [%s]",
        RuntimeException.class.getName(), IllegalStateException.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertRuntimeExceptionHavingUnexpectedMessageThrowsAssertionException() {

    try {
      assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException("actual message"))
        .havingMessage("expected message")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected message [expected message]; but was [actual message]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertRuntimeExceptionHavingMessageContainingIsCorrect() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> codeThrowingIllegalArgumentException("An error occurred on line 123 in file os.bin"))
      .havingMessageContaining("error occurred on line")
      .withNoCause();
  }

  @Test(expected = AssertionException.class)
  public void assertRuntimeExceptionHavingMessageContainingUnexpectedTextThrowsAssertionException() {

    String message = "An error occurred on line 987 in file junk.txt";

    try {
      assertThatIllegalStateException()
        .isThrownBy(args -> codeThrowingIllegalStateException(message))
        .havingMessageContaining("error occurred on line 123")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected message containing [error occurred on line 123] in [%s]",
        message);

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertRuntimeExceptionHavingMessageEndingWithIsCorrect() {

    assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> codeThrowingIndexOutOfBoundsException("Mock message ending with test"))
      .havingMessageEndingWith("ending with test")
      .withNoCause();
  }

  @Test(expected = AssertionException.class)
  public void assertCheckedExceptionHavingMessageEndingWithUnexpectedTextThrowsAssertionException() {

    String message = "Beginning of the message and then the end of the message";

    try {
      assertThatInterruptedException()
        .isThrownBy(args -> codeThrowingInterruptedException(message))
        .havingMessageEndingWith("Beginning of the message")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected)
        .hasMessage("Expected message ending with [Beginning of the message] in [%s]",message);

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertRuntimeExceptionHavingMessageStartingWithIsCorrect() {

    assertThatNullPointerException()
      .isThrownBy(args -> codeThrowingNullPointerException("Beginning of a mock message"))
      .havingMessageStartingWith("Beginning of")
      .withNoCause();
  }

  @Test(expected = AssertionException.class)
  public void assertNonCheckedExceptionHavingMessageStartingWithUnexpectedTextThrowsAssertionException() {

    String message = "Beginning of the message and then the end of the message";

    try {
      assertThatUnsupportedOperationException()
        .isThrownBy(args -> codeThrowingUnsupportedOperationException(message))
        .havingMessageStartingWith("and then the end of the message")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected)
        .hasMessage("Expected message starting with [and then the end of the message] in [%s]", message);

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertRuntimeExceptionWithNonThrowingCodeThrowsAssertionException() {

    try {
      assertThatRuntimeException()
        .isThrownBy(args -> codeReturningValue());
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected Throwable of type [%s] to be thrown by operation",
        RuntimeException.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertRuntimeExceptionWithNonThrowingCodeThrowsAssertionExceptionUsingDescription() {

    try {
      assertThatRuntimeException()
        .describedAs("Test %1$s %2$s Message", "Assertion", "Error")
        .isThrownBy(args -> codeReturningValue());
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Test Assertion Error Message");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertSecurityExceptionWithNoCauseThrowsAssertionException() {

    try {
      assertThatSecurityException()
        .isThrownBy(args -> codeThrowingSecurityException("Unauthorized user"))
        .causedBy(AuthorizationException.class)
        .havingMessage("[jonDoe] is not authorized to perform action [save]")
        .withNoCause();
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected Throwable [%s] to have a cause of type [%s]; but was [null]",
        SecurityException.class.getName(), AuthorizationException.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertSecurityExceptionWithUnexpectedCauseThrowsAssertionException() {

    try {
      assertThatSecurityException()
        .isThrownBy(args -> codeThrowingSecurityException("Unauthorized user",
          new AuthenticationException("Unauthenticated user")))
        .causedBy(AuthorizationException.class);
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("Expected Throwable [%s] to have a cause of type [%s]; but was [%s]",
        SecurityException.class.getName(), AuthorizationException.class.getName(), AuthenticationException.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void testAssertJExceptionAssertions() {

    try {
      assertThatExceptionOfType(ApplicationException.class)
        .isThrownBy(() -> codeThrowingApplicationException("app message",
          new SecurityException("security message", new RuntimeException("runtime message"))))
        .withMessage("app message")
        .withCauseInstanceOf(SecurityException.class)
        .withMessage("security message")
        .withCauseInstanceOf(RuntimeException.class)
        .withMessage("runtime message");

      throw newTestException("AssertJ does not handle Exception switching unlike Elements");
    }
    catch (AssertionError ignore) { }
  }
}
