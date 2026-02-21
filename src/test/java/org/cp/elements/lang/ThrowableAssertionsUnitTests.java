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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatArrayIndexOutOfBoundsException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalStateException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIndexOutOfBoundsException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatInterruptedException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatNullPointerException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatRuntimeException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatSecurityException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.cp.elements.lang.ThrowableAssertions.assertThatUnsupportedOperationException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNotNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.PatternSyntaxException;

import org.junit.jupiter.api.Test;

import org.assertj.core.api.Assertions;
import org.cp.elements.function.FunctionExecutionException;
import org.cp.elements.function.ThrowingConsumer;
import org.cp.elements.function.ThrowingFunction;
import org.cp.elements.function.ThrowableSupplier;
import org.cp.elements.lang.ThrowableAssertions.AssertThatThrowableExpression;
import org.cp.elements.lang.ThrowableAssertions.ThrowableSource;
import org.cp.elements.lang.ThrowableAssertions.ThrowableSourceExpression;
import org.cp.elements.security.AuthenticationException;
import org.cp.elements.security.AuthorizationException;
import org.cp.elements.security.model.User;
import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.ArrayUtils;
import org.mockito.InOrder;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link ThrowableAssertions}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.ThrowableAssertions
 * @since 1.0.0
 */
class ThrowableAssertionsUnitTests {

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

  private static void consumerThrowingException(Object target) throws Exception {
    throw new Exception("TEST");
  }

  private static ThrowingFunction<?, ?> functionThrowingException(Object target) throws Exception {
    throw new Exception("TEST");
  }

  private static Object supplierThrowingException() throws Exception {
    throw new Exception("TEST");
  }

  @Test
  void constructAssertThatThrowable() {

    AuthenticationException exception = new AuthenticationException("test");

    AssertThatThrowableExpression expression = new AssertThatThrowableExpression(RuntimeException.class, exception);

    assertThat(expression).isNotNull();
    assertThat(expression.getThrowable()).isSameAs(exception);
    assertThat(expression.getType()).isEqualTo(RuntimeException.class);
  }

  @Test
  void constructAssertThatThrowableWithIncompatibleThrowable() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> AssertThatThrowableExpression.from(IllegalAccessException.class, new AuthorizationException("test")))
      .withMessage("Expected Throwable [%s] to be an instance of [%s]",
        AuthorizationException.class.getName(), IllegalAccessException.class.getName())
      .withNoCause();
  }

  @Test
  void constructAssertThatThrowableWithNullThrowable() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> AssertThatThrowableExpression.from(RuntimeException.class, null))
      .withMessage("Expected Throwable [null] to be an instance of [%s]", RuntimeException.class.getName())
      .withNoCause();
  }

  @Test
  void constructAssertThatThrowableWithNullType() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> AssertThatThrowableExpression.from(null, new AuthenticationException("test")))
      .withMessage("Type of Throwable is required")
      .withNoCause();
  }

  @Test
  void constructThrowableSource() {

    ThrowableSourceExpression expression = new ThrowableSourceExpression(IllegalArgumentException.class);

    assertThat(expression).isNotNull();
    assertThat(expression.getType()).isEqualTo(IllegalArgumentException.class);
    assertThat(expression.getDescription())
      .isEqualTo("Expected Throwable of type [%s] to be thrown by operation",
        IllegalArgumentException.class.getName());
  }

  @Test
  void constructThrowableSourceDescribedAs() {

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
  void constructThrowableSourceWithNullType() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowableSource.from(null))
      .withMessage("Type of Throwable is required")
      .withNoCause();
  }

  @Test
  void assertApplicationExceptionThrownByIsCorrect() {

    assertThatApplicationException()
      .isThrownBy(args -> codeThrowingApplicationException("mock", new SecurityException("test security error")))
      .havingMessage("mock")
      .causedBy(SecurityException.class)
      .havingMessage("test security error")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionThrownByIsCorrect() {

    assertThatRuntimeException()
      .isThrownBy(args -> codeThrowingRuntimeException("test message"))
      .havingMessage("test message")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionCausedByIsCorrect() {

    assertThatRuntimeException()
      .isThrownBy(args -> codeThrowingRuntimeException("test message",
        new IllegalStateException("mock message")))
      .havingMessage("%s message", "test")
      .causedBy(IllegalStateException.class)
      .havingMessage("{0} message", "mock")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionWithNoCauseThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException("test message"))
        .causedBy(IllegalAccessException.class)
        .havingMessage("mock message")
        .withNoCause())
      .withMessage("Expected Throwable [%s] to have a cause of type [%s]; but was [null]",
        RuntimeException.class.getName(), IllegalAccessException.class.getName())
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionWithUnexpectedCauseThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException("test message", new IllegalStateException("mock message")))
        .havingMessage("test message")
        .withNoCause())
      .withMessage("Expected Throwable [%s] to have no cause; but was caused by [%s]",
        RuntimeException.class.getName(), IllegalStateException.class.getName())
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionWithNoMessageThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException(null))
        .havingMessage("expected message")
        .withNoCause())
      .withMessage("Expected message [expected message]; but was [null]")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionHavingUnexpectedMessageThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .isThrownBy(args -> codeThrowingRuntimeException("actual message"))
        .havingMessage("expected message")
        .withNoCause())
      .withMessage("Expected message [expected message]; but was [actual message]")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionHavingMessageContainingIsCorrect() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> codeThrowingIllegalArgumentException("An error occurred on line 123 in file os.bin"))
      .havingMessageContaining("error occurred on line")
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionHavingMessageContainingUnexpectedTextThrowsAssertionException() {

    String message = "An error occurred on line 987 in file junk.txt";

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatIllegalStateException()
        .isThrownBy(args -> codeThrowingIllegalStateException(message))
        .havingMessageContaining("error occurred on line 123")
        .withNoCause())
      .withMessage("Expected message containing [error occurred on line 123] in [%s]", message)
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionHavingMessageEndingWithIsCorrect() {

    assertThatIndexOutOfBoundsException()
      .isThrownBy(args -> codeThrowingIndexOutOfBoundsException("Mock message ending with test"))
      .havingMessageEndingWith("ending with test")
      .withNoCause();
  }

  // java.lang.InterruptedException

  @Test
  void assertCheckedExceptionHavingMessageEndingWithUnexpectedTextThrowsAssertionException() {

    String message = "Beginning of the message and then the end of the message";

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatInterruptedException()
        .isThrownBy(args -> codeThrowingInterruptedException(message))
        .havingMessageEndingWith("Beginning of the message")
        .withNoCause())
      .withMessage("Expected message ending with [Beginning of the message] in [%s]", message)
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionHavingMessageStartingWithIsCorrect() {

    assertThatNullPointerException()
      .isThrownBy(args -> codeThrowingNullPointerException("Beginning of a mock message"))
      .havingMessageStartingWith("Beginning of")
      .withNoCause();
  }

  @Test
  void assertNonCheckedExceptionHavingMessageStartingWithUnexpectedTextThrowsAssertionException() {

    String message = "Beginning of the message and then the end of the message";

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatUnsupportedOperationException()
        .isThrownBy(args -> codeThrowingUnsupportedOperationException(message))
        .havingMessageStartingWith("and then the end of the message")
        .withNoCause())
      .withMessage("Expected message starting with [and then the end of the message] in [%s]", message)
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionWithNonThrowingCodeThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .isThrownBy(args -> codeReturningValue()))
      .withMessage("Expected Throwable of type [%s] to be thrown by operation",
        RuntimeException.class.getName())
      .withNoCause();
  }

  @Test
  void assertRuntimeExceptionWithNonThrowingCodeThrowsAssertionExceptionUsingDescription() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatRuntimeException()
        .describedAs("Test %1$s %2$s Message", "Assertion", "Error")
        .isThrownBy(args -> codeReturningValue()))
      .withMessage("Test Assertion Error Message")
      .withNoCause();
  }

  // DUPLICATE
  // @see assertRuntimeExceptionWithNoCauseThrowsAssertionException();
  @Test
  void assertSecurityExceptionWithNoCauseThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatSecurityException()
        .isThrownBy(args -> codeThrowingSecurityException("Unauthorized user"))
        .causedBy(AuthorizationException.class)
        .havingMessage("[jonDoe] is not authorized to perform action [save]")
        .withNoCause())
      .withMessage("Expected Throwable [%s] to have a cause of type [%s]; but was [null]",
        SecurityException.class.getName(), AuthorizationException.class.getName())
      .withNoCause();
  }

  @Test
  void assertSecurityExceptionWithExpectedButUnexpectedCauseThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatSecurityException()
        .isThrownBy(args -> codeThrowingSecurityException("Unauthorized user",
          new AuthenticationException("Unauthenticated user")))
        .causedBy(AuthorizationException.class)
        .havingMessage("[janeDoe] is not authorized to perform action [delete]")
        .withNoCause())
      .withMessage("Expected Throwable [%s] to have a cause of type [%s]; but was [%s]",
        SecurityException.class.getName(), AuthorizationException.class.getName(),
        AuthenticationException.class.getName())
      .withNoCause();
  }

  @Test
  void assertSecurityExceptionWithExpectedButUnexpectedMessageThrowsAssertionException() {

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatSecurityException()
        .isThrownBy(args -> codeThrowingSecurityException("User [rogueOne] is not authorized to execute operation [findAll]"))
        .havingMessage("User [%1$s] is unauthorized to execute [%2$s]", "rogueOne", "findAll")
        .withNoCause())
      .withMessage("Expected message [User [rogueOne] is unauthorized to execute [findAll]];"
        + " but was [User [rogueOne] is not authorized to execute operation [findAll]]")
      .withNoCause();
  }

  @Test
  void assertIllegalArgumentExceptionUsingArrayOfArguments() {

    User<Integer> jonDoe = TestUser.as("jonDoe");

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(IllegalArgumentException::new));

    assertThatIllegalArgumentException()
      .usingArguments(true, 1, "mock", jonDoe)
      .isThrownBy(operation::run)
      .havingMessage("TEST")
      .withNoCause();

    verify(operation, times(1)).run(eq(new Object[] { true, 1, "mock", jonDoe }));
    verifyNoMoreInteractions(operation);
  }

  @Test
  @SuppressWarnings("unchecked")
  void assertIllegalArgumentExceptionUsingSupplierOfArguments() {

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(IllegalArgumentException::new));

    Supplier<Object[]> mockArgumentSupplier = mock(Supplier.class);

    User<Integer> janeDoe = TestUser.as("janeDoe");

    doReturn(ArrayUtils.asArray(true, 1, "mock", janeDoe)).when(mockArgumentSupplier).get();

    ThrowableSource throwableSource = spy(assertThatIllegalArgumentException()
      .usingArguments(mockArgumentSupplier));

    throwableSource
      .isThrownBy(operation::run)
      .havingMessage("TEST")
      .withNoCause();

    InOrder order = inOrder(mockArgumentSupplier, operation, throwableSource);

    order.verify(throwableSource, times(1)).isThrownBy(isNotNull());
    order.verify(mockArgumentSupplier, times(1)).get();
    order.verify(operation, times(1)).run(eq(new Object[] { true, 1, "mock", janeDoe }));

    verifyNoMoreInteractions(mockArgumentSupplier, operation);
  }

  @Test
  @SuppressWarnings("unchecked")
  void assertIllegalStateExceptionHavingMessageMatchingRegularExpression() {

    User<Integer> jonDoe = TestUser.as("jonDoe");

    ObjectOperationThrowingRuntimeException operation =
      new ObjectOperationThrowingRuntimeException(IllegalArgumentException::new);

    assertThatIllegalStateException()
      .usingArguments(jonDoe)
      .isThrownBy(args -> operation.process((User<Integer>) ArrayUtils.getFirstElement(args)))
      .havingMessageMatching("Cannot process User \\[.*\\(.*jonDoe\\)]")
      .withNoCause();
  }

  @Test
  void assertThatArrayIndexOutOfBoundsExceptionIsThrownByOperation() {

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(ArrayIndexOutOfBoundsException::new));

    assertThatArrayIndexOutOfBoundsException()
      .isThrownBy(args -> operation.atIndex(-1))
      .havingMessage("Index [-1] is not valid")
      .withNoCause();
  }

  @Test
  void assertIndexOutOfBoundsExceptionHavingNonRegularExpressionMatchingMessage() {

    ObjectOperationThrowingRuntimeException operation =
      new ObjectOperationThrowingRuntimeException(IndexOutOfBoundsException::new);

    Assertions.assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThatIndexOutOfBoundsException()
        .usingArguments(10)
        .isThrownBy(args -> operation.atIndex(Integer.parseInt(String.valueOf(ArrayUtils.getFirstElement(args)))))
        .havingMessageMatching("Index \\[\\d] is not valid")
        .withNoCause())
      .withMessage("The Throwable [%s] message [Index [10] is not valid] does not match the pattern [Index \\[\\d] is not valid]",
          IndexOutOfBoundsException.class.getName())
      .withNoCause();
  }

  @Test
  void assertThatInterruptedExceptionIsThrownByOperation() {

    ObjectOperationThrowingCheckedException operation =
      spy(new ObjectOperationThrowingCheckedException(InterruptedException::new));

    assertThatInterruptedException()
      .isThrownBy(args -> operation.run("TEST"))
      .havingMessage("TEST")
      .withNoCause();
  }

  @Test
  void assertThatNullPointerExceptionWithInvalidRegularExpressionPattern() {

    ObjectOperationThrowingRuntimeException operation =
      new ObjectOperationThrowingRuntimeException(NullPointerException::new);

    Assertions.assertThatExceptionOfType(PatternSyntaxException.class)
      .isThrownBy(() -> assertThatNullPointerException()
        .isThrownBy(args -> operation.usingNullReference())
        .havingMessageMatching("\\(.*)")
        .withNoCause())
      .withMessageStartingWith("Unmatched closing ')'")
      .withNoCause();
  }

  @Test
  void assertThatRuntimeExceptionIsThrownByOperation() {

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(RuntimeException::new));

    assertThatRuntimeException()
      .isThrownBy(args -> operation.run(ObjectUtils.EMPTY_OBJECT_ARRAY))
      .havingMessage("TEST")
      .withNoCause();
  }

  @Test
  void assertThatSecurityExceptionIsThrownByOperation() {

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(SecurityException::new));

    assertThatSecurityException()
      .isThrownBy(args -> operation.secure())
      .havingMessage("INSECURE")
      .withNoCause();
  }

  @Test
  void assertThatUnsupportedOperationExceptionIsThrownByOperation() {

    ObjectOperationThrowingRuntimeException operation =
      spy(new ObjectOperationThrowingRuntimeException(UnsupportedOperationException::new));

    assertThatUnsupportedOperationException()
      .isThrownBy(args -> operation.notSupported())
      .havingMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  @Test
  void assertConsumerThrowingException() {

    assertThatThrowableOfType(IllegalStateException.class)
      .thrownBy(ThrowableAssertionsUnitTests::consumerThrowingException)
      .havingMessageStartingWith("Failed to consume object")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();
  }

  @Test
  void assertWithNullThrowingConsumer() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThatThrowableOfType(Exception.class).thrownBy((ThrowingConsumer<?>) null))
      .withMessage("Consumer is required")
      .withNoCause();
  }

  @Test
  void assertFunctionThrowingException() {

    assertThatThrowableOfType(FunctionExecutionException.class)
      .thrownByFunction(ThrowableAssertionsUnitTests::functionThrowingException)
      .havingMessageStartingWith("Failed to execute Function")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();
  }

  @Test
  void assertWithNullThrowingFunction() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThatThrowableOfType(Exception.class).thrownByFunction(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void assertSupplierThrowsException() {

    assertThatThrowableOfType(IllegalStateException.class)
      .thrownBy(ThrowableAssertionsUnitTests::supplierThrowingException)
      .havingMessage("Failed to get supplied value")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();
  }

  @Test
  void withNullThrowingSupplier() {

    Assertions.assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThatThrowableOfType(Exception.class).thrownBy((ThrowableSupplier<?>) null))
      .withMessage("Supplier is required")
      .withNoCause();
  }

  @Test
  void testAssertJExceptionAssertions() {

    try {
      assertThatExceptionOfType(ApplicationException.class)
        .isThrownBy(() -> codeThrowingApplicationException("app message",
          new SecurityException("security message", new RuntimeException("runtime message"))))
        .withMessage("app message")
        .withCauseInstanceOf(SecurityException.class)
        .withMessage("security message")
        .withCauseInstanceOf(RuntimeException.class)
        .withMessage("runtime message")
        .withNoCause();

      throw newTestException("AssertJ does not handle Exception switching unlike Elements");
    }
    catch (AssertionError ignore) { }
  }

  static class ObjectOperationThrowingCheckedException {

    private final Function<String, Exception> exceptionFunction;

    ObjectOperationThrowingCheckedException(Function<String, Exception> exceptionFunction) {
      this.exceptionFunction = exceptionFunction;
    }

    Object run(Object... arguments) throws Exception {

      String argument = Arrays.stream(ArrayUtils.nullSafeArray(arguments))
        .reduce((one, two) -> String.valueOf(one).concat(String.valueOf(two)))
        .orElse("NULL")
        .toString();

      throw this.exceptionFunction.apply(argument);
    }
  }

  static class ObjectOperationThrowingRuntimeException {

    private final Function<String, RuntimeException> runtimeExceptionFunction;


    ObjectOperationThrowingRuntimeException(Function<String, RuntimeException> runtimeExceptionFunction) {
      this.runtimeExceptionFunction = runtimeExceptionFunction;
    }

    Object atIndex(int index) {
      throw this.runtimeExceptionFunction.apply(String.format("Index [%d] is not valid", index));
    }

    Object notSupported() {
      throw this.runtimeExceptionFunction.apply(Constants.OPERATION_NOT_SUPPORTED);
    }

    Object process(User<?> user) {
      throw newIllegalStateException("Cannot process User [%s]", user);
    }

    Object run(Object[] arguments) {
      throw this.runtimeExceptionFunction.apply("TEST");
    }

    Object secure() {
      throw this.runtimeExceptionFunction.apply("INSECURE");
    }

    Object usingNullReference() {
      throw this.runtimeExceptionFunction.apply("Null Pointer");
    }
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser implements User<Integer> {

    @Setter
    private Integer id;

    @lombok.NonNull
    private final String name;

  }
}
