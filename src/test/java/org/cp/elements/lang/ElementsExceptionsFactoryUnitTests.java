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
import static org.cp.elements.lang.ElementsExceptionsFactory.newApplicationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newAssertionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newAuthenticationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newAuthorizationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newBeansException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newCacheException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newCacheNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newCloneException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newComparisonException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newCompressionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConfigurationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConstructorNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newDataAccessException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newDecompressionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newDeserializationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newEmbeddedProcessExecutionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newEqualityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newExpectationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFailedTestException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFieldAccessException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFieldNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFormatException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFunctionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newHungTestException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIdentityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIllegalPropertyValueException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIllegalTypeException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newImmutableObjectException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newInitializationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newLoserException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newMappingException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newMethodInvocationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newMethodNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNetworkException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNoAvailablePortException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNoSuchConstructorException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNoSuchFileException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newObjectInstantiationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newObjectNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPageNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newParseException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPidUnknownException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newProcessException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newProcessExecutionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newProcessNotRespondingException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyNotSetException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyReadException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyWriteException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newReadOnlyException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newResourceNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newRuleException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSearchException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSecurityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSerializationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newServiceException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newServiceInvocationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newServiceUnavailableException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSortException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSystemException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newTestException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newThrowableOperationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newTypeNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newUndeclaredPropertyException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newUndefinedPropertyException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newUnhandledMethodInvocationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newUserException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newWriteOnlyException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.IllegalPropertyValueException;
import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.beans.PropertyNotSetException;
import org.cp.elements.beans.PropertyReadException;
import org.cp.elements.beans.PropertyWriteException;
import org.cp.elements.biz.rules.RuleException;
import org.cp.elements.context.configure.ConfigurationException;
import org.cp.elements.dao.DataAccessException;
import org.cp.elements.data.caching.CacheException;
import org.cp.elements.data.caching.CacheNotFoundException;
import org.cp.elements.data.compression.CompressionException;
import org.cp.elements.data.compression.DecompressionException;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.mapping.MappingException;
import org.cp.elements.data.serialization.DeserializationException;
import org.cp.elements.data.serialization.SerializationException;
import org.cp.elements.function.FunctionException;
import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.lang.factory.NoSuchConstructorException;
import org.cp.elements.lang.factory.ObjectInstantiationException;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldAccessException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodInvocationException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;
import org.cp.elements.net.NetworkException;
import org.cp.elements.net.NoAvailablePortException;
import org.cp.elements.process.EmbeddedProcessExecutionException;
import org.cp.elements.process.PidUnknownException;
import org.cp.elements.process.ProcessException;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessNotRespondingException;
import org.cp.elements.security.AbstractSecurityException;
import org.cp.elements.security.AuthenticationException;
import org.cp.elements.security.AuthorizationException;
import org.cp.elements.service.ServiceException;
import org.cp.elements.service.ServiceInvocationException;
import org.cp.elements.service.ServiceUnavailableException;
import org.cp.elements.test.FailedTestException;
import org.cp.elements.test.HungTestException;
import org.cp.elements.test.TestException;
import org.cp.elements.text.FormatException;
import org.cp.elements.text.ParseException;
import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.LoserException;
import org.cp.elements.util.ReadOnlyException;
import org.cp.elements.util.SystemException;
import org.cp.elements.util.UndeclaredPropertyException;
import org.cp.elements.util.UndefinedPropertyException;
import org.cp.elements.util.UserException;
import org.cp.elements.util.WriteOnlyException;
import org.cp.elements.util.paging.PageNotFoundException;
import org.cp.elements.util.search.SearchException;
import org.cp.elements.util.sort.SortException;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link ElementsExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ElementsExceptionsFactory
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ElementsExceptionsFactoryUnitTests {

  @Mock
  private Throwable mockCause;

  private void assertThrowable(Throwable throwable, Class<? extends Throwable> type, String message) {
    assertThrowable(throwable, type, message, null);
  }

  private void assertThrowable(Throwable throwable, Class<? extends Throwable> type,
      String message, Throwable cause) {

    assertThat(throwable).isNotNull();
    assertThat(throwable).isInstanceOf(type);
    assertThat(throwable).hasCause(cause);
    assertThat(throwable).hasMessage(message);
  }

  @Test
  public void newBeansExceptionWithMessage() {
    assertThrowable(newBeansException("test"), BeansException.class, "test");
  }

  @Test
  public void newBeansExceptionWithFormattedMessageAndCause() {
    assertThrowable(newBeansException(this.mockCause, "%s is a {1}", "This", "test"),
      BeansException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newIllegalPropertyValueExceptionWithMessage() {
    assertThrowable(newIllegalPropertyValueException("test"), IllegalPropertyValueException.class, "test");
  }

  @Test
  public void newIllegalPropertyValueExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIllegalPropertyValueException(this.mockCause, "%s is a {1}", "This", "test"),
      IllegalPropertyValueException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPropertyNotFoundExceptionWithMessage() {
    assertThrowable(newPropertyNotFoundException("test"), PropertyNotFoundException.class, "test");
  }

  @Test
  public void newPropertyNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPropertyNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      PropertyNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPropertyNotSetExceptionWithMessage() {
    assertThrowable(newPropertyNotSetException("test"), PropertyNotSetException.class, "test");
  }

  @Test
  public void newPropertyNotSetExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPropertyNotSetException(this.mockCause, "%s is a {1}", "This", "test"),
      PropertyNotSetException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPropertyReadExceptionWithMessage() {
    assertThrowable(newPropertyReadException("test"), PropertyReadException.class, "test");
  }

  @Test
  public void newPropertyReadExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPropertyReadException(this.mockCause, "%s is a {1}", "This", "test"),
      PropertyReadException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPropertyWriteExceptionWithMessage() {
    assertThrowable(newPropertyWriteException("test"), PropertyWriteException.class, "test");
  }

  @Test
  public void newPropertyWriteExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPropertyWriteException(this.mockCause, "%s is a {1}", "This", "test"),
      PropertyWriteException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newRuleExceptionWithMessage() {
    assertThrowable(newRuleException("test"), RuleException.class, "test");
  }

  @Test
  public void newRuleExceptionWithFormattedMessageAndCause() {
    assertThrowable(newRuleException(this.mockCause, "%s is a {1}", "This", "test"),
      RuleException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newConfigurationExceptionWithMessage() {
    assertThrowable(newConfigurationException("test"), ConfigurationException.class, "test");
  }

  @Test
  public void newConfigurationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newConfigurationException(this.mockCause, "%s is a {1}", "This", "test"),
      ConfigurationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newDataAccessExceptionWithMessage() {
    assertThrowable(newDataAccessException("test"), DataAccessException.class, "test");
  }

  @Test
  public void newDataAccessExceptionWithFormattedMessageAndCause() {
    assertThrowable(newDataAccessException(this.mockCause, "%s is a {1}", "This", "test"),
      DataAccessException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newCacheExceptionWithMessage() {
    assertThrowable(newCacheException("test"), CacheException.class, "test");
  }

  @Test
  public void newCacheExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCacheException(this.mockCause, "%s is a {1}", "This", "test"),
      CacheException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newCacheNotFoundExceptionWithMessage() {
    assertThrowable(newCacheNotFoundException("test"), CacheNotFoundException.class, "test");
  }

  @Test
  public void newCacheNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCacheNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      CacheNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newCompressionExceptionWithMessage() {
    assertThrowable(newCompressionException("test"), CompressionException.class, "test");
  }

  @Test
  public void newCompressionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCompressionException(this.mockCause, "%s is a {1}", "This", "test"),
      CompressionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newDecompressionExceptionWithMessage() {
    assertThrowable(newDecompressionException("test"), DecompressionException.class, "test");
  }

  @Test
  public void newDecompressionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newDecompressionException(this.mockCause, "%s is a {1}", "This", "test"),
      DecompressionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newConversionExceptionWithMessage() {
    assertThrowable(newConversionException("test"), ConversionException.class, "test");
  }

  @Test
  public void newConversionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newConversionException(this.mockCause, "%s is a {1}", "This", "test"),
      ConversionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newDeserializationExceptionWithMessage() {
    assertThrowable(newDeserializationException("test"), DeserializationException.class, "test");
  }

  @Test
  public void newDeserializationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newDeserializationException(this.mockCause, "%s is a {1}", "This", "test"),
      DeserializationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newSerializationExceptionWithMessage() {
    assertThrowable(newSerializationException("test"), SerializationException.class, "test");
  }

  @Test
  public void newSerializationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSerializationException(this.mockCause, "%s is a {1}", "This", "test"),
      SerializationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newMappingExceptionWithMessage() {
    assertThrowable(newMappingException("test"), MappingException.class, "test");
  }

  @Test
  public void newMappingExceptionWithFormattedMessageAndCause() {
    assertThrowable(newMappingException(this.mockCause, "%s is a {1}", "This", "test"),
      MappingException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newFunctionExceptionWithMessage() {
    assertThrowable(newFunctionException("test"), FunctionException.class, "test");
  }

  @Test
  public void newFunctionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFunctionException(this.mockCause, "%s is a {1}", "This", "test"),
      FunctionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newNoSuchFileExceptionWithMessage() {
    assertThrowable(newNoSuchFileException("test"), NoSuchFileException.class, "test");
  }

  @Test
  public void newNoSuchFileExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoSuchFileException(this.mockCause, "%s is a {1}", "This", "test"),
      NoSuchFileException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newAssertionExceptionWithMessage() {
    assertThrowable(newAssertionException("test"), AssertionException.class, "test");
  }

  @Test
  public void newAssertionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAssertionException(this.mockCause, "%s is a {1}", "This", "test"),
      AssertionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newCloneExceptionWithMessage() {
    assertThrowable(newCloneException("test"), CloneException.class, "test");
  }

  @Test
  public void newCloneExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCloneException(this.mockCause, "%s is a {1}", "This", "test"),
      CloneException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newComparisonExceptionWithMessage() {
    assertThrowable(newComparisonException("test"), ComparisonException.class, "test");
  }

  @Test
  public void newComparisonExceptionWithFormattedMessageAndCause() {
    assertThrowable(newComparisonException(this.mockCause, "%s is a {1}", "This", "test"),
      ComparisonException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newEqualityExceptionWithMessage() {
    assertThrowable(newEqualityException("test"), EqualityException.class, "test");
  }

  @Test
  public void newEqualityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newEqualityException(this.mockCause, "%s is a {1}", "This", "test"),
      EqualityException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newExpectationExceptionWithMessage() {
    assertThrowable(newExpectationException("test"), ExpectationException.class, "test");
  }

  @Test
  public void newExpectationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newExpectationException(this.mockCause, "%s is a {1}", "This", "test"),
      ExpectationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newIdentityExceptionWithMessage() {
    assertThrowable(newIdentityException("test"), IdentityException.class, "test");
  }

  @Test
  public void newIdentityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIdentityException(this.mockCause, "%s is a {1}", "This", "test"),
      IdentityException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newIllegalTypeExceptionWithMessage() {
    assertThrowable(newIllegalTypeException("test"), IllegalTypeException.class, "test");
  }

  @Test
  public void newIllegalTypeExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIllegalTypeException(this.mockCause, "%s is a {1}", "This", "test"),
      IllegalTypeException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newImmutableObjectExceptionWithMessage() {
    assertThrowable(newImmutableObjectException("test"), ImmutableObjectException.class, "test");
  }

  @Test
  public void newImmutableObjectExceptionWithFormattedMessageAndCause() {
    assertThrowable(newImmutableObjectException(this.mockCause, "%s is a {1}", "This", "test"),
      ImmutableObjectException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newInitializationExceptionWithMessage() {
    assertThrowable(newInitializationException("test"), InitializationException.class, "test");
  }

  @Test
  public void newInitializationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newInitializationException(this.mockCause, "%s is a {1}", "This", "test"),
      InitializationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newObjectNotFoundExceptionWithMessage() {
    assertThrowable(newObjectNotFoundException("test"), ObjectNotFoundException.class, "test");
  }

  @Test
  public void newObjectNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newObjectNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      ObjectNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newResourceNotFoundExceptionWithMessage() {
    assertThrowable(newResourceNotFoundException("test"), ResourceNotFoundException.class, "test");
  }

  @Test
  public void newResourceNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newResourceNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      ResourceNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newThrowableOperationExceptionWithMessage() {
    assertThrowable(newThrowableOperationException("test"), ThrowableOperationException.class, "test");
  }

  @Test
  public void newThrowableOperationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newThrowableOperationException(this.mockCause, "%s is a {1}", "This", "test"),
      ThrowableOperationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newTypeNotFoundExceptionWithMessage() {
    assertThrowable(newTypeNotFoundException("test"), TypeNotFoundException.class, "test");
  }

  @Test
  public void newTypeNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newTypeNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      TypeNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newNoSuchConstructorExceptionWithMessage() {
    assertThrowable(newNoSuchConstructorException("test"), NoSuchConstructorException.class, "test");
  }

  @Test
  public void newNoSuchConstructorExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoSuchConstructorException(this.mockCause, "%s is a {1}", "This", "test"),
      NoSuchConstructorException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newObjectInstantiationExceptionWithMessage() {
    assertThrowable(newObjectInstantiationException("test"), ObjectInstantiationException.class, "test");
  }

  @Test
  public void newObjectInstantiationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newObjectInstantiationException(this.mockCause, "%s is a {1}", "This", "test"),
      ObjectInstantiationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newConstructorNotFoundExceptionWithMessage() {
    assertThrowable(newConstructorNotFoundException("test"), ConstructorNotFoundException.class, "test");
  }

  @Test
  public void newConstructorNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newConstructorNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      ConstructorNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newFieldAccessExceptionExceptionWithMessage() {
    assertThrowable(newFieldAccessException("test"), FieldAccessException.class, "test");
  }

  @Test
  public void newFieldAccessExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFieldAccessException(this.mockCause, "%s is a {1}", "This", "test"),
      FieldAccessException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newFieldNotFoundExceptionExceptionWithMessage() {
    assertThrowable(newFieldNotFoundException("test"), FieldNotFoundException.class, "test");
  }

  @Test
  public void newFieldNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFieldNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      FieldNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newMethodInvocationExceptionExceptionWithMessage() {
    assertThrowable(newMethodInvocationException("test"), MethodInvocationException.class, "test");
  }

  @Test
  public void newMethodInvocationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newMethodInvocationException(this.mockCause, "%s is a {1}", "This", "test"),
      MethodInvocationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newMethodNotFoundExceptionExceptionWithMessage() {
    assertThrowable(newMethodNotFoundException("test"), MethodNotFoundException.class, "test");
  }

  @Test
  public void newMethodNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newMethodNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      MethodNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newUnhandledMethodInvocationExceptionExceptionWithMessage() {
    assertThrowable(newUnhandledMethodInvocationException("test"), UnhandledMethodInvocationException.class, "test");
  }

  @Test
  public void newUnhandledMethodInvocationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUnhandledMethodInvocationException(this.mockCause, "%s is a {1}", "This", "test"),
      UnhandledMethodInvocationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newNetworkExceptionWithMessage() {
    assertThrowable(newNetworkException("test"), NetworkException.class, "test");
  }

  @Test
  public void newNetworkExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNetworkException(this.mockCause, "%s is a {1}", "This", "test"),
      NetworkException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newNoAvailablePortExceptionWithMessage() {
    assertThrowable(newNoAvailablePortException("test"), NoAvailablePortException.class, "test");
  }

  @Test
  public void newNoAvailablePortExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoAvailablePortException(this.mockCause, "%s is a {1}", "This", "test"),
      NoAvailablePortException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newEmbeddedProcessExecutionExceptionWithMessage() {
    assertThrowable(newEmbeddedProcessExecutionException("test"), EmbeddedProcessExecutionException.class, "test");
  }

  @Test
  public void newEmbeddedProcessExecutionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newEmbeddedProcessExecutionException(this.mockCause, "%s is a {1}", "This", "test"),
      EmbeddedProcessExecutionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPidUnknownExceptionWithMessage() {
    assertThrowable(newPidUnknownException("test"), PidUnknownException.class, "test");
  }

  @Test
  public void newPidUnknownExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPidUnknownException(this.mockCause, "%s is a {1}", "This", "test"),
      PidUnknownException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newProcessExceptionWithMessage() {
    assertThrowable(newProcessException("test"), ProcessException.class, "test");
  }

  @Test
  public void newProcessExceptionWithFormattedMessageAndCause() {
    assertThrowable(newProcessException(this.mockCause, "%s is a {1}", "This", "test"),
      ProcessException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newProcessExecutionExceptionWithMessage() {
    assertThrowable(newProcessExecutionException("test"), ProcessExecutionException.class, "test");
  }

  @Test
  public void newProcessExecutionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newProcessExecutionException(this.mockCause, "%s is a {1}", "This", "test"),
      ProcessExecutionException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newProcessNotRespondingExceptionWithMessage() {
    assertThrowable(newProcessNotRespondingException("test"), ProcessNotRespondingException.class, "test");
  }

  @Test
  public void newProcessNotRespondingExceptionWithFormattedMessageAndCause() {
    assertThrowable(newProcessNotRespondingException(this.mockCause, "%s is a {1}", "This", "test"),
      ProcessNotRespondingException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newAuthenticationExceptionWithMessage() {
    assertThrowable(newAuthenticationException("test"), AuthenticationException.class, "test");
  }

  @Test
  public void newAuthenticationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAuthenticationException(this.mockCause, "%s is a {1}", "This", "test"),
      AuthenticationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newAuthorizationExceptionWithMessage() {
    assertThrowable(newAuthorizationException("test"), AuthorizationException.class, "test");
  }

  @Test
  public void newAuthorizationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAuthorizationException(this.mockCause, "%s is a {1}", "This", "test"),
      AuthorizationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newSecurityExceptionWithMessage() {
    assertThrowable(newSecurityException("test"), AbstractSecurityException.class, "test");
  }

  @Test
  public void newSecurityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSecurityException(this.mockCause, "%s is a {1}", "This", "test"),
      AbstractSecurityException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newServiceExceptionWithMessage() {
    assertThrowable(newServiceException("test"), ServiceException.class, "test");
  }

  @Test
  public void newServiceExceptionWithFormattedMessageAndCause() {
    assertThrowable(newServiceException(this.mockCause, "%s is a {1}", "This", "test"),
      ServiceException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newServiceInvocationExceptionWithMessage() {
    assertThrowable(newServiceInvocationException("test"), ServiceInvocationException.class, "test");
  }

  @Test
  public void newServiceInvocationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newServiceInvocationException(this.mockCause, "%s is a {1}", "This", "test"),
      ServiceInvocationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newServiceUnavailableExceptionWithMessage() {
    assertThrowable(newServiceUnavailableException("test"), ServiceUnavailableException.class, "test");
  }

  @Test
  public void newServiceUnavailableExceptionWithFormattedMessageAndCause() {
    assertThrowable(newServiceUnavailableException(this.mockCause, "%s is a {1}", "This", "test"),
      ServiceUnavailableException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newFailedTestExceptionWithMessage() {
    assertThrowable(newFailedTestException("test"), FailedTestException.class, "test");
  }

  @Test
  public void newFailedTestExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFailedTestException(this.mockCause, "%s is a {1}", "This", "test"),
      FailedTestException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newHungTestExceptionWithMessage() {
    assertThrowable(newHungTestException("test"), HungTestException.class, "test");
  }

  @Test
  public void newHungTestExceptionWithFormattedMessageAndCause() {
    assertThrowable(newHungTestException(this.mockCause, "%s is a {1}", "This", "test"),
      HungTestException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newTestExceptionWithMessage() {
    assertThrowable(newTestException("test"), TestException.class, "test");
  }

  @Test
  public void newTestExceptionWithFormattedMessageAndCause() {
    assertThrowable(newTestException(this.mockCause, "%s is a {1}", "This", "test"),
      TestException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newFormatExceptionWithMessage() {
    assertThrowable(newFormatException("test"), FormatException.class, "test");
  }

  @Test
  public void newFormatExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFormatException(this.mockCause, "%s is a {1}", "This", "test"),
      FormatException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newParseExceptionWithMessage() {
    assertThrowable(newParseException("test"), ParseException.class, "test");
  }

  @Test
  public void newParseExceptionWithFormattedMessageAndCause() {
    assertThrowable(newParseException(this.mockCause, "%s is a {1}", "This", "test"),
      ParseException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newApplicationExceptionWithMessage() {
    assertThrowable(newApplicationException("test"), ApplicationException.class, "test");
  }

  @Test
  public void newApplicationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newApplicationException(this.mockCause, "%s is a {1}", "This", "test"),
      ApplicationException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newLoserExceptionWithMessage() {
    assertThrowable(newLoserException("test"), LoserException.class, "test");
  }

  @Test
  public void newLoserExceptionWithFormattedMessageAndCause() {
    assertThrowable(newLoserException(this.mockCause, "%s is a {1}", "This", "test"),
      LoserException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newReadOnlyExceptionWithMessage() {
    assertThrowable(newReadOnlyException("test"), ReadOnlyException.class, "test");
  }

  @Test
  public void newReadOnlyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newReadOnlyException(this.mockCause, "%s is a {1}", "This", "test"),
      ReadOnlyException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newSystemExceptionWithMessage() {
    assertThrowable(newSystemException("test"), SystemException.class, "test");
  }

  @Test
  public void newSystemExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSystemException(this.mockCause, "%s is a {1}", "This", "test"),
      SystemException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newUndeclaredPropertyExceptionWithMessage() {
    assertThrowable(newUndeclaredPropertyException("test"), UndeclaredPropertyException.class, "test");
  }

  @Test
  public void newUndeclaredPropertyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUndeclaredPropertyException(this.mockCause, "%s is a {1}", "This", "test"),
      UndeclaredPropertyException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newUndefinedPropertyExceptionWithMessage() {
    assertThrowable(newUndefinedPropertyException("test"), UndefinedPropertyException.class, "test");
  }

  @Test
  public void newUndefinedPropertyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUndefinedPropertyException(this.mockCause, "%s is a {1}", "This", "test"),
      UndefinedPropertyException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newUserExceptionWithMessage() {
    assertThrowable(newUserException("test"), UserException.class, "test");
  }

  @Test
  public void newUserExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUserException(this.mockCause, "%s is a {1}", "This", "test"),
      UserException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newWriteOnlyExceptionWithMessage() {
    assertThrowable(newWriteOnlyException("test"), WriteOnlyException.class, "test");
  }

  @Test
  public void newWriteOnlyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newWriteOnlyException(this.mockCause, "%s is a {1}", "This", "test"),
      WriteOnlyException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newPageNotFoundExceptionWithMessage() {
    assertThrowable(newPageNotFoundException("test"), PageNotFoundException.class, "test");
  }

  @Test
  public void newPageNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPageNotFoundException(this.mockCause, "%s is a {1}", "This", "test"),
      PageNotFoundException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newSearchExceptionWithMessage() {
    assertThrowable(newSearchException("test"), SearchException.class, "test");
  }

  @Test
  public void newSearchExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSearchException(this.mockCause, "%s is a {1}", "This", "test"),
      SearchException.class, "This is a test", this.mockCause);
  }

  @Test
  public void newSortExceptionWithMessage() {
    assertThrowable(newSortException("test"), SortException.class, "test");
  }

  @Test
  public void newSortExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSortException(this.mockCause, "%s is a {1}", "This", "test"),
      SortException.class, "This is a test", this.mockCause);
  }
}
