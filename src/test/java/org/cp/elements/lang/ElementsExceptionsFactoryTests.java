/*
 * Copyright 2016 Author or Authors.
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
import static org.cp.elements.lang.ElementsExceptionsFactory.newCloneException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newComparisonException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConfigurationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newDataAccessException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newEqualityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newFormatException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIdentityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIllegalTypeException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newImmutableObjectException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newInitializationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNoAvailablePortException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newNoSuchFileException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newObjectNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPageNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newParseException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newReadOnlyException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newResourceNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newRuleException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSearchException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSecurityException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newServiceInvocationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSystemException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newTypeNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newUserException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newWriteOnlyException;

import org.cp.elements.biz.rules.RuleException;
import org.cp.elements.context.configure.ConfigurationException;
import org.cp.elements.dao.DataAccessException;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.net.NoAvailablePortException;
import org.cp.elements.security.AuthenticationException;
import org.cp.elements.security.AuthorizationException;
import org.cp.elements.security.SecurityException;
import org.cp.elements.service.ServiceInvocationException;
import org.cp.elements.text.FormatException;
import org.cp.elements.text.ParseException;
import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.ReadOnlyException;
import org.cp.elements.util.SystemException;
import org.cp.elements.util.UserException;
import org.cp.elements.util.WriteOnlyException;
import org.cp.elements.util.paging.PageNotFoundException;
import org.cp.elements.util.search.SearchException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ElementsExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.ElementsExceptionsFactory
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ElementsExceptionsFactoryTests {

  @Mock
  private Throwable mockCause;

  protected void assertThrowable(Throwable throwable, Class<? extends Throwable> type, String message) {
    assertThrowable(throwable, type, message, null);
  }

  protected void assertThrowable(Throwable throwable, Class<? extends Throwable> type,
    String message, Throwable cause) {

    assertThat(throwable).isNotNull();
    assertThat(throwable).isInstanceOf(type);
    assertThat(throwable).hasCause(cause);
    assertThat(throwable).hasMessage(message);
  }

  @Test
  public void newRuleExceptionWithMessage() {
    assertThrowable(newRuleException("test"), RuleException.class, "test");
  }

  @Test
  public void newRuleExceptionWithFormattedMessageAndCause() {
    assertThrowable(newRuleException(mockCause, "%s is a {1}", "This", "test"),
      RuleException.class, "This is a test", mockCause);
  }

  @Test
  public void newConfigurationExceptionWithMessage() {
    assertThrowable(newConfigurationException("test"), ConfigurationException.class, "test");
  }

  @Test
  public void newConfigurationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newConfigurationException(mockCause, "%s is a {1}", "This", "test"),
      ConfigurationException.class, "This is a test", mockCause);
  }

  @Test
  public void newDataAccessExceptionWithMessage() {
    assertThrowable(newDataAccessException("test"), DataAccessException.class, "test");
  }

  @Test
  public void newDataAccessExceptionWithFormattedMessageAndCause() {
    assertThrowable(newDataAccessException(mockCause, "%s is a {1}", "This", "test"),
      DataAccessException.class, "This is a test", mockCause);
  }

  @Test
  public void newNoSuchFileExceptionWithMessage() {
    assertThrowable(newNoSuchFileException("test"), NoSuchFileException.class, "test");
  }

  @Test
  public void newNoSuchFileExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoSuchFileException(mockCause, "%s is a {1}", "This", "test"),
      NoSuchFileException.class, "This is a test", mockCause);
  }

  @Test
  public void newAssertionExceptionWithMessage() {
    assertThrowable(newAssertionException("test"), AssertionException.class, "test");
  }

  @Test
  public void newAssertionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAssertionException(mockCause, "%s is a {1}", "This", "test"),
      AssertionException.class, "This is a test", mockCause);
  }

  @Test
  public void newCloneExceptionWithMessage() {
    assertThrowable(newCloneException("test"), CloneException.class, "test");
  }

  @Test
  public void newCloneExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCloneException(mockCause, "%s is a {1}", "This", "test"),
      CloneException.class, "This is a test", mockCause);
  }

  @Test
  public void newComparisonExceptionWithMessage() {
    assertThrowable(newComparisonException("test"), ComparisonException.class, "test");
  }

  @Test
  public void newComparisonExceptionWithFormattedMessageAndCause() {
    assertThrowable(newComparisonException(mockCause, "%s is a {1}", "This", "test"),
      ComparisonException.class, "This is a test", mockCause);
  }

  @Test
  public void newEqualityExceptionWithMessage() {
    assertThrowable(newEqualityException("test"), EqualityException.class, "test");
  }

  @Test
  public void newEqualityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newEqualityException(mockCause, "%s is a {1}", "This", "test"),
      EqualityException.class, "This is a test", mockCause);
  }

  @Test
  public void newIdentityExceptionWithMessage() {
    assertThrowable(newIdentityException("test"), IdentityException.class, "test");
  }

  @Test
  public void newIdentityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIdentityException(mockCause, "%s is a {1}", "This", "test"),
      IdentityException.class, "This is a test", mockCause);
  }

  @Test
  public void newIllegalTypeExceptionWithMessage() {
    assertThrowable(newIllegalTypeException("test"), IllegalTypeException.class, "test");
  }

  @Test
  public void newIllegalTypeExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIllegalTypeException(mockCause, "%s is a {1}", "This", "test"),
      IllegalTypeException.class, "This is a test", mockCause);
  }

  @Test
  public void newImmutableObjectExceptionWithMessage() {
    assertThrowable(newImmutableObjectException("test"), ImmutableObjectException.class, "test");
  }

  @Test
  public void newImmutableObjectExceptionWithFormattedMessageAndCause() {
    assertThrowable(newImmutableObjectException(mockCause, "%s is a {1}", "This", "test"),
      ImmutableObjectException.class, "This is a test", mockCause);
  }

  @Test
  public void newInitializationExceptionWithMessage() {
    assertThrowable(newInitializationException("test"), InitializationException.class, "test");
  }

  @Test
  public void newInitializationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newInitializationException(mockCause, "%s is a {1}", "This", "test"),
      InitializationException.class, "This is a test", mockCause);
  }

  @Test
  public void newObjectNotFoundExceptionWithMessage() {
    assertThrowable(newObjectNotFoundException("test"), ObjectNotFoundException.class, "test");
  }

  @Test
  public void newObjectNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newObjectNotFoundException(mockCause, "%s is a {1}", "This", "test"),
      ObjectNotFoundException.class, "This is a test", mockCause);
  }

  @Test
  public void newResourceNotFoundExceptionWithMessage() {
    assertThrowable(newResourceNotFoundException("test"), ResourceNotFoundException.class, "test");
  }

  @Test
  public void newResourceNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newResourceNotFoundException(mockCause, "%s is a {1}", "This", "test"),
      ResourceNotFoundException.class, "This is a test", mockCause);
  }

  @Test
  public void newTypeNotFoundExceptionWithMessage() {
    assertThrowable(newTypeNotFoundException("test"), TypeNotFoundException.class, "test");
  }

  @Test
  public void newTypeNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newTypeNotFoundException(mockCause, "%s is a {1}", "This", "test"),
      TypeNotFoundException.class, "This is a test", mockCause);
  }

  @Test
  public void newNoAvailablePortExceptionWithMessage() {
    assertThrowable(newNoAvailablePortException("test"), NoAvailablePortException.class, "test");
  }

  @Test
  public void newNoAvailablePortExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoAvailablePortException(mockCause, "%s is a {1}", "This", "test"),
      NoAvailablePortException.class, "This is a test", mockCause);
  }

  @Test
  public void newAuthenticationExceptionWithMessage() {
    assertThrowable(newAuthenticationException("test"), AuthenticationException.class, "test");
  }

  @Test
  public void newAuthenticationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAuthenticationException(mockCause, "%s is a {1}", "This", "test"),
      AuthenticationException.class, "This is a test", mockCause);
  }

  @Test
  public void newAuthorizationExceptionWithMessage() {
    assertThrowable(newAuthorizationException("test"), AuthorizationException.class, "test");
  }

  @Test
  public void newAuthorizationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newAuthorizationException(mockCause, "%s is a {1}", "This", "test"),
      AuthorizationException.class, "This is a test", mockCause);
  }

  @Test
  public void newSecurityExceptionWithMessage() {
    assertThrowable(newSecurityException("test"), SecurityException.class, "test");
  }

  @Test
  public void newSecurityExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSecurityException(mockCause, "%s is a {1}", "This", "test"),
      SecurityException.class, "This is a test", mockCause);
  }

  @Test
  public void newServiceInvoationExceptionWithMessage() {
    assertThrowable(newServiceInvocationException("test"), ServiceInvocationException.class, "test");
  }

  @Test
  public void newServiceInvocationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newServiceInvocationException(mockCause, "%s is a {1}", "This", "test"),
      ServiceInvocationException.class, "This is a test", mockCause);
  }

  @Test
  public void newFormatExceptionWithMessage() {
    assertThrowable(newFormatException("test"), FormatException.class, "test");
  }

  @Test
  public void newFormatExceptionWithFormattedMessageAndCause() {
    assertThrowable(newFormatException(mockCause, "%s is a {1}", "This", "test"),
      FormatException.class, "This is a test", mockCause);
  }

  @Test
  public void newParseExceptionWithMessage() {
    assertThrowable(newParseException("test"), ParseException.class, "test");
  }

  @Test
  public void newParseExceptionWithFormattedMessageAndCause() {
    assertThrowable(newParseException(mockCause, "%s is a {1}", "This", "test"),
      ParseException.class, "This is a test", mockCause);
  }

  @Test
  public void newApplicationExceptionWithMessage() {
    assertThrowable(newApplicationException("test"), ApplicationException.class, "test");
  }

  @Test
  public void newApplicationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newApplicationException(mockCause, "%s is a {1}", "This", "test"),
      ApplicationException.class, "This is a test", mockCause);
  }

  @Test
  public void newConversionExceptionWithMessage() {
    assertThrowable(newConversionException("test"), ConversionException.class, "test");
  }

  @Test
  public void newConversionExceptionWithFormattedMessageAndCause() {
    assertThrowable(newConversionException(mockCause, "%s is a {1}", "This", "test"),
      ConversionException.class, "This is a test", mockCause);
  }

  @Test
  public void newPageNotFoundExceptionWithMessage() {
    assertThrowable(newPageNotFoundException("test"), PageNotFoundException.class, "test");
  }

  @Test
  public void newPageNotFoundExceptionWithFormattedMessageAndCause() {
    assertThrowable(newPageNotFoundException(mockCause, "%s is a {1}", "This", "test"),
      PageNotFoundException.class, "This is a test", mockCause);
  }

  @Test
  public void newReadOnlyExceptionWithMessage() {
    assertThrowable(newReadOnlyException("test"), ReadOnlyException.class, "test");
  }

  @Test
  public void newReadOnlyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newReadOnlyException(mockCause, "%s is a {1}", "This", "test"),
      ReadOnlyException.class, "This is a test", mockCause);
  }

  @Test
  public void newSearchExceptionWithMessage() {
    assertThrowable(newSearchException("test"), SearchException.class, "test");
  }

  @Test
  public void newSearchExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSearchException(mockCause, "%s is a {1}", "This", "test"),
      SearchException.class, "This is a test", mockCause);
  }

  @Test
  public void newSystemExceptionWithMessage() {
    assertThrowable(newSystemException("test"), SystemException.class, "test");
  }

  @Test
  public void newSystemExceptionWithFormattedMessageAndCause() {
    assertThrowable(newSystemException(mockCause, "%s is a {1}", "This", "test"),
      SystemException.class, "This is a test", mockCause);
  }

  @Test
  public void newUserExceptionWithMessage() {
    assertThrowable(newUserException("test"), UserException.class, "test");
  }

  @Test
  public void newUserExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUserException(mockCause, "%s is a {1}", "This", "test"),
      UserException.class, "This is a test", mockCause);
  }

  @Test
  public void newWriteOnlyExceptionWithMessage() {
    assertThrowable(newWriteOnlyException("test"), WriteOnlyException.class, "test");
  }

  @Test
  public void newWriteOnlyExceptionWithFormattedMessageAndCause() {
    assertThrowable(newWriteOnlyException(mockCause, "%s is a {1}", "This", "test"),
      WriteOnlyException.class, "This is a test", mockCause);
  }
}
