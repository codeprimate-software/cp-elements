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

import static org.cp.elements.text.FormatUtils.format;

import org.cp.elements.biz.rules.RuleException;
import org.cp.elements.context.configure.ConfigurationException;
import org.cp.elements.dao.DataAccessException;
import org.cp.elements.data.convert.ConversionException;
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
import org.cp.elements.util.search.SearchException;

/**
 * The {@link ElementsExceptionsFactory} class is an object factory used to construct and initialized common, useful
 * Elements API {@link RuntimeException} classes.
 *
 * @author John Blum
 * @see org.cp.elements.biz.rules.RuleException
 * @see org.cp.elements.context.configure.ConfigurationException
 * @see org.cp.elements.dao.DataAccessException
 * @see org.cp.elements.io.NoSuchFileException
 * @see org.cp.elements.lang.AssertionException
 * @see org.cp.elements.lang.CloneException
 * @see org.cp.elements.lang.ComparisonException
 * @see org.cp.elements.lang.EqualityException
 * @see org.cp.elements.lang.IdentityException
 * @see org.cp.elements.lang.IllegalTypeException
 * @see org.cp.elements.lang.ImmutableObjectException
 * @see org.cp.elements.lang.InitializationException
 * @see org.cp.elements.lang.ObjectNotFoundException
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @see org.cp.elements.lang.TypeNotFoundException
 * @see org.cp.elements.net.NoAvailablePortException
 * @see org.cp.elements.security.AuthenticationException
 * @see org.cp.elements.security.AuthorizationException
 * @see org.cp.elements.security.SecurityException
 * @see org.cp.elements.service.ServiceInvocationException
 * @see org.cp.elements.text.FormatException
 * @see org.cp.elements.text.ParseException
 * @see org.cp.elements.util.ApplicationException
 * @see org.cp.elements.util.ReadOnlyException
 * @see org.cp.elements.util.SystemException
 * @see org.cp.elements.util.UserException
 * @see org.cp.elements.util.WriteOnlyException
 * @see org.cp.elements.data.convert.ConversionException
 * @see org.cp.elements.util.search.SearchException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ElementsExceptionsFactory extends RuntimeException {

  // package org.cp.elements.biz.rules

  /**
   * Constructs and initializes a new {@link RuleException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link RuleException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuleException} with the given {@link String message}.
   * @see #newRuleException(Throwable, String, Object...)
   * @see org.cp.elements.biz.rules.RuleException
   */
  public static RuleException newRuleException(String message, Object... args) {
    return newRuleException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link RuleException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link RuleException} was thrown.
   * @param message {@link String} describing the {@link RuleException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuleException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.biz.rules.RuleException
   */
  public static RuleException newRuleException(Throwable cause, String message, Object... args) {
    return new RuleException(format(message, args), cause);
  }

  // package org.cp.elements.context.configure

  /**
   * Constructs and initializes a new {@link ConfigurationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ConfigurationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ConfigurationException} with the given {@link String message}.
   * @see #newConfigurationException(Throwable, String, Object...)
   * @see org.cp.elements.context.configure.ConfigurationException
   */
  public static ConfigurationException newConfigurationException(String message, Object... args) {
    return newConfigurationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ConfigurationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ConfigurationException} was thrown.
   * @param message {@link String} describing the {@link ConfigurationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ConfigurationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.context.configure.ConfigurationException
   */
  public static ConfigurationException newConfigurationException(Throwable cause, String message, Object... args) {
    return new ConfigurationException(format(message, args), cause);
  }

  // package org.cp.elements.dao

  /**
   * Constructs and initializes a new {@link DataAccessException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link DataAccessException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link DataAccessException} with the given {@link String message}.
   * @see #newDataAccessException(Throwable, String, Object...)
   * @see org.cp.elements.dao.DataAccessException
   */
  public static DataAccessException newDataAccessException(String message, Object... args) {
    return newDataAccessException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link DataAccessException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link DataAccessException} was thrown.
   * @param message {@link String} describing the {@link DataAccessException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link DataAccessException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.dao.DataAccessException
   */
  public static DataAccessException newDataAccessException(Throwable cause, String message, Object... args) {
    return new DataAccessException(format(message, args), cause);
  }

  // package org.cp.elements.io

  /**
   * Constructs and initializes a new {@link NoSuchFileException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link NoSuchFileException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchFileException} with the given {@link String message}.
   * @see #newNoSuchFileException(Throwable, String, Object...)
   * @see org.cp.elements.io.NoSuchFileException
   */
  public static NoSuchFileException newNoSuchFileException(String message, Object... args) {
    return newNoSuchFileException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link NoSuchFileException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link NoSuchFileException} was thrown.
   * @param message {@link String} describing the {@link NoSuchFileException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchFileException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.io.NoSuchFileException
   */
  public static NoSuchFileException newNoSuchFileException(Throwable cause, String message, Object... args) {
    return new NoSuchFileException(format(message, args), cause);
  }

  // package org.cp.elements.lang

  /**
   * Constructs and initializes a new {@link AssertionException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link AssertionException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AssertionException} with the given {@link String message}.
   * @see #newAssertionException(Throwable, String, Object...)
   * @see org.cp.elements.lang.AssertionException
   */
  public static AssertionException newAssertionException(String message, Object... args) {
    return newAssertionException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link AssertionException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link AssertionException} was thrown.
   * @param message {@link String} describing the {@link AssertionException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AssertionException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.AssertionException
   */
  public static AssertionException newAssertionException(Throwable cause, String message, Object... args) {
    return new AssertionException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link CloneException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link CloneException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneException} with the given {@link String message}.
   * @see #newCloneException(Throwable, String, Object...)
   * @see org.cp.elements.lang.CloneException
   */
  public static CloneException newCloneException(String message, Object... args) {
    return newCloneException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link CloneException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link CloneException} was thrown.
   * @param message {@link String} describing the {@link CloneException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.CloneException
   */
  public static CloneException newCloneException(Throwable cause, String message, Object... args) {
    return new CloneException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ComparisonException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ComparisonException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ComparisonException} with the given {@link String message}.
   * @see #newComparisonException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ComparisonException
   */
  public static ComparisonException newComparisonException(String message, Object... args) {
    return newComparisonException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ComparisonException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ComparisonException} was thrown.
   * @param message {@link String} describing the {@link ComparisonException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ComparisonException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.ComparisonException
   */
  public static ComparisonException newComparisonException(Throwable cause, String message, Object... args) {
    return new ComparisonException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link EqualityException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link EqualityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link EqualityException} with the given {@link String message}.
   * @see #newEqualityException(Throwable, String, Object...)
   * @see org.cp.elements.lang.EqualityException
   */
  public static EqualityException newEqualityException(String message, Object... args) {
    return newEqualityException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link EqualityException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link EqualityException} was thrown.
   * @param message {@link String} describing the {@link EqualityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link EqualityException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.EqualityException
   */
  public static EqualityException newEqualityException(Throwable cause, String message, Object... args) {
    return new EqualityException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link IdentityException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link IdentityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IdentityException} with the given {@link String message}.
   * @see #newIdentityException(Throwable, String, Object...)
   * @see org.cp.elements.lang.IdentityException
   */
  public static IdentityException newIdentityException(String message, Object... args) {
    return newIdentityException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IdentityException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link IdentityException} was thrown.
   * @param message {@link String} describing the {@link IdentityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IdentityException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.IdentityException
   */
  public static IdentityException newIdentityException(Throwable cause, String message, Object... args) {
    return new IdentityException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link IllegalTypeException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link IllegalTypeException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalTypeException} with the given {@link String message}.
   * @see #newIllegalTypeException(Throwable, String, Object...)
   * @see org.cp.elements.lang.IllegalTypeException
   */
  public static IllegalTypeException newIllegalTypeException(String message, Object... args) {
    return newIllegalTypeException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IllegalTypeException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link IllegalTypeException} was thrown.
   * @param message {@link String} describing the {@link IllegalTypeException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalTypeException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.IllegalTypeException
   */
  public static IllegalTypeException newIllegalTypeException(Throwable cause, String message, Object... args) {
    return new IllegalTypeException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ImmutableObjectException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ImmutableObjectException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ImmutableObjectException} with the given {@link String message}.
   * @see #newImmutableObjectException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ImmutableObjectException
   */
  public static ImmutableObjectException newImmutableObjectException(String message, Object... args) {
    return newImmutableObjectException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ImmutableObjectException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ImmutableObjectException} was thrown.
   * @param message {@link String} describing the {@link ImmutableObjectException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ImmutableObjectException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.ImmutableObjectException
   */
  public static ImmutableObjectException newImmutableObjectException(Throwable cause, String message, Object... args) {
    return new ImmutableObjectException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link InitializationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link InitializationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link InitializationException} with the given {@link String message}.
   * @see #newInitializationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.InitializationException
   */
  public static InitializationException newInitializationException(String message, Object... args) {
    return newInitializationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link InitializationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link InitializationException} was thrown.
   * @param message {@link String} describing the {@link InitializationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link InitializationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.InitializationException
   */
  public static InitializationException newInitializationException(Throwable cause, String message, Object... args) {
    return new InitializationException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ObjectNotFoundException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ObjectNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ObjectNotFoundException} with the given {@link String message}.
   * @see #newObjectNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ObjectNotFoundException
   */
  public static ObjectNotFoundException newObjectNotFoundException(String message, Object... args) {
    return newObjectNotFoundException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ObjectNotFoundException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ObjectNotFoundException} was thrown.
   * @param message {@link String} describing the {@link ObjectNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ObjectNotFoundException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.ObjectNotFoundException
   */
  public static ObjectNotFoundException newObjectNotFoundException(Throwable cause, String message, Object... args) {
    return new ObjectNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ResourceNotFoundException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ResourceNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ResourceNotFoundException} with the given {@link String message}.
   * @see #newResourceNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ResourceNotFoundException
   */
  public static ResourceNotFoundException newResourceNotFoundException(String message, Object... args) {
    return newResourceNotFoundException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ResourceNotFoundException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ResourceNotFoundException} was thrown.
   * @param message {@link String} describing the {@link ResourceNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ResourceNotFoundException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.ResourceNotFoundException
   */
  public static ResourceNotFoundException newResourceNotFoundException(Throwable cause,
      String message, Object... args) {

    return new ResourceNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link TypeNotFoundException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link TypeNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotFoundException} with the given {@link String message}.
   * @see #newTypeNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.TypeNotFoundException
   */
  public static TypeNotFoundException newTypeNotFoundException(String message, Object... args) {
    return newTypeNotFoundException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link TypeNotFoundException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link TypeNotFoundException} was thrown.
   * @param message {@link String} describing the {@link TypeNotFoundException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotFoundException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.lang.TypeNotFoundException
   */
  public static TypeNotFoundException newTypeNotFoundException(Throwable cause, String message, Object... args) {
    return new TypeNotFoundException(format(message, args), cause);
  }

  // package org.cp.elements.net

  /**
   * Constructs and initializes a new {@link NoAvailablePortException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link NoAvailablePortException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoAvailablePortException} with the given {@link String message}.
   * @see #newNoAvailablePortException(Throwable, String, Object...)
   * @see org.cp.elements.net.NoAvailablePortException
   */
  public static NoAvailablePortException newNoAvailablePortException(String message, Object... args) {
    return newNoAvailablePortException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link NoAvailablePortException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link NoAvailablePortException} was thrown.
   * @param message {@link String} describing the {@link NoAvailablePortException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoAvailablePortException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.net.NoAvailablePortException
   */
  public static NoAvailablePortException newNoAvailablePortException(Throwable cause, String message, Object... args) {
    return new NoAvailablePortException(format(message, args), cause);
  }

  // package org.cp.elements.security

  /**
   * Constructs and initializes a new {@link AuthenticationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link AuthenticationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AuthenticationException} with the given {@link String message}.
   * @see #newAuthenticationException(Throwable, String, Object...)
   * @see org.cp.elements.security.AuthenticationException
   */
  public static AuthenticationException newAuthenticationException(String message, Object... args) {
    return newAuthenticationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link AuthenticationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link AuthenticationException} was thrown.
   * @param message {@link String} describing the {@link AuthenticationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AuthenticationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.security.AuthenticationException
   */
  public static AuthenticationException newAuthenticationException(Throwable cause, String message, Object... args) {
    return new AuthenticationException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link AuthorizationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link AuthorizationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AuthorizationException} with the given {@link String message}.
   * @see #newAuthorizationException(Throwable, String, Object...)
   * @see org.cp.elements.security.AuthorizationException
   */
  public static AuthorizationException newAuthorizationException(String message, Object... args) {
    return newAuthorizationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link AuthorizationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link AuthorizationException} was thrown.
   * @param message {@link String} describing the {@link AuthorizationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link AuthorizationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.security.AuthorizationException
   */
  public static AuthorizationException newAuthorizationException(Throwable cause, String message, Object... args) {
    return new AuthorizationException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link SecurityException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link SecurityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SecurityException} with the given {@link String message}.
   * @see #newSecurityException(Throwable, String, Object...)
   * @see org.cp.elements.security.SecurityException
   */
  public static SecurityException newSecurityException(String message, Object... args) {
    return newSecurityException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link SecurityException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link SecurityException} was thrown.
   * @param message {@link String} describing the {@link SecurityException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SecurityException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.security.SecurityException
   */
  public static SecurityException newSecurityException(Throwable cause, String message, Object... args) {
    return new SecurityException(format(message, args), cause);
  }

  // package org.cp.elements.service

  /**
   * Constructs and initializes a new {@link ServiceInvocationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ServiceInvocationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ServiceInvocationException} with the given {@link String message}.
   * @see #newServiceInvocationException(Throwable, String, Object...)
   * @see org.cp.elements.service.ServiceInvocationException
   */
  public static ServiceInvocationException newServiceInvocationException(String message, Object... args) {
    return newServiceInvocationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ServiceInvocationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ServiceInvocationException} was thrown.
   * @param message {@link String} describing the {@link ServiceInvocationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ServiceInvocationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.service.ServiceInvocationException
   */
  public static ServiceInvocationException newServiceInvocationException(Throwable cause,
      String message, Object... args) {

    return new ServiceInvocationException(format(message, args), cause);
  }

  // package org.cp.elements.text

  /**
   * Constructs and initializes a new {@link FormatException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link FormatException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link FormatException} with the given {@link String message}.
   * @see #newFormatException(Throwable, String, Object...)
   * @see org.cp.elements.text.FormatException
   */
  public static FormatException newFormatException(String message, Object... args) {
    return newFormatException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link FormatException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link FormatException} was thrown.
   * @param message {@link String} describing the {@link FormatException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link FormatException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.text.FormatException
   */
  public static FormatException newFormatException(Throwable cause, String message, Object... args) {
    return new FormatException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ParseException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ParseException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ParseException} with the given {@link String message}.
   * @see #newParseException(Throwable, String, Object...)
   * @see org.cp.elements.text.ParseException
   */
  public static ParseException newParseException(String message, Object... args) {
    return newParseException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ParseException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ParseException} was thrown.
   * @param message {@link String} describing the {@link ParseException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ParseException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.text.ParseException
   */
  public static ParseException newParseException(Throwable cause, String message, Object... args) {
    return new ParseException(format(message, args), cause);
  }

  // package org.cp.elements.util

  /**
   * Constructs and initializes a new {@link ApplicationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ApplicationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ApplicationException} with the given {@link String message}.
   * @see #newApplicationException(Throwable, String, Object...)
   * @see org.cp.elements.util.ApplicationException
   */
  public static ApplicationException newApplicationException(String message, Object... args) {
    return newApplicationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ApplicationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ApplicationException} was thrown.
   * @param message {@link String} describing the {@link ApplicationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ApplicationException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.ApplicationException
   */
  public static ApplicationException newApplicationException(Throwable cause, String message, Object... args) {
    return new ApplicationException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ConversionException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ConversionException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ConversionException} with the given {@link String message}.
   * @see #newConversionException(Throwable, String, Object...)
   * @see org.cp.elements.data.convert.ConversionException
   */
  public static ConversionException newConversionException(String message, Object... args) {
    return newConversionException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ConversionException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ConversionException} was thrown.
   * @param message {@link String} describing the {@link ConversionException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ConversionException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.data.convert.ConversionException
   */
  public static ConversionException newConversionException(Throwable cause, String message, Object... args) {
    return new ConversionException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link ReadOnlyException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link ReadOnlyException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ReadOnlyException} with the given {@link String message}.
   * @see #newReadOnlyException(Throwable, String, Object...)
   * @see org.cp.elements.util.ReadOnlyException
   */
  public static ReadOnlyException newReadOnlyException(String message, Object... args) {
    return newReadOnlyException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link ReadOnlyException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link ReadOnlyException} was thrown.
   * @param message {@link String} describing the {@link ReadOnlyException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link ReadOnlyException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.ReadOnlyException
   */
  public static ReadOnlyException newReadOnlyException(Throwable cause, String message, Object... args) {
    return new ReadOnlyException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link SearchException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link SearchException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SearchException} with the given {@link String message}.
   * @see #newSearchException(Throwable, String, Object...)
   * @see org.cp.elements.util.search.SearchException
   */
  public static SearchException newSearchException(String message, Object... args) {
    return newSearchException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link SearchException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link SearchException} was thrown.
   * @param message {@link String} describing the {@link SearchException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SearchException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.search.SearchException
   */
  public static SearchException newSearchException(Throwable cause, String message, Object... args) {
    return new SearchException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link SystemException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link SystemException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SystemException} with the given {@link String message}.
   * @see #newSystemException(Throwable, String, Object...)
   * @see org.cp.elements.util.SystemException
   */
  public static SystemException newSystemException(String message, Object... args) {
    return newSystemException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link SystemException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link SystemException} was thrown.
   * @param message {@link String} describing the {@link SystemException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link SystemException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.SystemException
   */
  public static SystemException newSystemException(Throwable cause, String message, Object... args) {
    return new SystemException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link UserException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link UserException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UserException} with the given {@link String message}.
   * @see #newUserException(Throwable, String, Object...)
   * @see org.cp.elements.util.UserException
   */
  public static UserException newUserException(String message, Object... args) {
    return newUserException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link UserException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link UserException} was thrown.
   * @param message {@link String} describing the {@link UserException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UserException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.UserException
   */
  public static UserException newUserException(Throwable cause, String message, Object... args) {
    return new UserException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link WriteOnlyException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link WriteOnlyException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link WriteOnlyException} with the given {@link String message}.
   * @see #newWriteOnlyException(Throwable, String, Object...)
   * @see org.cp.elements.util.WriteOnlyException
   */
  public static WriteOnlyException newWriteOnlyException(String message, Object... args) {
    return newWriteOnlyException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link WriteOnlyException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link WriteOnlyException} was thrown.
   * @param message {@link String} describing the {@link WriteOnlyException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link WriteOnlyException} with the given {@link Throwable cause} and {@link String message}.
   * @see org.cp.elements.util.WriteOnlyException
   */
  public static WriteOnlyException newWriteOnlyException(Throwable cause, String message, Object... args) {
    return new WriteOnlyException(format(message, args), cause);
  }
}
