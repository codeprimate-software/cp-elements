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

package org.cp.elements.lang.factory.provider;

import static org.hamcrest.Matchers.any;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;
import org.junit.After;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.internal.matchers.VarargMatcher;

/**
 * Test suite of test cases testing the contract and functionality of the {@link PrototypeObjectFactory} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.provider.PrototypeObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactoryTests {

  protected Object[] equalVarargs(Object... varargs) {
    return argThat(new CustomVarargsMatcher(varargs));
  }

  @After
  public void tearDown() {
    ObjectFactoryReferenceHolder.clear();
    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test
  public void constructionAndReference() {
    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertNotNull(objectFactory);
    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(objectFactory, ObjectFactoryReferenceHolder.get());
  }

  @Test
  public void constructionAndNoReference() {
    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertNotNull(objectFactory);
    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertNotSame(objectFactory, ObjectFactoryReferenceHolder.get());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configure() {
    Configuration mockConfiguration = mock(Configuration.class);
    Configurable mockObject = mock(Configurable.class);

    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable(), is(true));
    assertThat(objectFactory.getConfiguration(), is(sameInstance(mockConfiguration)));
    assertThat(objectFactory.configure(mockObject), is(sameInstance(mockObject)));

    verify(mockObject, times(1)).configure(same(mockConfiguration));
  }

  @Test
  public void configureWithAvailableConfigurationAndNonConfigurableObject() {
    Configuration mockConfiguration = mock(Configuration.class);

    Object bean = new Object();

    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertTrue(objectFactory.isConfigurationAvailable());
    assertSame(mockConfiguration, objectFactory.getConfiguration());
    assertSame(bean, objectFactory.configure(bean));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configureWithConfigurableObjectAndUnavailableConfiguration() {
    Configurable mockConfigurable = mock(Configurable.class);
    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    assertThat(objectFactory.isConfigurationAvailable(), is(false));
    assertThat(objectFactory.configure(mockConfigurable), is(sameInstance(mockConfigurable)));

    verify(mockConfigurable, never()).configure(any(Configuration.class));
  }

  @Test
  public void configureWithNonConfigurableObjectAndUnavailableConfiguration() {
    Object bean = new Object();
    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    assertFalse(objectFactory.isConfigurationAvailable());
    assertSame(bean, objectFactory.configure(bean));
  }

  @Test
  public void initializeWithArguments() {
    Object[] arguments = new Object[0];
    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    assertThat(new PrototypeObjectFactory().initialize(mockParameterizedInitable, arguments),
      is(sameInstance(mockParameterizedInitable)));

    verify(mockParameterizedInitable, times(1)).init(equalVarargs(arguments));
  }

  @Test
  public void initializeWithNamedParameters() {
    Map parameters = Collections.emptyMap();
    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.initialize(mockParameterizedInitable, parameters),
      is(sameInstance(mockParameterizedInitable)));

    verify(mockParameterizedInitable, times(1)).init(same(parameters));
  }

  @Test
  public void initializeWithNoParametersOrArguments() {
    Initable mockInitable = mock(Initable.class);

    assertThat(new PrototypeObjectFactory().initialize(mockInitable), is(sameInstance(mockInitable)));

    verify(mockInitable, times(1)).init();
  }

  @Test
  public void initialize() {
    Object bean = new Object();

    assertThat(new PrototypeObjectFactory().initialize(bean, Collections.emptyMap()), is(sameInstance(bean)));
  }

  @Test
  public void postConstruct() {
    ConfigurableInitable mockConfigurableInitable = mock(ConfigurableInitable.class);
    Configuration mockConfiguration = mock(Configuration.class);
    Object[] arguments = new Object[0];

    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable(), is(true));
    assertThat(objectFactory.getConfiguration(), is(sameInstance(mockConfiguration)));
    assertThat(objectFactory.postConstruct(mockConfigurableInitable, arguments),
      is(sameInstance(mockConfigurableInitable)));

    verify(mockConfigurableInitable, times(1)).configure(same(mockConfiguration));
    verify(mockConfigurableInitable, times(1)).init(equalVarargs(arguments));
  }

  protected interface ConfigurableInitable extends Configurable<Configuration>, ParameterizedInitable {
  }

  protected static class CustomVarargsMatcher implements ArgumentMatcher<Object[]>, VarargMatcher {

    private final Object[] expectedVarargs;

    protected CustomVarargsMatcher(Object... expectedVarargs) {
      this.expectedVarargs = expectedVarargs;
    }

    @Override
    public boolean matches(Object[] actualVarargs) {
      return Arrays.equals(expectedVarargs, actualVarargs);
    }
  }

  protected static final class TestPrototypeObjectFactory extends PrototypeObjectFactory {

    @Override
    protected boolean isConfigurationAvailable() {
      return super.isConfigurationAvailable();
    }

    @Override
    protected Configuration getConfiguration() {
      return super.getConfiguration();
    }
  }
}
