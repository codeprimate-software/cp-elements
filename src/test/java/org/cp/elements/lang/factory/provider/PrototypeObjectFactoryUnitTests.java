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
package org.cp.elements.lang.factory.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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
 * Unit Tests for {@link PrototypeObjectFactory}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.provider.PrototypeObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactoryUnitTests {

  private Object[] equalVarargs(Object... varargs) {
    return argThat(new CustomVarargsMatcher(varargs));
  }

  @After
  public void tearDown() {

    ObjectFactoryReferenceHolder.clear();

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();
  }

  @Test
  public void constructionWithReference() {

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory).isNotNull();
    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(objectFactory);
  }

  @Test
  public void constructionWithNoReference() {

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);

    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory).isNotNull();
    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isNotSameAs(objectFactory);
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configure() {

    Configurable<Configuration> mockObject = mock(Configurable.class);
    Configuration mockConfiguration = mock(Configuration.class);

    TestObjectFactory objectFactory = new TestObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable()).isTrue();
    assertThat(objectFactory.getConfiguration()).isSameAs(mockConfiguration);
    assertThat(objectFactory.configure(mockObject)).isSameAs(mockObject);

    verify(mockObject, times(1)).configure(same(mockConfiguration));
    verifyNoMoreInteractions(mockObject);
    verifyNoInteractions(mockConfiguration);
  }

  @Test
  public void configureWithAvailableConfigurationAndNonConfigurableObject() {

    Configuration mockConfiguration = mock(Configuration.class);

    Object bean = new Object();

    TestObjectFactory objectFactory = new TestObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable()).isTrue();
    assertThat(objectFactory.getConfiguration()).isSameAs(mockConfiguration);
    assertThat(objectFactory.configure(bean)).isSameAs(bean);
    verifyNoInteractions(mockConfiguration);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configureWithConfigurableObjectAndUnavailableConfiguration() {

    Configurable<Configuration> mockConfigurable = mock(Configurable.class);

    TestObjectFactory objectFactory = new TestObjectFactory();

    assertThat(objectFactory.isConfigurationAvailable()).isFalse();
    assertThat(objectFactory.configure(mockConfigurable)).isSameAs(mockConfigurable);

    verify(mockConfigurable, never()).configure(any(Configuration.class));
  }

  @Test
  public void configureWithNonConfigurableObjectAndUnavailableConfiguration() {

    Object bean = new Object();

    TestObjectFactory objectFactory = new TestObjectFactory();

    assertThat(objectFactory.isConfigurationAvailable()).isFalse();
    assertThat(objectFactory.configure(bean)).isSameAs(bean);
  }

  @Test
  public void configureWithNullIsNullSafe() {

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.<Object>configure(null)).isNull();
  }

  @Test
  public void initializeWithArguments() {

    Object[] arguments = new Object[0];

    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.initialize(mockParameterizedInitable, arguments)).isSameAs(mockParameterizedInitable);

    verify(mockParameterizedInitable, times(1)).init(equalVarargs(arguments));
    verifyNoMoreInteractions(mockParameterizedInitable);
  }

  @Test
  public void initializeWithNamedParameters() {

    Map<?, ?> parameters = Collections.emptyMap();

    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.initialize(mockParameterizedInitable, parameters)).isSameAs(mockParameterizedInitable);

    verify(mockParameterizedInitable, times(1)).init(same(parameters));
    verifyNoMoreInteractions(mockParameterizedInitable);
  }

  @Test
  public void initializeWithNoParametersOrArguments() {

    Initable mockInitable = mock(Initable.class);

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.initialize(mockInitable)).isSameAs(mockInitable);

    verify(mockInitable, times(1)).init();
    verifyNoMoreInteractions(mockInitable);
  }

  @Test
  public void initializeWithNullIsNullSafe() {

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.<Object>initialize(null, Collections.singletonMap("test", 1))).isNull();
  }

  @Test
  public void initialize() {

    Object bean = new Object();

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertThat(objectFactory.initialize(bean, Collections.emptyMap())).isSameAs(bean);
  }

  @Test
  public void postConstruct() {

    ConfigurableInitable mockConfigurableInitable = mock(ConfigurableInitable.class);

    Configuration mockConfiguration = mock(Configuration.class);

    Object[] arguments = new Object[0];

    TestObjectFactory objectFactory = new TestObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable()).isTrue();
    assertThat(objectFactory.getConfiguration()).isSameAs(mockConfiguration);
    assertThat(objectFactory.postConstruct(mockConfigurableInitable, arguments)).isSameAs(mockConfigurableInitable);

    verify(mockConfigurableInitable, times(1)).configure(same(mockConfiguration));
    verify(mockConfigurableInitable, times(1)).init(equalVarargs(arguments));
    verifyNoMoreInteractions(mockConfigurableInitable);
    verifyNoInteractions(mockConfiguration);
  }

  interface ConfigurableInitable extends Configurable<Configuration>, ParameterizedInitable { }

  static class CustomVarargsMatcher implements ArgumentMatcher<Object[]>, VarargMatcher {

    private final Object[] expectedVarargs;

    protected CustomVarargsMatcher(Object... expectedVarargs) {
      this.expectedVarargs = expectedVarargs;
    }

    @Override
    public boolean matches(Object[] actualVarargs) {
      return Arrays.equals(this.expectedVarargs, actualVarargs);
    }
  }

  static final class TestObjectFactory extends PrototypeObjectFactory {

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
