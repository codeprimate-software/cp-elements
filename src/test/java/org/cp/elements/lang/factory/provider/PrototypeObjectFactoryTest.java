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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.Map;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Test;

/**
 * The PrototypeObjectFactoryTest class is a test suite of test cases testing the contract and functionality of the
 * PrototypeObjectFactory class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.factory.provider.PrototypeObjectFactory
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactoryTest extends AbstractMockingTestSuite {

  @After
  public void tearDown() {
    ObjectFactoryReferenceHolder.clear();
    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test
  public void testConstructionAndReference() {
    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertNotNull(objectFactory);
    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(objectFactory, ObjectFactoryReferenceHolder.get());
  }

  @Test
  public void testConstructionAndNoReference() {
    ObjectFactory mockObjectFactory = mockContext.mock(ObjectFactory.class);
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
  public void testConfigure() {
    final Configuration mockConfiguration = mockContext.mock(Configuration.class);
    final Configurable mockObject = mockContext.mock(Configurable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockObject).configure(with(same(mockConfiguration)));
    }});

    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertTrue(objectFactory.isConfigurationAvailable());
    assertSame(mockConfiguration, objectFactory.getConfiguration());
    assertSame(mockObject, objectFactory.configure(mockObject));
  }

  @Test
  public void testConfigureWithAvailableConfigurationAndNonConfigurableObject() {
    Configuration mockConfiguration = mockContext.mock(Configuration.class);
    Object bean = new Object();
    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertTrue(objectFactory.isConfigurationAvailable());
    assertSame(mockConfiguration, objectFactory.getConfiguration());
    assertSame(bean, objectFactory.configure(bean));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testConfigureWithConfigurableObjectAndUnavailableConfiguration() {
    final Configurable mockObject = mockContext.mock(Configurable.class);
    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    mockContext.checking(new Expectations() {{
      never(mockObject).configure(with(any(Configuration.class)));
    }});

    assertFalse(objectFactory.isConfigurationAvailable());
    assertSame(mockObject, objectFactory.configure(mockObject));
  }

  @Test
  public void testConfigureWithNonConfigurableObjectAndUnavailableConfiguration() {
    Object bean = new Object();
    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    assertFalse(objectFactory.isConfigurationAvailable());
    assertSame(bean, objectFactory.configure(bean));
  }

  @Test
  public void testInitializeWithNamedParameters() {
    final Map parameters = Collections.emptyMap();
    final ParameterizedInitable mockObject = mockContext.mock(ParameterizedInitable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockObject).init(with(same(parameters)));
    }});

    PrototypeObjectFactory objectFactory = new PrototypeObjectFactory();

    assertSame(mockObject, objectFactory.initialize(mockObject, parameters));
  }

  @Test
  public void testInitializeWithArguments() {
    final Object[] arguments = new Object[0];
    final ParameterizedInitable mockObject = mockContext.mock(ParameterizedInitable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockObject).init(with(same(arguments)));
    }});

    assertSame(mockObject, new PrototypeObjectFactory().initialize(mockObject, arguments));
  }

  @Test
  public void testInitializeWithNoParametersOrArguments() {
    final Initable mockObject = mockContext.mock(Initable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockObject).init();
    }});

    assertSame(mockObject, new PrototypeObjectFactory().initialize(mockObject));
  }

  @Test
  public void testInitialize() {
    Object bean = new Object();

    assertSame(bean, new PrototypeObjectFactory().initialize(bean, Collections.emptyMap()));
  }

  @Test
  public void testPostConstruct() {
    final ConfigurableInitable mockObject = mockContext.mock(ConfigurableInitable.class);
    final Configuration mockConfiguration = mockContext.mock(Configuration.class);
    final Object[] arguments = new Object[0];

    mockContext.checking(new Expectations() {{
      oneOf(mockObject).configure(with(same(mockConfiguration)));
      oneOf(mockObject).init(with(same(arguments)));
    }});

    TestPrototypeObjectFactory objectFactory = new TestPrototypeObjectFactory();

    objectFactory.setConfiguration(mockConfiguration);

    assertTrue(objectFactory.isConfigurationAvailable());
    assertSame(mockConfiguration, objectFactory.getConfiguration());
    assertSame(mockObject, objectFactory.postConstruct(mockObject, arguments));
  }

  protected interface ConfigurableInitable extends Configurable<Configuration>, ParameterizedInitable {
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
