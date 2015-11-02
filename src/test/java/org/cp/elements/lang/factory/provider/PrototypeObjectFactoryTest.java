/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.factory.provider;

import static org.junit.Assert.*;

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
 * <p/>
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
