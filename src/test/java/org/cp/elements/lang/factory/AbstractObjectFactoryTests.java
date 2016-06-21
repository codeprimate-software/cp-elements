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

package org.cp.elements.lang.factory;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Constructor;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.DateTimeUtils;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.convert.ConversionService;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link AbstractObjectFactory} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see org.cp.elements.util.convert.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractObjectFactoryTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  private TestObjectFactory objectFactory = new TestObjectFactory();

  @After
  public void tearDown() {
    objectFactory.setConfiguration(null);
    objectFactory.setConversionService(null);
    objectFactory.init();

    assertThat(objectFactory.isInitialized(), is(true));
  }

  @Test
  public void getArgumentTypes() {
    Class[] expectedArgumentTypes = { Boolean.class, Character.class, Double.class, Integer.class, String.class };
    Class[] actualArgumentTypes = objectFactory.getArgumentTypes(Boolean.TRUE, 'X', Math.PI, 2, "test");

    assertThat(actualArgumentTypes, is(notNullValue(Class[].class)));
    assertThat(actualArgumentTypes, is(equalTo(expectedArgumentTypes)));
  }

  @Test
  public void getArgumentTypesForEmptyArguments() {
    Class[] actualArgumentTypes = objectFactory.getArgumentTypes();

    assertThat(actualArgumentTypes, is(notNullValue(Class[].class)));
    assertThat(actualArgumentTypes.length, is(equalTo(0)));
  }

  @Test
  public void getArgumentTypesWithNullArguments() {
    Class[] expectedArgumentTypes = { Boolean.class, Object.class, Double.class, Integer.class, Object.class };
    Class[] actualArgumentTypes = objectFactory.getArgumentTypes(Boolean.FALSE, null, Math.PI, 2, null);

    assertThat(actualArgumentTypes, is(notNullValue(Class[].class)));
    assertThat(actualArgumentTypes, is(equalTo(expectedArgumentTypes)));
  }

  @Test
  public void setAndGetConfiguration() {
    Configuration mockConfiguration = mock(Configuration.class);

    assertThat(objectFactory.isConfigurationAvailable(), is(false));

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable(), is(true));
    assertThat(objectFactory.getConfiguration(), is(sameInstance(mockConfiguration)));

    objectFactory.setConfiguration(null);

    assertThat(objectFactory.isConfigurationAvailable(), is(false));
  }

  @Test
  public void getUninitializedConfiguration() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The Configuration was not properly initialized");

    objectFactory.getConfiguration();
  }

  @Test
  public void setAndGetConversionService() {
    ConversionService mockConversionService = mock(ConversionService.class);

    assertThat(objectFactory.isConversionServiceAvailable(), is(false));

    objectFactory.setConversionService(mockConversionService);

    assertThat(objectFactory.isConversionServiceAvailable(), is(true));
    assertThat(objectFactory.getConversionService(), is(sameInstance(mockConversionService)));

    objectFactory.setConversionService(null);

    assertThat(objectFactory.isConversionServiceAvailable(), is(false));
  }

  @Test
  public void getUninitializedConversionService() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The ConversionService was not properly initialized");

    objectFactory.getConversionService();
  }

  @Test
  public void resolveConstructor() {
    Constructor idNameDateTimeConstructor = objectFactory.resolveConstructor(TestDomainObject.class,
      Long.class, String.class, Calendar.class);

    assertNotNull(idNameDateTimeConstructor);
    assertEquals(TestDomainObject.class.getName(), idNameDateTimeConstructor.getName());

    Class[] parameterTypes = idNameDateTimeConstructor.getParameterTypes();

    assertNotNull(parameterTypes);
    assertEquals(3, parameterTypes.length);
    assertEquals(Long.class, parameterTypes[0]);
    assertEquals(String.class, parameterTypes[1]);
    assertEquals(Calendar.class, parameterTypes[2]);
  }

  @Test
  public void resolveCompatibleConstructor() {
    Constructor numberConstructor = objectFactory.resolveConstructor(TestDomainObject.class, Long.class);

    assertNotNull(numberConstructor);
    assertEquals(TestDomainObject.class.getName(), numberConstructor.getName());

    Class[] parameterTypes = numberConstructor.getParameterTypes();

    assertNotNull(parameterTypes);
    assertEquals(1, parameterTypes.length);
    assertEquals(Number.class, parameterTypes[0]);
  }

  @Test
  public void resolveDefaultConstructor() {
    Constructor defaultConstructor = objectFactory.resolveConstructor(TestDomainObjectExtension.class, Integer.class);

    assertNotNull(defaultConstructor);
    assertEquals(TestDomainObjectExtension.class.getName(), defaultConstructor.getName());

    Class[] parameterTypes = defaultConstructor.getParameterTypes();

    assertNotNull(parameterTypes);
    assertEquals(0, parameterTypes.length);
  }

  @Test
  public void resolveConstructorWithNoSuchConstructor() {
    exception.expect(NoSuchConstructorException.class);
    exception.expectCause(is(instanceOf(NoSuchMethodException.class)));
    exception.expectMessage(String.format("Failed to find a constructor with signature ([]) in Class (%1$s)",
      TestDomainObject.class.getName()));

    objectFactory.resolveConstructor(TestDomainObject.class, String.class);
  }

  @Test
  public void resolveDefaultConstructorWithNoSuchConstructor() {
    exception.expect(NoSuchConstructorException.class);
    exception.expectCause(is(instanceOf(NoSuchMethodException.class)));
    exception.expectMessage(String.format("Failed to find a constructor with signature ([]) in Class (%1$s)",
      TestDomainObject.class.getName()));

    objectFactory.resolveConstructor(TestDomainObject.class);
  }

  @Test
  public void createUsingObjectTypeName() {
    TestDomainObject domainObject = objectFactory.create(TestDomainObject.class.getName(), 123l);

    assertNotNull(domainObject);
    assertEquals(123, domainObject.getId().longValue());
    assertNull(domainObject.getDateTime());
    assertNull(domainObject.getName());

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertTrue(objectFactory.postConstructCalled());
    assertNotNull(postConstructArguments);
    assertEquals(0, postConstructArguments.length);
  }

  @Test
  public void createUsingObjectTypeNameNoArguments() {
    TestDomainObjectExtension domainObjectExtension = objectFactory.create(TestDomainObjectExtension.class.getName(),
      new Class[0], new Object[0]);

    assertNotNull(domainObjectExtension);
    assertNull(domainObjectExtension.getDateTime());
    assertNull(domainObjectExtension.getId());
    assertNull(domainObjectExtension.getName());

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertTrue(objectFactory.postConstructCalled());
    assertNotNull(postConstructArguments);
    assertEquals(0, postConstructArguments.length);
  }

  @Test
  public void createUsingObjectType() {
    TestDomainObjectExtension domainObjectExtension = objectFactory.create(TestDomainObjectExtension.class, 123l, "test");

    assertNotNull(domainObjectExtension);
    assertNull(domainObjectExtension.getDateTime());
    assertNull(domainObjectExtension.getId());
    assertNull(domainObjectExtension.getName());

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertTrue(objectFactory.postConstructCalled());
    assertNotNull(postConstructArguments);
    assertEquals(2, postConstructArguments.length);
    assertEquals(123l, postConstructArguments[0]);
    assertEquals("test", postConstructArguments[1]);
  }

  @Test
  public void createUsingObjectTypeWithCompatibleArguments() {
    Calendar expectedDateTime = Calendar.getInstance();
    TestDomainObject domainObject = objectFactory.create(TestDomainObject.class,
      new Class[] { Long.class, String.class, Calendar.class }, 123l, "test", expectedDateTime);

    assertNotNull(domainObject);
    assertEquals(expectedDateTime, domainObject.getDateTime());
    assertEquals(123l, domainObject.getId().longValue());
    assertEquals("test", domainObject.getName());

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertTrue(objectFactory.postConstructCalled());
    assertNotNull(postConstructArguments);
    assertEquals(0, postConstructArguments.length);
  }

  @Test(expected = ObjectInstantiationException.class)
  public void createThrowsObjectInstantiationException() {
    try {
      objectFactory.create(TestDomainObject.class, "test");
    }
    catch (ObjectInstantiationException expected) {
      assertEquals(String.format("Failed to instantiate and instance of class (%1$s) with constructor having signature (%2$s) using arguments (%3$s)!",
        TestDomainObject.class.getName(), "[class java.lang.String]", "[test]"), expected.getMessage());
      assertTrue(expected.getCause() instanceof NoSuchConstructorException);
      assertEquals(String.format("Failed to find a constructor with signature (%1$s) in Class (%2$s)", "[]",
        TestDomainObject.class.getName()), expected.getCause().getMessage());
      throw expected;
    }
  }

  public static class TestDomainObject {

    private static final DateFormat DATE_FORMAT = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss a");

    private final Calendar dateTime;
    private final Long id;
    private final String name;

    protected TestDomainObject() {
      this.dateTime = null;
      this.id = null;
      this.name = null;
    }

    private TestDomainObject(final Long id) {
      this.id = id;
      this.dateTime = null;
      this.name = null;
    }

    public TestDomainObject(final Number id) {
      this(id != null ? id.longValue() : null);
    }

    public TestDomainObject(final Long id, final String name, final Calendar dateTime) {
      this.id = id;
      this.name = name;
      this.dateTime = dateTime;
    }

    public Calendar getDateTime() {
      return DateTimeUtils.clone(dateTime);
    }

    public Long getId() {
      return id;
    }

    public String getName() {
      return name;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof TestDomainObject)) {
        return false;
      }

      TestDomainObject that = (TestDomainObject) obj;

      return ObjectUtils.equals(this.getDateTime(), this.getDateTime())
        && ObjectUtils.equals(this.getId(), that.getId())
        && ObjectUtils.equals(this.getName(), that.getName());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getDateTime());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getName());
      return hashValue;
    }

    private static String toString(final Calendar dateTime) {
      return (dateTime != null ? DATE_FORMAT.format(dateTime) : null);
    }

    @Override
    public String toString() {
      return String.format("{ type = %1$s, dateTime = %2$s, id = %3$s, name = %4$s }", getClass(),
        toString(getDateTime()), getId(), getName());
    }
  }

  public static class TestDomainObjectExtension extends TestDomainObject {

    public TestDomainObjectExtension() {
    }

    public TestDomainObjectExtension(final Long id) {
      super(id);
    }
  }

  protected static final class TestObjectFactory extends AbstractObjectFactory implements Initable {

    private boolean postConstructCalled = false;
    private Object[] postConstructArguments;

    public Object[] getPostConstructArguments() {
      return postConstructArguments;
    }

    @Override
    public boolean isInitialized() {
      return !postConstructCalled;
    }

    @Override
    public void init() {
      postConstructCalled = false;
    }

    @Override
    protected <T> T postConstruct(T object, Object... args) {
      postConstructArguments = args;
      postConstructCalled = true;
      return super.postConstruct(object, args);
    }

    public boolean postConstructCalled() {
      return postConstructCalled;
    }
  }
}
