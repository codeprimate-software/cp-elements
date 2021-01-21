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
package org.cp.elements.lang.factory;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Constructor;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.test.TestUtils;
import org.cp.elements.time.DateTimeUtils;
import org.junit.After;
import org.junit.Test;

/**
 * Unit Tests for {@link AbstractObjectFactory}.
 *
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractObjectFactoryTests {

  private TestObjectFactory objectFactory = new TestObjectFactory();

  @After
  public void tearDown() {

    objectFactory.setConfiguration(null);
    objectFactory.setConversionService(null);
    objectFactory.init();

    assertThat(objectFactory.isInitialized()).isTrue();
  }

  @Test
  public void getArgumentTypes() {

    Class<?>[] expectedArgumentTypes = { Boolean.class, Character.class, Double.class, Integer.class, String.class };
    Class<?>[] actualArgumentTypes = objectFactory.getArgumentTypes(Boolean.TRUE, 'X', Math.PI, 2, "test");

    assertThat(actualArgumentTypes).isNotNull();
    assertThat(actualArgumentTypes).isEqualTo(expectedArgumentTypes);
  }

  @Test
  public void getArgumentTypesForEmptyArguments() {

    Class<?>[] actualArgumentTypes = objectFactory.getArgumentTypes();

    assertThat(actualArgumentTypes).isNotNull();
    assertThat(actualArgumentTypes.length).isEqualTo(0);
  }

  @Test
  public void getArgumentTypesWithNullArguments() {

    Class<?>[] expectedArgumentTypes = { Boolean.class, Object.class, Double.class, Integer.class, Object.class };
    Class<?>[] actualArgumentTypes = objectFactory.getArgumentTypes(Boolean.FALSE, null, Math.PI, 2, null);

    assertThat(actualArgumentTypes).isNotNull();
    assertThat(actualArgumentTypes).isEqualTo(expectedArgumentTypes);
  }

  @Test
  public void setAndGetConfiguration() {

    Configuration mockConfiguration = mock(Configuration.class);

    assertThat(objectFactory.isConfigurationAvailable()).isFalse();

    objectFactory.setConfiguration(mockConfiguration);

    assertThat(objectFactory.isConfigurationAvailable()).isTrue();
    assertThat(objectFactory.getConfiguration()).isSameAs(mockConfiguration);

    objectFactory.setConfiguration(null);

    assertThat(objectFactory.isConfigurationAvailable()).isFalse();
  }

  @Test(expected = IllegalStateException.class)
  public void getUninitializedConfiguration() {

    TestUtils.doIllegalStateExceptionThrowingOperation(() -> objectFactory.getConfiguration(),
      () -> "The Configuration was not properly initialized");
  }

  @Test
  public void setAndGetConversionService() {

    ConversionService mockConversionService = mock(ConversionService.class);

    assertThat(objectFactory.isConversionServiceAvailable()).isFalse();

    objectFactory.setConversionService(mockConversionService);

    assertThat(objectFactory.isConversionServiceAvailable()).isTrue();
    assertThat(objectFactory.getConversionService()).isSameAs(mockConversionService);

    objectFactory.setConversionService(null);

    assertThat(objectFactory.isConversionServiceAvailable()).isFalse();
  }

  @Test(expected = IllegalStateException.class)
  public void getUninitializedConversionService() {

    TestUtils.doIllegalStateExceptionThrowingOperation(() -> objectFactory.getConversionService(),
      () -> "The ConversionService was not properly initialized");
  }

  @Test
  public void resolveConstructor() {

    Constructor<?> idNameDateTimeConstructor = objectFactory.resolveConstructor(TestDomainObject.class,
      Long.class, String.class, Calendar.class);

    assertThat(idNameDateTimeConstructor).isNotNull();
    assertThat(idNameDateTimeConstructor.getName()).isEqualTo(TestDomainObject.class.getName());

    Class<?>[] parameterTypes = idNameDateTimeConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(3);
    assertThat(parameterTypes[0]).isEqualTo(Long.class);
    assertThat(parameterTypes[1]).isEqualTo(String.class);
    assertThat(parameterTypes[2]).isEqualTo(Calendar.class);
  }

  @Test
  public void resolveCompatibleConstructor() {

    Constructor<?> numberConstructor = objectFactory.resolveConstructor(TestDomainObject.class, Long.class);

    assertThat(numberConstructor).isNotNull();
    assertThat(numberConstructor.getName()).isEqualTo(TestDomainObject.class.getName());

    Class<?>[] parameterTypes = numberConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(1);
    assertThat(parameterTypes[0]).isEqualTo(Number.class);
  }

  @Test
  public void resolveDefaultConstructor() {

    Constructor<?> defaultConstructor = objectFactory.resolveConstructor(TestDomainObjectExtension.class, Integer.class);

    assertThat(defaultConstructor).isNotNull();
    assertThat(defaultConstructor.getName()).isEqualTo(TestDomainObjectExtension.class.getName());

    Class<?>[] parameterTypes = defaultConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(0);
  }

  @Test(expected = NoSuchConstructorException.class)
  public void resolveConstructorWithNoSuchConstructor() {

    try {
      objectFactory.resolveConstructor(TestDomainObject.class, String.class);
    }
    catch (NoSuchConstructorException expected) {

      assertThat(expected).hasMessage("Failed to find a constructor with signature ([]) in Class (%s)",
        TestDomainObject.class.getName());

      assertThat(expected).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = NoSuchConstructorException.class)
  public void resolveDefaultConstructorWithNoSuchConstructor() {

    try {
      objectFactory.resolveConstructor(TestDomainObject.class);
    }
    catch (NoSuchConstructorException expected) {

      assertThat(expected).hasMessage("Failed to find a constructor with signature ([]) in Class (%s)",
        TestDomainObject.class.getName());

      assertThat(expected).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void createUsingObjectTypeName() {

    TestDomainObject domainObject = objectFactory.create(TestDomainObject.class.getName(), 123L);

    assertThat(domainObject).isNotNull();
    assertThat(domainObject.getId().longValue()).isEqualTo(123);
    assertThat(domainObject.getDateTime()).isNull();
    assertThat(domainObject.getName()).isNull();

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertThat(objectFactory.postConstructCalled()).isTrue();
    assertThat(postConstructArguments).isNotNull();
    assertThat(postConstructArguments.length).isEqualTo(0);
  }

  @Test
  public void createUsingObjectTypeNameNoArguments() {

    TestDomainObjectExtension domainObjectExtension =
      objectFactory.create(TestDomainObjectExtension.class.getName(), new Class[0], new Object[0]);

    assertThat(domainObjectExtension).isNotNull();
    assertThat(domainObjectExtension.getDateTime()).isNull();
    assertThat(domainObjectExtension.getId()).isNull();
    assertThat(domainObjectExtension.getName()).isNull();

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertThat(objectFactory.postConstructCalled()).isTrue();
    assertThat(postConstructArguments).isNotNull();
    assertThat(postConstructArguments.length).isEqualTo(0);
  }

  @Test
  public void createUsingObjectType() {

    TestDomainObjectExtension domainObjectExtension =
      objectFactory.create(TestDomainObjectExtension.class, 123L, "test");

    assertThat(domainObjectExtension).isNotNull();
    assertThat(domainObjectExtension.getDateTime()).isNull();
    assertThat(domainObjectExtension.getId()).isNull();
    assertThat(domainObjectExtension.getName()).isNull();

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertThat(objectFactory.postConstructCalled()).isTrue();
    assertThat(postConstructArguments).isNotNull();
    assertThat(postConstructArguments.length).isEqualTo(2);
    assertThat(postConstructArguments[0]).isEqualTo(123L);
    assertThat(postConstructArguments[1]).isEqualTo("test");
  }

  @Test
  public void createUsingObjectTypeWithCompatibleArguments() {

    Calendar expectedDateTime = Calendar.getInstance();

    TestDomainObject domainObject = objectFactory.create(TestDomainObject.class,
      new Class[] { Long.class, String.class, Calendar.class }, 123L, "test", expectedDateTime);

    assertThat(domainObject).isNotNull();
    assertThat(domainObject.getDateTime()).isEqualTo(expectedDateTime);
    assertThat(domainObject.getId().longValue()).isEqualTo(123L);
    assertThat(domainObject.getName()).isEqualTo("test");

    Object[] postConstructArguments = objectFactory.getPostConstructArguments();

    assertThat(objectFactory.postConstructCalled()).isTrue();
    assertThat(postConstructArguments).isNotNull();
    assertThat(postConstructArguments.length).isEqualTo(0);
  }

  @Test(expected = ObjectInstantiationException.class)
  public void createThrowsObjectInstantiationException() {

    try {
      objectFactory.create(TestDomainObject.class, "test");
    }
    catch (ObjectInstantiationException expected) {

      assertThat(expected.getMessage()).isEqualTo(String.format(
        "Failed to instantiate and instance of class (%1$s) with constructor having signature (%2$s) using arguments (%3$s)!",
        TestDomainObject.class.getName(), "[class java.lang.String]", "[test]"));
      assertThat(expected.getCause() instanceof NoSuchConstructorException).isTrue();
      assertThat(expected.getCause().getMessage()).isEqualTo(
        String.format("Failed to find a constructor with signature (%1$s) in Class (%2$s)", "[]",
          TestDomainObject.class.getName()));

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

      if (this == obj) {
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
      return dateTime != null ? DATE_FORMAT.format(dateTime) : null;
    }

    @Override
    public String toString() {
      return String.format("{ type = %1$s, dateTime = %2$s, id = %3$s, name = %4$s }", getClass(),
        toString(getDateTime()), getId(), getName());
    }
  }

  public static class TestDomainObjectExtension extends TestDomainObject {

    public TestDomainObjectExtension() { }

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
