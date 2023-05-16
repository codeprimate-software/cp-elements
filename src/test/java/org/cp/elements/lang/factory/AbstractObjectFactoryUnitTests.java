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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.Constructor;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.function.Function;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

/**
 * Unit Tests for {@link AbstractObjectFactory}.
 *
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@RunWith(MockitoJUnitRunner.class)
public class AbstractObjectFactoryUnitTests {

  @Spy
  private TestObjectFactory objectFactory = new TestObjectFactory();

  @Test
  public void setAndGetConfiguration() {

    Configuration mockConfiguration = mock(Configuration.class);

    assertThat(this.objectFactory.isConfigurationAvailable()).isFalse();

    this.objectFactory.setConfiguration(mockConfiguration);

    assertThat(this.objectFactory.isConfigurationAvailable()).isTrue();
    assertThat(this.objectFactory.getConfiguration()).isEqualTo(mockConfiguration);

    this.objectFactory.setConfiguration(null);

    assertThat(this.objectFactory.isConfigurationAvailable()).isFalse();
  }

  @Test(expected = IllegalStateException.class)
  public void getUninitializedConfiguration() {

    TestUtils.doIllegalStateExceptionThrowingOperation(this.objectFactory::getConfiguration,
      () -> String.format("A Configuration for this ObjectFactory [%s] was not properly initialized",
        this.objectFactory.getClass().getName()));
  }

  @Test
  public void setAndGetConversionService() {

    ConversionService mockConversionService = mock(ConversionService.class);

    assertThat(this.objectFactory.isConversionServiceAvailable()).isFalse();

    this.objectFactory.setConversionService(mockConversionService);

    assertThat(this.objectFactory.isConversionServiceAvailable()).isTrue();
    assertThat(this.objectFactory.getConversionService()).isEqualTo(mockConversionService);

    this.objectFactory.setConversionService(null);

    assertThat(this.objectFactory.isConversionServiceAvailable()).isFalse();
  }

  @Test(expected = IllegalStateException.class)
  public void getUninitializedConversionService() {

    TestUtils.doIllegalStateExceptionThrowingOperation(this.objectFactory::getConversionService,
      () -> String.format("The ConversionService used by this ObjectFactory [%s] was not properly initialized",
        this.objectFactory.getClass().getName()));
  }

  @Test
  public void getDefaultObjectPostProcessor() {

    Function<Object, Object> objectPostProcessor = this.objectFactory.getObjectPostProcessor();

    assertThat(objectPostProcessor).isNotNull();
    assertThat(objectPostProcessor.apply("TEST")).isEqualTo("TEST");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void registerAndGetObjectPostProcessor() {

    Function<Object, Object> mockFunctionOne = mock(Function.class);
    Function<Object, Object> mockFunctionTwo = mock(Function.class);

    Answer<Object> answer = invocation -> invocation.getArgument(0, Object.class);

    doAnswer(answer).when(mockFunctionOne).apply(any());
    doAnswer(answer).when(mockFunctionTwo).apply(any());

    assertThat(this.objectFactory.<TestObjectFactory>registerObjectPostProcessor(mockFunctionOne)
      .<TestObjectFactory>registerObjectPostProcessor(mockFunctionTwo)).isSameAs(this.objectFactory);

    Function<Object, Object> objectPostProcessor = this.objectFactory.getObjectPostProcessor();

    assertThat(objectPostProcessor).isNotNull();
    assertThat(objectPostProcessor).isNotSameAs(mockFunctionOne);
    assertThat(objectPostProcessor).isNotSameAs(mockFunctionTwo);
    assertThat(objectPostProcessor.apply("TEST")).isEqualTo("TEST");

    verify(mockFunctionOne, times(1)).apply(eq("TEST"));
    verify(mockFunctionTwo, times(1)).apply(eq("TEST"));
    verifyNoMoreInteractions(mockFunctionOne, mockFunctionTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void registerNullObjectPostProcessor() {

    try {
      this.objectFactory.registerObjectPostProcessor(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Function used to post process the object is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void resolveConstructor() {

    Constructor<?> idNameDateTimeConstructor =
      this.objectFactory.resolveConstructor(TestObject.class, Long.class, String.class, Instant.class);

    assertThat(idNameDateTimeConstructor).isNotNull();
    assertThat(idNameDateTimeConstructor.getName()).isEqualTo(TestObject.class.getName());

    Class<?>[] parameterTypes = idNameDateTimeConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(3);
    assertThat(parameterTypes[0]).isEqualTo(Long.class);
    assertThat(parameterTypes[1]).isEqualTo(String.class);
    assertThat(parameterTypes[2]).isEqualTo(Instant.class);
  }

  @Test
  public void resolveCompatibleConstructor() {

    Constructor<?> numberConstructor =
      this.objectFactory.resolveConstructor(TestObject.class, Long.class);

    assertThat(numberConstructor).isNotNull();
    assertThat(numberConstructor.getName()).isEqualTo(TestObject.class.getName());

    Class<?>[] parameterTypes = numberConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(1);
    assertThat(parameterTypes[0]).isEqualTo(Number.class);
  }

  @Test
  public void resolveDefaultConstructor() {

    Constructor<?> defaultConstructor =
      this.objectFactory.resolveConstructor(TestObjectExtension.class, Integer.class);

    assertThat(defaultConstructor).isNotNull();
    assertThat(defaultConstructor.getName()).isEqualTo(TestObjectExtension.class.getName());

    Class<?>[] parameterTypes = defaultConstructor.getParameterTypes();

    assertThat(parameterTypes).isNotNull();
    assertThat(parameterTypes.length).isEqualTo(0);
  }

  @Test(expected = NoSuchConstructorException.class)
  public void resolveConstructorWhenNoSuchConstructorExists() {

    try {
      this.objectFactory.resolveConstructor(TestObject.class, String.class);
    }
    catch (NoSuchConstructorException expected) {

      assertThat(expected).hasMessage("Failed to find a constructor with signature [%s] in Class [%s]",
        Arrays.toString(ArrayUtils.asArray(String.class)), TestObject.class.getName());

      assertThat(expected).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = NoSuchConstructorException.class)
  public void resolveDefaultConstructorWhenNoSuchConstructorExists() {

    try {
      this.objectFactory.resolveConstructor(TestObject.class);
    }
    catch (NoSuchConstructorException expected) {

      assertThat(expected).hasMessage("Failed to find a constructor with signature [[]] in Class [%s]",
        TestObject.class.getName());

      assertThat(expected).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void createUsingObjectTypeNameAndNoArguments() {

    TestObjectExtension domainObjectExtension =
      this.objectFactory.create(TestObjectExtension.class.getName());

    assertThat(domainObjectExtension).isNotNull();
    assertThat(domainObjectExtension.getDateTime()).isNull();
    assertThat(domainObjectExtension.getId()).isNull();
    assertThat(domainObjectExtension.getName()).isNull();

    verify(this.objectFactory, times(1)).postConstruct(eq(domainObjectExtension));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void createUsingObjectTypeNameWithArguments() {

    Function<Object, Object> mockFunction = mock(Function.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(mockFunction).apply(any());

    TestObject domainObject = this.objectFactory
      .registerObjectPostProcessor(mockFunction)
      .create(TestObject.class.getName(), 123);

    assertThat(domainObject).isNotNull();
    assertThat(domainObject.getId()).isEqualTo(123L);
    assertThat(domainObject.getDateTime()).isNull();
    assertThat(domainObject.getName()).isNull();

    verify(this.objectFactory, times(1)).postConstruct(eq(domainObject));
    verify(mockFunction, times(1)).apply(eq(domainObject));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void createUsingObjectType() {

    Function<Object, Object> mockFunction = mock(Function.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(mockFunction).apply(any());

    TestObjectExtension domainObjectExtension = this.objectFactory
      .registerObjectPostProcessor(mockFunction)
      .create(TestObjectExtension.class);

    assertThat(domainObjectExtension).isNotNull();
    assertThat(domainObjectExtension.getDateTime()).isNull();
    assertThat(domainObjectExtension.getId()).isNull();
    assertThat(domainObjectExtension.getName()).isNull();

    verify(this.objectFactory, times(1)).postConstruct(eq(domainObjectExtension));
    verify(mockFunction, times(1)).apply(eq(domainObjectExtension));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test
  public void createUsingObjectTypeWithArguments() {

    TestObjectExtension domainObjectExtension =
      this.objectFactory.create(TestObjectExtension.class, 123L, "test");

    assertThat(domainObjectExtension).isNotNull();
    assertThat(domainObjectExtension.getDateTime()).isNull();
    assertThat(domainObjectExtension.getId()).isNull();
    assertThat(domainObjectExtension.getName()).isNull();

    verify(this.objectFactory, times(1))
      .postConstruct(eq(domainObjectExtension), eq(123L), eq("test"));
  }

  @Test
  public void createUsingObjectTypeWithCompatibleArguments() {

    Instant expectedDateTime = Instant.now();

    TestObject domainObject = this.objectFactory.create(TestObject.class,
      new Class[] { Long.class, String.class, Instant.class }, 123L, "test", expectedDateTime);

    assertThat(domainObject).isNotNull();
    assertThat(domainObject.getDateTime()).isEqualTo(expectedDateTime);
    assertThat(domainObject.getId()).isEqualTo(123L);
    assertThat(domainObject.getName()).isEqualTo("test");

    verify(this.objectFactory, times(1)).postConstruct(eq(domainObject));
  }

  @Test(expected = ObjectInstantiationException.class)
  public void createThrowsObjectInstantiationException() {

    try {
      this.objectFactory.create(TestObject.class, "test");
    }
    catch (ObjectInstantiationException expected) {

      assertThat(expected)
        .hasMessage("Failed to instantiate an instance of class [%1$s] with constructor having signature [%2$s] using arguments [%3$s]!",
        TestObject.class.getName(), "[class java.lang.String]", "[test]");

      assertThat(expected).hasCauseInstanceOf(NoSuchConstructorException.class);

      assertThat(expected.getCause())
        .hasMessage("Failed to find a constructor with signature [%1$s] in Class [%2$s]",
          "[class java.lang.String]", TestObject.class.getName());

      assertThat(expected.getCause()).hasCauseInstanceOf(NoSuchMethodException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  public static class TestObject {

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss a");

    private static final String TO_STRING = "{ type = %1$s, dateTime = %2$s, id = %3$s, name = %4$s }";

    private final Instant dateTime;
    private final Long id;
    private final String name;

    private TestObject() {

      this.dateTime = null;
      this.id = null;
      this.name = null;
    }

    protected TestObject(Long id) {

      this.id = id;
      this.dateTime = null;
      this.name = null;
    }

    public TestObject(Number id) {
      this(id != null ? id.longValue() : null);
    }

    public TestObject(Long id, String name, Instant dateTime) {

      this.id = id;
      this.name = name;
      this.dateTime = dateTime;
    }

    public Instant getDateTime() {
      return this.dateTime;
    }

    public Long getId() {
      return this.id;
    }

    public String getName() {
      return this.name;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof TestObject)) {
        return false;
      }

      TestObject that = (TestObject) obj;

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

    private static String toString(Instant dateTime) {

      return dateTime != null
        ? dateTime.atZone(ZoneId.systemDefault()).format(DATE_TIME_FORMATTER)
        : null;
    }

    @Override
    public String toString() {
      return String.format(TO_STRING, getClass(), toString(getDateTime()), getId(), getName());
    }
  }

  public static class TestObjectExtension extends TestObject {

    public TestObjectExtension() { }

    public TestObjectExtension(String id) {
      super(id != null ? Long.parseLong(id) : null);
    }
  }

  static class TestObjectFactory extends AbstractObjectFactory { }

}
