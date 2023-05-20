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
package org.cp.elements.beans.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.BeanInfo;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Unit Tests for {@link Properties}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Properties
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
public class PropertiesUnitTests {

  private final Function<Property, Property> READABLE_PROPERTY_FUNCTION = property -> {
    doReturn(true).when(property).isReadable();
    return property;
  };

  private final Function<Property, Property> REQUIRED_PROPERTY_FUNCTION = property -> {
    doReturn(true).when(property).isRequired();
    return property;
  };

  private final Function<Property, Property> TRANSIENT_PROPERTY_FUNCTION = property -> {
    doReturn(true).when(property).isTransient();
    return property;
  };

  private final Function<Property, Property> WRITABLE_PROPERTY_FUNCTION = property -> {
    doReturn(true).when(property).isWritable();
    return property;
  };

  private void assertThatPropertiesContains(Properties properties, Property... propertyElements) {

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(propertyElements.length);
    assertThat(properties).containsExactlyInAnyOrder(propertyElements);
  }

  private void assertThatPropertiesIsEmpty(Properties properties) {

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  private Property mockProperty(String name, Class<?> type) {
    return mockProperty(name, type, Function.identity());
  }

  private Property mockProperty(String name, Class<?> type, Function<Property, Property> propertyFunction) {

    Property mockProperty = mock(Property.class, name);

    doReturn(name).when(mockProperty).getName();
    doReturn(type).when(mockProperty).getType();
    doCallRealMethod().when(mockProperty).isSerializable();
    doCallRealMethod().when(mockProperty).compareTo(any(Property.class));

    return propertyFunction.apply(mockProperty);
  }

  @Test
  public void emptyProperties() {

    Properties properties = Properties.empty();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void fromBeanModelIsCorrect() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    BeanInfo mockBeanInfo = mock(BeanInfo.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(mockBeanInfo).when(mockBeanModel).getBeanInfo();
    doReturn(ArrayUtils.asArray(mockPropertyDescriptor)).when(mockBeanInfo).getPropertyDescriptors();
    doReturn("mockProperty").when(mockPropertyDescriptor).getName();

    Properties properties = Properties.from(mockBeanModel);

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(1);

    Property property = properties.stream().findFirst().orElse(null);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isSameAs(mockBeanModel);
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);
    assertThat(property.getName()).isEqualTo("mockProperty");

    verify(mockBeanModel, times(1)).getBeanInfo();
    verify(mockBeanInfo, times(1)).getPropertyDescriptors();
    verify(mockPropertyDescriptor, atLeastOnce()).getName();
    verify(mockPropertyDescriptor, atLeastOnce()).getPropertyType();

    verifyNoMoreInteractions(mockBeanModel, mockBeanInfo, mockPropertyDescriptor);
  }

  @Test
  public void fromNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Properties.from(null))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void ofArrayOfProperties() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(2);
    assertThat(properties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void ofEmptyArray() {

    Properties properties = Properties.of();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofNullArrayIsNullSafe() {

    Properties properties = Properties.of((Property[]) null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofIterableContainingProperties() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Iterable<Property> iterable = ArrayUtils.asIterable(mockPropertyOne, mockPropertyTwo);

    Properties properties = Properties.of(iterable);

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(2);
    assertThat(properties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void ofEmptyIterable() {

    Properties properties = Properties.of(CollectionUtils.emptyIterable());

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofNullIterableIsNullSafe() {

    Properties properties = Properties.of((Iterable<Property>) null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofStreamOfProperties() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Stream<Property> stream = StreamUtils.stream(mockPropertyOne, mockPropertyTwo);

    Properties properties = Properties.of(stream);

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(2);
    assertThat(properties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void ofEmptyStream() {

    Properties properties = Properties.of(StreamUtils.empty());

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofNullStreamIsNullSafe() {

    Properties properties = Properties.of((Stream<Property>) null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void getPropertiesReturnsUnmodifiableSet() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Properties properties = Properties.of(mockPropertyOne);

    assertThat(properties).isNotNull();

    Set<Property> propertySet = properties.getProperties();

    assertThat(propertySet).isNotNull();
    assertThat(propertySet).hasSize(1);
    assertThat(propertySet).containsExactly(mockPropertyOne);

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> propertySet.add(mockPropertyTwo))
      .withNoCause();

    assertThat(propertySet).hasSize(1);
    assertThat(propertySet).containsExactly(mockPropertyOne);

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> propertySet.remove(mockPropertyOne))
      .withNoCause();

    assertThat(propertySet).hasSize(1);
    assertThat(propertySet).containsExactly(mockPropertyOne);
  }

  @Test
  public void containsProperty() {

    Property mockProperty = mock(Property.class);

    Properties properties = Properties.of(mockProperty);

    assertThat(properties).isNotNull();
    assertThat(properties).containsExactly(mockProperty);
    assertThat(properties.contains(mockProperty)).isTrue();
  }

  @Test
  public void containsMissingProperty() {

    Property mockProperty = mock(Property.class);

    Properties properties = Properties.empty();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
    assertThat(properties).doesNotContain(mockProperty);
    assertThat(properties.contains(mockProperty)).isFalse();

    verifyNoInteractions(mockProperty);
  }

  @Test
  public void containsNullPropertyIsNullSafe() {
    assertThat(Properties.empty().contains(null)).isFalse();
  }

  @Test
  public void findByPredicate() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    Predicate<Property> testPredicate = mockPropertyOne::equals;

    Properties queriedProperties = Properties.of(properties.findBy(testPredicate));

    assertThatPropertiesContains(queriedProperties, mockPropertyOne);
  }

  @Test
  public void findByNullPredicate() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    Properties queriedProperties = Properties.of(properties.findBy(null));

    assertThatPropertiesContains(queriedProperties, mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void findPropertyByName() {

    Property mockPropertyOne = mockProperty("ONE", String.class);
    Property mockPropertyTwo = mockProperty("TWO", Integer.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    Property namedProperty = properties.findByName("TWO");

    assertThat(namedProperty).isNotNull();
    assertThat(namedProperty.getName()).isEqualTo("TWO");

    verify(mockPropertyOne, atLeastOnce()).getName();
    verify(mockPropertyTwo, atLeast(2)).getName();
  }

  private void testFindPropertyByName(String name) {

    Property mockPropertyOne = mockProperty("ONE", String.class);
    Property mockPropertyTwo = mockProperty("TWO", Integer.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    assertThatExceptionOfType(PropertyNotFoundException.class)
      .isThrownBy(() -> properties.findByName(name))
      .withMessage("Property with name [%s] not found", name)
      .withNoCause();

    verify(mockPropertyOne, atLeastOnce()).getName();
    verify(mockPropertyTwo, atLeastOnce()).getName();
  }

  @Test
  public void findPropertyByCaseSensitiveName() {
    testFindPropertyByName("two");
  }

  @Test
  public void findPropertyByMisspelledName() {
    testFindPropertyByName("to");
  }

  @Test
  public void findPropertyByNonExistingName() {
    testFindPropertyByName("2");
  }

  @Test
  public void findPropertyByType() {

    Property mockPropertyOne = mockProperty("one", String.class);
    Property mockPropertyTwo = mockProperty("two", Integer.class);
    Property mockPropertyThree = mockProperty("three", Double.class);
    Property mockPropertyFour = mockProperty("four", Integer.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree, mockPropertyFour);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree, mockPropertyFour);

    Properties typedProperties = properties.findByType(Integer.class);

    assertThatPropertiesContains(typedProperties, mockPropertyTwo, mockPropertyFour);

    typedProperties = properties.findByType(String.class);

    assertThatPropertiesContains(typedProperties, mockPropertyOne);

    typedProperties = properties.findByType(Character.class);

    assertThatPropertiesIsEmpty(typedProperties);

    typedProperties = properties.findByType(Number.class);

    assertThatPropertiesContains(typedProperties, mockPropertyTwo, mockPropertyThree, mockPropertyFour);

    typedProperties = properties.findByType(Float.class);

    assertThatPropertiesIsEmpty(typedProperties);
  }

  @Test
  public void findReadableProperties() {

    Property mockPropertyOne = mockProperty("one", String.class,
      READABLE_PROPERTY_FUNCTION.andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      READABLE_PROPERTY_FUNCTION);

    Property mockPropertyThree = mockProperty("three", Boolean.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    Properties readableProperties = properties.findReadable();

    assertThatPropertiesContains(readableProperties, mockPropertyOne, mockPropertyTwo);

    readableProperties = properties.findByType(Integer.class).findReadable();

    assertThatPropertiesContains(readableProperties, mockPropertyTwo);

    readableProperties = properties.findByType(Boolean.class).findReadable();

    assertThatPropertiesIsEmpty(readableProperties);
  }

  @Test
  public void findRequiredProperties() {

    Property mockPropertyOne = mockProperty("one", String.class,
      REQUIRED_PROPERTY_FUNCTION.andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      REQUIRED_PROPERTY_FUNCTION);

    Property mockPropertyThree = mockProperty("three", Boolean.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    Properties requiredProperties = properties.findRequired();

    assertThatPropertiesContains(requiredProperties, mockPropertyOne, mockPropertyTwo);

    requiredProperties = properties.findByType(Integer.class).findRequired();

    assertThatPropertiesContains(requiredProperties, mockPropertyTwo);

    requiredProperties = properties.findByType(Boolean.class).findRequired();

    assertThatPropertiesIsEmpty(requiredProperties);
  }

  @Test
  public void findSerializableProperties() {

    Property mockPropertyOne = mockProperty("one", String.class, READABLE_PROPERTY_FUNCTION);

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      READABLE_PROPERTY_FUNCTION.andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyThree = mockProperty("three", Boolean.class, WRITABLE_PROPERTY_FUNCTION);

    Property mockPropertyFour = mockProperty("four", BigDecimal.class,
      READABLE_PROPERTY_FUNCTION.andThen(WRITABLE_PROPERTY_FUNCTION).andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyFive = mockProperty("five", Character.class,
      READABLE_PROPERTY_FUNCTION.andThen(WRITABLE_PROPERTY_FUNCTION).andThen(REQUIRED_PROPERTY_FUNCTION));

    Properties properties =
      Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree, mockPropertyFour, mockPropertyFive);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree,
      mockPropertyFour, mockPropertyFive);

    Properties serializableProperties = properties.findSerializable();

    assertThat(serializableProperties).isNotNull();
    assertThat(serializableProperties).hasSize(2);
    assertThat(serializableProperties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyFive);
  }

  @Test
  public void findTransientProperties() {

    Property mockPropertyOne = mockProperty("one", String.class,
      REQUIRED_PROPERTY_FUNCTION.andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      TRANSIENT_PROPERTY_FUNCTION);

    Property mockPropertyThree = mockProperty("three", Boolean.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    Properties transientProperties = properties.findTransient();

    assertThatPropertiesContains(transientProperties, mockPropertyOne, mockPropertyTwo);

    transientProperties = properties.findByType(Integer.class).findTransient();

    assertThatPropertiesContains(transientProperties, mockPropertyTwo);

    transientProperties = properties.findByType(Boolean.class).findTransient();

    assertThatPropertiesIsEmpty(transientProperties);
  }

  @Test
  public void findWritableProperties() {

    Property mockPropertyOne = mockProperty("one", String.class,
      READABLE_PROPERTY_FUNCTION.andThen(WRITABLE_PROPERTY_FUNCTION));

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      WRITABLE_PROPERTY_FUNCTION);

    Property mockPropertyThree = mockProperty("three", Boolean.class,
      READABLE_PROPERTY_FUNCTION);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo, mockPropertyThree);

    Properties writableProperties = properties.findWritable();

    assertThatPropertiesContains(writableProperties, mockPropertyOne, mockPropertyTwo);

    writableProperties = properties.findByType(Integer.class).findWritable();

    assertThatPropertiesContains(writableProperties, mockPropertyTwo);

    writableProperties = properties.findByType(Boolean.class).findWritable();

    assertThatPropertiesIsEmpty(writableProperties);
  }

  @Test
  public void queryPropertiesWithMultiplePredicates() {

    Property mockPropertyOne = mockProperty("one", String.class,
      READABLE_PROPERTY_FUNCTION.andThen(TRANSIENT_PROPERTY_FUNCTION));

    Property mockPropertyTwo = mockProperty("two", Integer.class,
      TRANSIENT_PROPERTY_FUNCTION);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    Properties result = properties.findByType(Boolean.class).findTransient();

    assertThatPropertiesIsEmpty(result);
  }

  @Test
  public void streamIsCorrect() {

    Property mockPropertyOne = mockProperty("one", String.class);
    Property mockPropertyTwo = mockProperty("two", Integer.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThatPropertiesContains(properties, mockPropertyOne, mockPropertyTwo);

    Stream<Property> propertiesStream = properties.stream();

    assertThat(propertiesStream).isNotNull();
    assertThat(propertiesStream).containsExactly(mockPropertyOne, mockPropertyTwo);

    propertiesStream = properties.findByType(Integer.class).stream();

    assertThat(propertiesStream).isNotNull();
    assertThat(propertiesStream).containsExactly(mockPropertyTwo);

    propertiesStream = properties.findByType(Boolean.class).stream();

    assertThat(propertiesStream).isNotNull();
    assertThat(propertiesStream).isEmpty();
  }
}
