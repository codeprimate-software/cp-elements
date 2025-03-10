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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.Period;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import org.cp.elements.beans.ReadPropertyException;
import org.cp.elements.beans.WritePropertyException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.data.struct.KeyValue;
import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.IllegalTypeException;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.reflect.ModifierUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link Property}.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
public class PropertyUnitTests {

  private Property mockProperty(String name) {
    return mockProperty(name, FunctionUtils.noopConsumer());
  }

  private Property mockProperty(String name, Consumer<Property> propertyConsumer) {

    Property mockProperty = mock(Property.class, name);

    doReturn(name).when(mockProperty).getName();
    propertyConsumer.accept(mockProperty);

    return mockProperty;
  }

  @Test
  public void constructProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    Property property = new Property(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isSameAs(mockBeanModel);
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);

    verifyNoInteractions(mockBeanModel, mockPropertyDescriptor);
  }

  @Test
  public void constructPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new Property(null, mock(PropertyDescriptor.class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new Property(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void fromFactoryMethodConstructsProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    Property property = Property.from(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isSameAs(mockBeanModel);
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);

    verifyNoInteractions(mockPropertyDescriptor);
  }

  @Test
  public void getBeanFromBeanModel() {

    BeanAdapter mockBean = mock(BeanAdapter.class);

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(mockBean).when(mockBeanModel).getBean();

    Property property = Property.from(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isSameAs(mockBeanModel);
    assertThat(property.getBean()).isEqualTo(mockBean);

    verify(mockBeanModel, times(1)).getBean();
    verifyNoMoreInteractions(mockBeanModel);
    verifyNoInteractions(mockBean, mockPropertyDescriptor);
  }

  @Test
  public void getFieldFromBeanPropertyBackedByObjectField() {

    Customer jonDoe = Customer.as("Jon Doe");

    BeanAdapter bean = BeanAdapter.from(jonDoe);

    Property property = bean.getModel().getProperty("name");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("name");
    assertThat(property.getType()).isEqualTo(String.class);
    assertThat(property.isDerived()).isFalse();

    Field name = property.getField();

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");
    assertThat(name.getType()).isEqualTo(String.class);
    assertThat(property.getField()).isSameAs(name);
  }

  @Test
  public void getFieldFromBeanPropertyBackedBySupertypeField() {

    Vip pieDoe = Vip.from("Pie Doe");

    BeanAdapter bean = BeanAdapter.from(pieDoe);

    Property property = bean.getModel().getProperty("birthdate");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("birthdate");
    assertThat(property.getType()).isEqualTo(LocalDate.class);
    assertThat(property.isDerived()).isFalse();

    Field field = property.getField();

    assertThat(field).isNotNull();
    assertThat(field.getName()).isEqualTo("birthdate");
    assertThat(field.getType()).isEqualTo(LocalDate.class);
    assertThat(property.getField()).isSameAs(field);
  }

  @Test
  public void getFieldFromDerivedBeanProperty() {

    Customer janeDoe = Customer.as("Jane Doe");

    BeanAdapter bean = BeanAdapter.from(janeDoe);

    Property property = bean.getModel().getProperty("age");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("age");
    assertThat(property.getType()).isEqualTo(Integer.TYPE);
    assertThat(property.isDerived()).isTrue();

    Field field = property.getField();

    assertThat(field).isNull();
  }

  @Test
  public void getAccessorMethodReturnsReadMethod() {

    Vip billyJean = Vip.from("Billy Jean");

    BeanAdapter bean = BeanAdapter.from(billyJean);

    Property age = spy(bean.getModel().getProperty("age"));

    assertThat(age).isNotNull();
    assertThat(age.getName()).isEqualTo("age");

    Method getAge = age.getAccessorMethod();

    assertThat(getAge).isNotNull();
    assertThat(getAge.getName()).isEqualTo("getAge");
    assertThat(getAge).isEqualTo(age.getReadMethod());

    verify(age, times(2)).getReadMethod();
  }

  @Test
  public void getReadMethodForReadableProperty() {

    Customer bobDoe = Customer.as("Bob Doe");

    BeanAdapter bean = BeanAdapter.from(bobDoe);

    Property property = bean.getModel().getProperty("age");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("age");
    assertThat(property.isDerived()).isTrue();
    assertThat(property.isReadable()).isTrue();

    Method getAge = property.getReadMethod();

    assertThat(getAge).isNotNull();
    assertThat(getAge.getName()).isEqualTo("getAge");
  }

  @Test
  public void getReadMethodForNonReadableProperty() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property property = bean.getModel().getProperty("salt");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("salt");
    assertThat(property.isReadable()).isFalse();

    Method method = property.getReadMethod();

    assertThat(method).isNull();
  }

  @Test
  public void getTargetObjectFromBean() {

    Customer cookieDoe = Customer.as("Cookie Doe");

    BeanAdapter mockBean = mock(BeanAdapter.class);

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(cookieDoe).when(mockBean).getTarget();
    doReturn(mockBean).when(mockBeanModel).getBean();

    Property property = spy(Property.from(mockBeanModel, mockPropertyDescriptor));

    assertThat(property).isNotNull();
    assertThat(property.getTargetObject()).isEqualTo(cookieDoe);

    verify(property, times(1)).getTargetObject();
    verify(property, times(1)).getBean();
    verify(property, times(1)).getBeanModel();
    verify(mockBeanModel, times(1)).getBean();
    verify(mockBean, times(1)).getTarget();
    verifyNoMoreInteractions(property, mockBeanModel, mockBean);
    verifyNoInteractions(mockPropertyDescriptor);
  }

  @Test
  public void getMutatorMethodReturnsWriteMethod() {

    Vip jackHandy = Vip.from("Jack Handy");

    BeanAdapter beanAdapter = BeanAdapter.from(jackHandy);

    Property birthdate = spy(beanAdapter.getModel().getProperty("birthdate"));

    assertThat(birthdate).isNotNull();
    assertThat(birthdate.getName()).isEqualTo("birthdate");

    Method setBirthdate = birthdate.getMutatorMethod();

    assertThat(setBirthdate).isNotNull();
    assertThat(setBirthdate.getName()).isEqualTo("setBirthdate");
    assertThat(setBirthdate).isEqualTo(birthdate.getWriteMethod());

    verify(birthdate, times(2)).getWriteMethod();
  }

  @Test
  public void getWriteMethodForWritableProperty() {

    Customer dillDoe = Customer.as("Dill Doe");

    BeanAdapter bean = BeanAdapter.from(dillDoe);

    Property property = bean.getModel().getProperty("birthdate");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("birthdate");
    assertThat(property.isWritable()).isTrue();

    Method setBirthdate = property.getWriteMethod();

    assertThat(setBirthdate).isNotNull();
    assertThat(setBirthdate.getName()).isEqualTo("setBirthdate");
  }

  @Test
  public void getWriteMethodForNonWritableProperty() {

    Customer froDoe = Customer.as("Fro Doe");

    BeanAdapter bean = BeanAdapter.from(froDoe);

    Property property = bean.getModel().getProperty("name");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("name");
    assertThat(property.isWritable()).isFalse();

    Method setName = property.getWriteMethod();

    assertThat(setName).isNull();
  }

  @Test
  public void isArrayTypedForArrayPropertyReturnsTrue() {

    Property array = BeanAdapter.from(new TypeWithArrayProperty()).getModel().getProperty("array");

    assertThat(array).isNotNull();
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(Object[].class);
    assertThat(array.isArrayTyped()).isTrue();
  }

  @Test
  public void isArrayTypedForNonArrayPropertyReturnsFalse() {

    Property list = BeanAdapter.from(new TypeWithListProperty()).getModel().getProperty("list");

    assertThat(list).isNotNull();
    assertThat(list.getName()).isEqualTo("list");
    assertThat(list.getType()).isEqualTo(List.class);
    assertThat(list.isArrayTyped()).isFalse();
  }

  @Test
  public void isArrayTypedForTwoDimensionalArrayPropertyReturnsTrue() {

    Property array = BeanAdapter.from(new TypeWithTwoDimensionalArrayProperty()).getModel().getProperty("array");

    assertThat(array).isNotNull();
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(Integer[][].class);
    assertThat(array.isArrayTyped()).isTrue();
  }

  @Test
  public void isCollectionLikeForArrayTypedPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isCollectionLike();
    doReturn(true).when(mockProperty).isArrayTyped();

    assertThat(mockProperty.isCollectionLike()).isTrue();

    verify(mockProperty, times(1)).isCollectionLike();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, never()).isTypedAs(any(Class.class));
    verify(mockProperty, never()).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isCollectionLikeForListTypedPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isArrayTyped();
    doCallRealMethod().when(mockProperty).isCollectionLike();
    doCallRealMethod().when(mockProperty).isTypedAs(any());
    doReturn(ArrayList.class).when(mockProperty).getType();

    assertThat(mockProperty.isCollectionLike()).isTrue();

    verify(mockProperty, times(1)).isCollectionLike();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).isTypedAs(eq(List.class));
    verify(mockProperty, times(2)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isCollectionLikeForMapTypedPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isArrayTyped();
    doCallRealMethod().when(mockProperty).isCollectionLike();
    doCallRealMethod().when(mockProperty).isTypedAs(any());
    doReturn(ConcurrentMap.class).when(mockProperty).getType();

    assertThat(mockProperty.isCollectionLike()).isTrue();

    verify(mockProperty, times(1)).isCollectionLike();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).isTypedAs(eq(List.class));
    verify(mockProperty, times(1)).isTypedAs(eq(Map.class));
    verify(mockProperty, times(3)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isCollectionLikeForSetTypedPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isArrayTyped();
    doCallRealMethod().when(mockProperty).isCollectionLike();
    doCallRealMethod().when(mockProperty).isTypedAs(any());
    doReturn(TreeSet.class).when(mockProperty).getType();

    assertThat(mockProperty.isCollectionLike()).isTrue();

    verify(mockProperty, times(1)).isCollectionLike();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).isTypedAs(eq(List.class));
    verify(mockProperty, times(1)).isTypedAs(eq(Map.class));
    verify(mockProperty, times(1)).isTypedAs(eq(Set.class));
    verify(mockProperty, times(4)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isCollectionLikeForNonCollectionLikePropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isCollectionLike();
    doCallRealMethod().when(mockProperty).isTypedAs(any());
    doReturn(false).when(mockProperty).isArrayTyped();
    doReturn(Object.class).when(mockProperty).getType();

    assertThat(mockProperty.isCollectionLike()).isFalse();

    verify(mockProperty, times(1)).isCollectionLike();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(3)).isTypedAs(any());
    verify(mockProperty, times(3)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isIndexedForArrayPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doReturn(true).when(mockProperty).isArrayTyped();
    doCallRealMethod().when(mockProperty).isIndexed();

    assertThat(mockProperty.isIndexed()).isTrue();

    verify(mockProperty, times(1)).isIndexed();
    verify(mockProperty, times(1)).isArrayTyped();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isIndexedForIndexedPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doReturn(false).when(mockProperty).isArrayTyped();
    doReturn(ArrayList.class).when(mockProperty).getType();
    doCallRealMethod().when(mockProperty).isIndexed();

    assertThat(mockProperty.isIndexed()).isTrue();

    verify(mockProperty, times(1)).isIndexed();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isIndexedForNonIndexedPropertyReturnsFalse() {

    Property mockProperty = mock(Property.class);

    doReturn(false).when(mockProperty).isArrayTyped();
    doReturn(Set.class).when(mockProperty).getType();
    doCallRealMethod().when(mockProperty).isIndexed();

    assertThat(mockProperty.isIndexed()).isFalse();

    verify(mockProperty, times(1)).isIndexed();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isIndexedForObjectPropertyReturnsFalse() {

    Property mockProperty = mock(Property.class);

    doReturn(false).when(mockProperty).isArrayTyped();
    doReturn(Object.class).when(mockProperty).getType();
    doCallRealMethod().when(mockProperty).isIndexed();

    assertThat(mockProperty.isIndexed()).isFalse();

    verify(mockProperty, times(1)).isIndexed();
    verify(mockProperty, times(1)).isArrayTyped();
    verify(mockProperty, times(1)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isTypedAsWithIncompatibleTypeReturnsFalse() {

    Property customer = BeanAdapter.from(Account.forCustomer(Customer.as("Imma Pigg")))
      .getModel().getProperty("customer");

    assertThat(customer).isNotNull();
    assertThat(customer.getName()).isEqualTo("customer");
    assertThat(customer.isTypedAs(Vip.class)).isFalse();
  }

  @Test
  public void isTypedAsWithExactTypeReturnsTrue() {

    Property customer = BeanAdapter.from(Account.forCustomer(Customer.as("Billy Bob")))
      .getModel().getProperty("customer");

    assertThat(customer).isNotNull();
    assertThat(customer.getName()).isEqualTo("customer");
    assertThat(customer.isTypedAs(Customer.class)).isTrue();
  }

  @Test
  public void isPersistentCallsIsSerializable() {

    Property mockProperty = mock(Property.class);

    doReturn(false).doReturn(true).when(mockProperty).isSerializable();
    doCallRealMethod().when(mockProperty).isPersistent();

    assertThat(mockProperty.isPersistent()).isFalse();
    assertThat(mockProperty.isPersistent()).isTrue();

    verify(mockProperty, times(2)).isPersistent();
    verify(mockProperty, times(2)).isSerializable();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isTypedAsWithCompatibleTypeReturnsTrue() {

    Property customer = BeanAdapter.from(Account.forCustomer(Customer.as("Ben Dover")))
      .getModel().getProperty("customer");

    assertThat(customer).isNotNull();
    assertThat(customer.getName()).isEqualTo("customer");
    assertThat(customer.isTypedAs(Person.class)).isTrue();
  }

  @Test
  public void isReadableForReadablePropertyReturnsTrue() {

    Customer jackBlack = Customer.as("Jack Black");

    BeanAdapter bean = BeanAdapter.from(jackBlack);

    Property age = bean.getModel().getProperty("age");

    assertThat(age).isNotNull();
    assertThat(age.getName()).isEqualTo("age");
    assertThat(age.isReadable()).isTrue();
  }

  @Test
  public void isReadableForWriteOnlyPropertyReturnsFalse() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property salt = bean.getModel().getProperty("salt");

    assertThat(salt).isNotNull();
    assertThat(salt.getName()).isEqualTo("salt");
    assertThat(salt.isReadable()).isFalse();
  }

  @Test
  public void isRequiredForRequiredPropertyIsTrue() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property property = bean.getModel().getProperty("salt");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("salt");
    assertThat(property.isRequired()).isTrue();
  }

  @Test
  public void isRequiredForOptionalPropertyIsFalse() {

    Customer hoeDoe = Customer.as("Hoe Doe");

    BeanAdapter bean = BeanAdapter.from(hoeDoe);

    Property property = bean.getModel().getProperty("birthdate");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("birthdate");
    assertThat(property.isRequired()).isFalse();
  }

  @Test
  public void isTransientForTransientProperty() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property property = bean.getModel().getProperty("salt");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("salt");

    Field salt = property.getField();

    assertThat(salt).isNotNull();
    assertThat(salt.getName()).isEqualTo("salt");
    assertThat(ModifierUtils.isTransient(salt)).isTrue();
    assertThat(property.isTransient()).isTrue();
  }

  @Test
  public void isSerializableForReadableNonTransientPropertyReturnsTrue() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isSerializable();
    doReturn(true).when(mockProperty).isReadable();
    doReturn(false).when(mockProperty).isTransient();

    assertThat(mockProperty.isSerializable()).isTrue();

    verify(mockProperty, times(1)).isSerializable();
    verify(mockProperty, times(1)).isReadable();
    verify(mockProperty, times(1)).isTransient();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isSerializableForReadableTransientPropertyReturnsFalse() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isSerializable();
    doReturn(true).when(mockProperty).isReadable();
    doReturn(true).when(mockProperty).isTransient();

    assertThat(mockProperty.isSerializable()).isFalse();

    verify(mockProperty, times(1)).isSerializable();
    verify(mockProperty, times(1)).isReadable();
    verify(mockProperty, times(1)).isTransient();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isSerializableForNonReadableNonTransientPropertyReturnsFalse() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isSerializable();
    doReturn(false).when(mockProperty).isReadable();
    doReturn(false).when(mockProperty).isTransient();

    assertThat(mockProperty.isSerializable()).isFalse();

    verify(mockProperty, times(1)).isSerializable();
    verify(mockProperty, times(1)).isReadable();
    verify(mockProperty, never()).isTransient();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isSerializableForNonReadableTransientPropertyReturnsFalse() {

    Property mockProperty = mock(Property.class);

    doCallRealMethod().when(mockProperty).isSerializable();
    doReturn(false).when(mockProperty).isReadable();
    doReturn(true).when(mockProperty).isTransient();

    assertThat(mockProperty.isSerializable()).isFalse();

    verify(mockProperty, times(1)).isSerializable();
    verify(mockProperty, times(1)).isReadable();
    verify(mockProperty, never()).isTransient();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void isTransientForNonTransientProperty() {

    Customer joeDoe = Customer.as("Joe Doe");

    BeanAdapter bean = BeanAdapter.from(joeDoe);

    Property property = bean.getModel().getProperty("name");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("name");
    assertThat(property.isTransient()).isFalse();
  }

  @Test
  public void isWritableForWritablePropertyReturnsTrue() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property salt = bean.getModel().getProperty("salt");

    assertThat(salt).isNotNull();
    assertThat(salt.getName()).isEqualTo("salt");
    assertThat(salt.isWritable()).isTrue();
  }

  @Test
  public void isWritableForReadOnlyPropertyReturnsFalse() {

    Account account = Account.forCustomer(Customer.as("Bubba"));

    BeanAdapter bean = BeanAdapter.from(account);

    Property customer = bean.getModel().getProperty("customer");

    assertThat(customer).isNotNull();
    assertThat(customer.getName()).isEqualTo("customer");
    assertThat(customer.isWritable()).isFalse();
  }

  @Test
  public void getNameFromPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn("testName").when(mockPropertyDescriptor).getName();

    Property property = spy(Property.from(mock(BeanModel.class), mockPropertyDescriptor));

    assertThat(property).isNotNull();
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);
    assertThat(property.getName()).isEqualTo("testName");

    verify(property, times(1)).getName();
    verify(property, times(2)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getName();
    verifyNoMoreInteractions(property, mockPropertyDescriptor);
  }

  @Test
  public void getTypeFromPropertyDescriptorReturnsPropertyType() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(LocalDateTime.class).when(mockPropertyDescriptor).getPropertyType();

    Property property = spy(Property.from(mock(BeanModel.class), mockPropertyDescriptor));

    assertThat(property).isNotNull();
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);
    assertThat(property.getType()).isEqualTo(LocalDateTime.class);

    verify(property, times(1)).getType();
    verify(property, times(2)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(property, mockPropertyDescriptor);
  }

  @Test
  public void getTypeFromPropertyDescriptorReturnsObjectClass() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(null).when(mockPropertyDescriptor).getPropertyType();

    Property property = spy(Property.from(mock(BeanModel.class), mockPropertyDescriptor));

    assertThat(property).isNotNull();
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);
    assertThat(property.getType()).isEqualTo(Object.class);

    verify(property, times(1)).getType();
    verify(property, times(2)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(property, mockPropertyDescriptor);
  }

  @Test
  public void getValueIsCorrect() {

    Customer moeDoe = Customer.as("Moe Doe");

    BeanAdapter bean = BeanAdapter.from(moeDoe);

    Property property = bean.getModel().getProperty("name");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("name");
    assertThat(property.getType()).isEqualTo(String.class);
    assertThat(property.isReadable()).isTrue();
    assertThat(property.getValue()).isEqualTo("Moe Doe");
  }

  @Test
  public void getValueFromNonReadableProperty() {

    CryptographicFunction function = new CryptographicFunction();

    BeanAdapter bean = BeanAdapter.from(function);

    Property property = bean.getModel().getProperty("salt");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("salt");
    assertThat(property.isReadable()).isFalse();

    ThrowableAssertions.assertThatThrowableOfType(ReadPropertyException.class)
      .isThrownBy(args -> property.getValue())
      .havingMessage("Property [salt] of bean [%s] is not readable", bean)
      .withNoCause();
  }

  @Test
  public void getTypedValueAsString() {

    Customer imaPigg = Customer.as("Ima Pigg");

    BeanAdapter bean = BeanAdapter.from(imaPigg);

    Property name = bean.getModel().getProperty("name");

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");
    assertThat(name.getType()).isEqualTo(String.class);

    Object nameValue = name.getValue();

    assertThat(nameValue).isInstanceOf(String.class);
    assertThat(nameValue).isEqualTo("Ima Pigg");

    String typedNameValue = name.getTypedValue();

    assertThat(typedNameValue).isEqualTo(nameValue);
  }

  @Test
  public void getTypedValueAsInteger() {

    Customer turdBurt = Customer.as("Turd Burt")
      .bornOn(LocalDate.now().minusYears(10));

    BeanAdapter bean = BeanAdapter.from(turdBurt);

    Property age = bean.getModel().getProperty("age");

    assertThat(age).isNotNull();
    assertThat(age.getName()).isEqualTo("age");
    assertThat(age.getType()).isEqualTo(Integer.TYPE);

    Object ageValue = age.getValue();

    assertThat(ageValue).isInstanceOf(Integer.class);
    assertThat(ageValue).isEqualTo(10);

    int typedAgeValue = age.getTypedValue();

    assertThat(typedAgeValue).isEqualTo(ageValue);

    Integer wrappedTypedAgeValue = age.getTypedValue();

    assertThat(wrappedTypedAgeValue).isEqualTo(typedAgeValue);
  }

  @Test
  public void getTypedValueAsCustomer() {

    Customer seamooreButts = Customer.as("Seamoore Butts");

    Account account = Account.forCustomer(seamooreButts);

    BeanAdapter bean = BeanAdapter.from(account);

    Property customer = bean.getModel().getProperty("customer");

    assertThat(customer).isNotNull();
    assertThat(customer.getName()).isEqualTo("customer");
    assertThat(customer.getType()).isEqualTo(Customer.class);

    Object customerValue = customer.getValue();

    assertThat(customerValue).isInstanceOf(Customer.class);
    assertThat(customerValue).isEqualTo(seamooreButts);

    Customer typedCustomerValue = customer.getTypedValue();

    assertThat(typedCustomerValue).isEqualTo(customerValue);
  }
  @Test
  public void setValueIsCorrect() {

    LocalDate birthdate = LocalDate.of(2022, Month.JUNE, 29);

    Customer pieDoe = Customer.as("Pie Doe");

    assertThat(pieDoe.getBirthdate()).isNull();

    BeanAdapter bean = BeanAdapter.from(pieDoe);

    Property property = bean.getModel().getProperty("birthdate");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("birthdate");
    assertThat(property.getType()).isEqualTo(LocalDate.class);
    assertThat(property.isWritable()).isTrue();

    property.setValue(birthdate);

    assertThat(pieDoe.getBirthdate()).isEqualTo(birthdate);
  }

  @Test
  public void setValueForNonWritableProperty() {

    Customer sourDoe = Customer.as("Sour Doe");

    BeanAdapter bean = BeanAdapter.from(sourDoe);

    Property property = bean.getModel().getProperty("name");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("name");
    assertThat(property.isWritable()).isFalse();

    ThrowableAssertions.assertThatThrowableOfType(WritePropertyException.class)
      .isThrownBy(args -> { property.setValue("Roller In Dough"); return null; })
      .havingMessage("Property [name] of bean [%s] is not writable", bean)
      .withNoCause();

    assertThat(sourDoe.getName()).isEqualTo("Sour Doe");
  }

  @Test
  public void asTypedPropertyIsCorrect() {

    Property property = new TestProperty(mock(BeanModel.class), mock(PropertyDescriptor.class));

    assertThat(property).isInstanceOf(TestProperty.class);

    TestProperty testProperty = property.asTypedProperty(TestProperty.class);

    assertThat(testProperty).isSameAs(property);
  }

  @Test
  public void asIllegalTypedPropertyThrowsException() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> mockProperty("test", property -> doCallRealMethod().when(property).asTypedProperty(any()))
        .asTypedProperty(TestProperty.class))
      .withMessage("[test] is not an instance of [%s]", TestProperty.class)
      .withNoCause();
  }

  @Test
  public void compareEqualPropertiesReturnsZero() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    doReturn("one").when(mockPropertyOne).getName();
    doReturn("one").when(mockPropertyTwo).getName();

    doCallRealMethod().when(mockPropertyOne).compareTo(any(Property.class));

    assertThat(mockPropertyOne.compareTo(mockPropertyTwo)).isZero();

    verify(mockPropertyOne, times(1)).compareTo(eq(mockPropertyTwo));
    verify(mockPropertyOne, times(1)).getName();
    verify(mockPropertyTwo, times(1)).getName();
    verifyNoMoreInteractions(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void comparePropertyToSelfReturnsZero() {

    Property mockProperty = mock(Property.class);

    doReturn("mockProperty").when(mockProperty).getName();
    doCallRealMethod().when(mockProperty).compareTo(any(Property.class));

    assertThat(mockProperty.compareTo(mockProperty)).isZero();

    verify(mockProperty, times(1)).compareTo(eq(mockProperty));
    verify(mockProperty, times(2)).getName();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void compareUnequalPropertiesReturnsNonZeroValue() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    doReturn("one").when(mockPropertyOne).getName();
    doReturn("two").when(mockPropertyTwo).getName();

    doCallRealMethod().when(mockPropertyOne).compareTo(any(Property.class));
    doCallRealMethod().when(mockPropertyTwo).compareTo(any(Property.class));

    assertThat(mockPropertyOne.compareTo(mockPropertyTwo)).isLessThan(0);
    assertThat(mockPropertyTwo.compareTo(mockPropertyOne)).isGreaterThan(0);

    verify(mockPropertyOne, times(1)).compareTo(eq(mockPropertyTwo));
    verify(mockPropertyTwo, times(1)).compareTo(eq(mockPropertyOne));
    verify(mockPropertyOne, times(2)).getName();
    verify(mockPropertyTwo, times(2)).getName();
    verifyNoMoreInteractions(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void propertyEqualsSelf() {

    Property mockProperty = mock(Property.class);

    assertThat(mockProperty.equals(mockProperty)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void propertiesAreEqual() {

    PropertyDescriptor mockPropertyDescriptor= mock(PropertyDescriptor.class);

    Property mockPropertyOne = Property.from(mock(BeanModel.class), mockPropertyDescriptor);
    Property mockPropertyTwo = Property.from(mock(BeanModel.class), mockPropertyDescriptor);

    doReturn("test").when(mockPropertyDescriptor).getName();

    assertThat(mockPropertyOne.equals(mockPropertyTwo)).isTrue();

    verify(mockPropertyDescriptor, times(2)).getName();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void propertiesAreNotEqual() {

    Property mockPropertyOne = mockProperty("mock");
    Property mockPropertyTwo = mockProperty("MOCK");
    Property testProperty = mockProperty("test");

    assertThat(mockPropertyOne.equals(mockPropertyTwo)).isFalse();
    assertThat(mockPropertyOne.equals(testProperty)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void propertyIsNotEqualToNull() {

    Property mockProperty = mockProperty("test");

    assertThat(mockProperty.equals(null)).isFalse();

    verify(mockProperty, never()).getName();
  }

  @Test
  @SuppressWarnings("all")
  public void propertyIsNotEqualToObject() {

    Property mockProperty = mockProperty("MOCK");

    assertThat(mockProperty.equals("MOCK")).isFalse();

    verify(mockProperty, never()).getName();
  }

  @Test
  public void propertyHashCode() {

    PropertyDescriptor mockPropertyDescriptorOne = mock(PropertyDescriptor.class);
    PropertyDescriptor mockPropertyDescriptorTwo = mock(PropertyDescriptor.class);

    doReturn("mock").when(mockPropertyDescriptorOne).getName();
    doReturn("test").when(mockPropertyDescriptorTwo).getName();

    Property propertyOne = Property.from(mock(BeanModel.class), mockPropertyDescriptorOne);
    Property propertyTwo = Property.from(mock(BeanModel.class), mockPropertyDescriptorTwo);

    int propertyOneHashCode = propertyOne.hashCode();
    int propertyTwoHashCode = propertyTwo.hashCode();

    assertThat(propertyOneHashCode).isNotZero();
    assertThat(propertyOneHashCode).isEqualTo(propertyOne.hashCode());
    assertThat(propertyOneHashCode).isNotEqualTo(propertyTwoHashCode);
    assertThat(propertyOneHashCode).isNotEqualTo("mock".hashCode());

    verify(mockPropertyDescriptorOne, times(2)).getName();
    verify(mockPropertyDescriptorTwo, times(1)).getName();
    verifyNoMoreInteractions(mockPropertyDescriptorOne, mockPropertyDescriptorTwo);
  }

  @Test
  public void toStringIsEqualToName() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn("mockName").when(mockPropertyDescriptor).getName();

    Property property = Property.from(mock(BeanModel.class), mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("mockName");
    assertThat(property.toString()).isEqualTo("mockName");

    verify(mockPropertyDescriptor, times(2)).getName();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void propertiesOfComposedType() {

    Vip vip = Vip.from("Jack Handy");

    BeanAdapter bean = BeanAdapter.from(vip);

    Properties properties = bean.getModel().getProperties();

    Set<String> propertyNames = properties.stream()
      .map(Property::getName)
      .collect(Collectors.toSet());

    assertThat(propertyNames).isNotNull();
    assertThat(propertyNames).hasSize(4);
    assertThat(propertyNames).containsExactlyInAnyOrder("age", "birthdate", "name", "rewardsNumber");
  }

  @Test
  public void propertiesOfInheritedType() {

    Customer jackBlack = Customer.as("Jack Black");

    Account account = Account.forCustomer(jackBlack);

    BeanAdapter bean = BeanAdapter.from(account);

    Properties properties = bean.getModel().getProperties();

    Set<String> propertyNames = properties.stream()
      .map(Property::getName)
      .collect(Collectors.toSet());

    assertThat(propertyNames).isNotNull();
    assertThat(propertyNames).hasSize(2);
    assertThat(propertyNames).containsExactlyInAnyOrder("customer", "number");
  }

  @Getter
  @ToString(of = "number")
  @EqualsAndHashCode(of = "customer")
  @RequiredArgsConstructor(staticName = "forCustomer")
  static class Account {

    @lombok.NonNull
    private final Customer customer;

    @Setter
    private String number;

    public Account with(String number) {
      setNumber(number);
      return this;
    }
  }

  @EqualsAndHashCode
  @SuppressWarnings("unused")
  static class CryptographicFunction {

    @Setter
    @Required
    private transient String salt;

    public String hash(String data) {
      throw new UnsupportedOperationException("Not Implemented");
    }
  }

  @Getter
  @EqualsAndHashCode
  @ToString(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class Customer implements Person {

    @Setter
    private LocalDate birthdate;

    private final String name;

    @SuppressWarnings("unused")
    public int getAge() {

      return Optional.ofNullable(getBirthdate())
        .map(birthdate -> Period.between(birthdate, LocalDate.now()))
        .map(Period::getYears)
        .orElse(0);
    }

    public Customer bornOn(LocalDate birthdate) {
      setBirthdate(birthdate);
      return this;
    }
  }

  @FunctionalInterface
  interface Person extends Nameable<String> { }

  @SuppressWarnings("unused")
  static class Vip extends Customer {

    static Vip from(Customer customer) {
      return new Vip(customer.getName());
    }

    static Vip from(String name) {
      return new Vip(name);
    }

    @Getter @Setter
    private String rewardsNumber;

    Vip(@lombok.NonNull String name) {
      super(name);
    }
  }

  static class TestProperty extends Property {

    TestProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
      super(beanModel, propertyDescriptor);
    }
  }

  static class TypeWithArrayProperty {

    @Getter @Setter
    private Object[] array;

  }

  static class TypeWithTwoDimensionalArrayProperty {

    @Getter @Setter
    private Integer[][] array;

  }

  static class TypeWithCollectionProperty {

    @Getter @Setter
    private Collection<?> collection;

  }

  static class TypeWithKeyValueProperty {

    @Getter @Setter
    private KeyValue<?, ?> keyValue;

  }

  static class TypeWithListProperty {

    @Getter @Setter
    private List<?> list = new ArrayList<>();

  }

  static class TypeWithMapProperty {

    @Getter @Setter
    private Map<?, ?> map = Collections.emptyMap();

  }

  static class TypeWithSetProperty {

    @Getter @Setter
    private Set<?> set = new HashSet<>();

  }

  static class TypeWithObjectProperty {

    @Getter @Setter
    private Object value;

  }
}
