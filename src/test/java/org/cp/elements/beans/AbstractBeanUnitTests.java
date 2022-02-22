/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.beans;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.time.Instant;

import org.cp.elements.beans.AbstractBean.StateChangeCallback;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.security.model.User;
import org.junit.Test;

/**
 * Unit Tests for {@link AbstractBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.AbstractBean
 * @since 1.0.0
 */
public class AbstractBeanUnitTests {

  @SuppressWarnings("unchecked")
  private static User<Long> mockUser(String name) {

    User<Long> mockUser = mock(User.class, withSettings().lenient());

    doReturn(System.currentTimeMillis()).when(mockUser).getId();
    doReturn(name).when(mockUser).getName();

    return mockUser;
  }

  @Test
  public void constructAbstractBean() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();
    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void constructAbstractBeanWithId() {

    TestBean<Integer> bean = new TestBean<>(2);

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isEqualTo(2);
    assertThat(bean.isNew()).isFalse();
    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void setAndGetId() {

    TestBean<Integer> bean = new TestBean<>(null);

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();

    bean.setId(8);

    assertThat(bean.getId()).isEqualTo(8);
    assertThat(bean.isNotNew()).isTrue();

    bean.setId(null);

    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();

    assertThat(bean.<Bean<Integer, User<Long>, Object>>identifiedBy(2)).isSameAs(bean);
    assertThat(bean.getId()).isEqualTo(2);
    assertThat(bean.isNotNew()).isTrue();
  }

  @Test
  public void setAndGetEventDispatchEnabled() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean).isNotNull();
    assertThat(bean.isEventDispatchEnabled()).isTrue();

    bean.setEventDispatchEnabled(false);

    assertThat(bean.isEventDispatchEnabled()).isFalse();

    bean.setEventDispatchEnabled(true);

    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void setAndGetAuditableFields() {

    User<Long> jonDoe = mockUser("jonDoe");
    User<Long> pieDoe = mockUser("pieDoe");
    User<Long> sourDoe = mockUser("sourDoe");

    Object process = AbstractBeanUnitTests.class;

    Instant now = Instant.now();
    Instant beforeNow = now.minusSeconds(5);
    Instant afterNow = now.plusSeconds(5);

    TestBean<Integer> bean = new TestBean<>();

    bean.setCreatedBy(jonDoe);
    bean.setCreatedOn(beforeNow);
    bean.setCreatedWith(process);
    bean.setModifiedBy(pieDoe);
    bean.setModifiedOn(now);
    bean.setModifiedWith(process);

    assertThat(bean.getCreatedBy()).isEqualTo(jonDoe);
    assertThat(bean.getCreatedOn()).isEqualTo(beforeNow);
    assertThat(bean.getCreatedWith()).isEqualTo(process);
    assertThat(bean.getModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getModifiedOn()).isEqualTo(now);
    assertThat(bean.getModifiedWith()).isEqualTo(process);
    assertThat(bean.getLastModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getLastModifiedOn()).isEqualTo(now);
    assertThat(bean.getLastModifiedWith()).isEqualTo(process);

    bean.setModifiedBy(sourDoe);
    bean.setModifiedOn(afterNow);
    bean.setModifiedWith(process);

    assertThat(bean.getCreatedBy()).isEqualTo(jonDoe);
    assertThat(bean.getCreatedOn()).isEqualTo(beforeNow);
    assertThat(bean.getCreatedWith()).isEqualTo(process);
    assertThat(bean.getModifiedBy()).isEqualTo(sourDoe);
    assertThat(bean.getModifiedOn()).isEqualTo(afterNow);
    assertThat(bean.getModifiedWith()).isEqualTo(process);
    assertThat(bean.getLastModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getLastModifiedOn()).isEqualTo(now);
    assertThat(bean.getLastModifiedWith()).isEqualTo(process);
  }

  @Test
  public void isModified() {

    ValueHolder value = new ValueHolder("test");

    assertThat(value).isNotNull();
    assertThat(value.isModified()).isFalse();

    value.setValue("test");

    assertThat(value.isModified()).isFalse();
    assertThat(value.isModified("id")).isFalse();
    assertThat(value.isModified("value")).isFalse();

    value.setValue("TEST");

    assertThat(value.isModified()).isTrue();
    assertThat(value.isModified("id")).isFalse();
    assertThat(value.isModified("value")).isTrue();

    value.setValue("mock");

    assertThat(value.isModified()).isTrue();
    assertThat(value.isModified("id")).isFalse();
    assertThat(value.isModified("value")).isTrue();

    value.setValue("test");

    assertThat(value.isModified()).isFalse();
    assertThat(value.isModified("id")).isFalse();
    assertThat(value.isModified("value")).isFalse();

    value.setValue(null);

    assertThat(value.isModified()).isTrue();
    assertThat(value.isModified("id")).isFalse();
    assertThat(value.isModified("value")).isTrue();
  }

  @Test
  public void acceptsVisitor() {

    Visitor mockVisitor = mock(Visitor.class);

    TestBean<Integer> bean = new TestBean<>();

    bean.accept(mockVisitor);

    verify(mockVisitor, times(1)).visit(eq(bean));
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void mapAndUnmapPropertyNameToFieldName() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean.mapPropertyNameToFieldName("testProperty", "testField")).isNull();
    assertThat(bean.getFieldName("testProperty")).isEqualTo("testField");
    assertThat(bean.getFieldName("mockProperty")).isEqualTo("mockProperty");
    assertThat(bean.unmapPropertyNameToFieldName("testProperty")).isEqualTo("testField");
    assertThat(bean.getFieldName("testProperty")).isEqualTo("testProperty");
    assertThat(bean.getFieldName("mockProperty")).isEqualTo("mockProperty");
  }

  private void testMapIllegalPropertyNameToFieldName(String propertyName) {

    try {
      new ValueHolder().mapPropertyNameToFieldName(propertyName, "value");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Property name [%s] is required", propertyName);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapBlankPropertyNameToFieldName() {
    testMapIllegalPropertyNameToFieldName("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapEmptyPropertyNameToFieldName() {
    testMapIllegalPropertyNameToFieldName("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapNullPropertyNameToFieldName() {
    testMapIllegalPropertyNameToFieldName(null);
  }

  private void testMapPropertyNameToIllegalFieldName(String fieldName) {

    try {
      new ValueHolder().mapPropertyNameToFieldName("alias", fieldName);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Field name [%s] is required", fieldName);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapPropertyNameToEmptyBlankName() {
    testMapPropertyNameToIllegalFieldName("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapPropertyNameToEmptyFieldName() {
    testMapPropertyNameToIllegalFieldName("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapPropertyNameToNullFieldName() {
    testMapPropertyNameToIllegalFieldName(null);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void mapAndUnmapPropertyNameToStateChangeCallback() {

    StateChangeCallback<Object> mockCallback = mock(StateChangeCallback.class);

    ValueHolder holder = new ValueHolder();

    assertThat(holder).isNotNull();
    assertThat(holder.getAlias()).isNull();
    assertThat(holder.getValue()).isNull();
    assertThat(holder.mapPropertyNameToStateChangeCallback("alias", mockCallback)).isNull();

    holder.setAlias("test");

    assertThat(holder.getAlias()).isNull();
    assertThat(holder.getValue()).isNull();
    assertThat(holder.unmapPropertyNameToStateChangeCallback("alias")).isEqualTo(mockCallback);

    verify(mockCallback, times(1)).changeState(eq("test"));
    verifyNoMoreInteractions(mockCallback);
  }

  @SuppressWarnings("unchecked")
  private void testMapIllegalPropertyNameToStateChangeCallback(String propertyName) {

    try {
      new ValueHolder().mapPropertyNameToStateChangeCallback(propertyName, mock(StateChangeCallback.class));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Property name [%s] is required", propertyName);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapBlankPropertyNameToStateChangeCallback() {
    testMapIllegalPropertyNameToStateChangeCallback("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapEmptyPropertyNameToStateChangeCallback() {
    testMapIllegalPropertyNameToStateChangeCallback("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapNullPropertyNameToStateChangeCallback() {
    testMapIllegalPropertyNameToStateChangeCallback(null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapPropertyNameToNullStateChangeCallback() {

    try {
      new ValueHolder().mapPropertyNameToStateChangeCallback("alias", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The StateChangeCallback to map to property [alias] is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = PropertyNotFoundException.class)
  public void setPropertyForUnmappedFieldAndUnmappedStateChangeCallback() {

    ValueHolder holder = new ValueHolder("test");

    assertThat(holder).isNotNull();
    assertThat(holder.getValue()).isEqualTo("test");

    try {
      holder.setAlias("mock");
    }
    catch (PropertyNotFoundException expected) {

      assertThat(expected).hasMessage("No field [alias] for property [alias] was found on this Bean [%s]",
        holder.getClass().getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);

      throw expected;
    }
    finally {
      assertThat(holder.getValue()).isEqualTo("test");
    }
  }

  @Test
  public void setPropertyWhenPropertyNameIsMappedToFieldName() {

    ValueHolder holder = new ValueHolder("test");

    assertThat(holder).isNotNull();
    assertThat(holder.getValue()).isEqualTo("test");
    assertThat(holder.mapPropertyNameToFieldName("alias", "value")).isNull();

    holder.setAlias("mock");

    assertThat(holder.getValue()).isEqualTo("mock");
  }

  @Test
  public void setPropertyWhenPropertyNameIsMappedToStateChangeCallback() {

    ValueHolder holder = new ValueHolder("test");

    assertThat(holder).isNotNull();
    assertThat(holder.getValue()).isEqualTo("test");
    assertThat(holder.mapPropertyNameToStateChangeCallback("alias", holder::setValue)).isNull();

    holder.setAlias("mock");

    assertThat(holder.getValue()).isEqualTo("mock");
  }

  private static final class TestBean<ID extends Comparable<ID>> extends AbstractBean<ID, User<Long>, Object> {

    public TestBean() { }

    public TestBean(ID id) {
      super(id);
    }
  }

  @SuppressWarnings("unused")
  private static final class ValueHolder extends AbstractBean<Integer, User<Long>, Object> {

    private Object value;

    public ValueHolder() { }

    public ValueHolder(@Nullable Object value) {
      this.value = value;
    }

    public @Nullable Object getAlias() {
      return getValue();
    }

    public void setAlias(Object value) {
      processChange("alias", getValue(), value);
    }

    public @Nullable Object getValue() {
      return this.value;
    }

    public void setValue(@Nullable Object value) {
      processChange("value", getValue(), value);
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ValueHolder)) {
        return false;
      }

      ValueHolder that = (ValueHolder) obj;

      return ObjectUtils.equals(this.getValue(), that.getValue());
    }

    /**
     * @inheritDoc
     */
    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getValue());
      return hashValue;
    }

    /**
     * @inheritDoc
     */
    @Override
    public String toString() {
      return String.format("ValueHolder [%s]", getValue());
    }
  }
}
