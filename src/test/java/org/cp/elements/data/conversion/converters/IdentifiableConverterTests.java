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

package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.provider.PrototypeObjectFactory;
import org.junit.Test;

/**
 * Unit tests for {@link IdentifiableConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.IdentifiableConverter
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
public class IdentifiableConverterTests {

  private IdentifiableConverter converter = new IdentifiableConverter(new PrototypeObjectFactory());

  @Test
  public void setAndGetObjectFactory() {

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);

    this.converter.setObjectFactory(mockObjectFactory);

    assertThat(this.converter.getObjectFactory()).isEqualTo(mockObjectFactory);
  }

  @Test(expected = IllegalStateException.class)
  public void getObjectFactoryWhenNotConfiguredThrowsException() {

    try {
      this.converter.setObjectFactory(null);
      this.converter.getObjectFactory();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("No ObjectFactory was configured");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void canConvertLongToIdentifiableReturnsTrue() {
    assertThat(this.converter.canConvert(Long.class, Identifiable.class)).isTrue();
  }

  @Test
  public void canConvertNullToIdentifiableReturnsFalse() {
    assertThat(this.converter.canConvert(null, Identifiable.class)).isFalse();
  }

  @Test
  public void cannotConvertToIdentifiableReturnsFalse() {

    assertThat(this.converter.canConvert(Identifiable.class, Identifiable.class)).isFalse();
    assertThat(this.converter.canConvert(Identifiable.class, null)).isFalse();
    assertThat(this.converter.canConvert(Identifiable.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Identifiable.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, null)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Identifiable.class)).isFalse();
  }

  @Test
  public void convertLongToIdentifiable() {

    Person person = this.converter.convert(1L, Person.class);

    assertThat(person).isNotNull();
    assertThat(person.getId()).isEqualTo(1L);
  }

  @Test
  public void convertLongToIdHolder() {

    IdHolder idHolder = this.converter.convert(2L, IdHolder.class);

    assertThat(idHolder).isNotNull();
    assertThat(idHolder.getId()).isEqualTo(2L);
  }

  @Test(expected = ConversionException.class)
  public void convertAnonymousTypeToIdentifiableThrowsException() {

    try {
      this.converter.convert(4L, AnonymousType.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert Long [4] into an Object of type [%s]",
        AnonymousType.class);

      assertThat(expected).hasCauseInstanceOf(IllegalAccessError.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @SuppressWarnings("unused")
  public static class IdHolder implements Identifiable<Long> {

    private Long id;

    public Long getId() {
      return id;
    }

    public void setId(Long id) {
      this.id = id;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof IdHolder)) {
        return false;
      }

      IdHolder that = (IdHolder) obj;

      return ObjectUtils.equals(this.getId(), that.getId());
    }

    @Override
    public int hashCode() {

      int hashValue = 17;

      hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());

      return hashValue;
    }

    @Override
    public String toString() {
      return String.valueOf(getId());
    }
  }

  @SuppressWarnings("unused")
  public static class AnonymousType extends IdHolder {

    @Override
    public Long getId() {
      throw new IllegalAccessError("Id cannot be accessed");
    }
  }

  @SuppressWarnings("unused")
  public static class Person extends IdHolder {

    private String firstName;
    private String lastName;

    public Person() {
    }

    public Person(Long id) {
      setId(id);
    }

    public Person(String firstName, String lastName) {
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public String getFirstName() {
      return firstName;
    }

    public void setFirstName(String firstName) {
      this.firstName = firstName;
    }

    public String getLastName() {
      return lastName;
    }

    public void setLastName(String lastName) {
      this.lastName = lastName;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return (ObjectUtils.equalsIgnoreNull(this.getId(), that.getId())
        && ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        && ObjectUtils.equals(this.getLastName(), that.getLastName()));
    }

    @Override
    public int hashCode() {

      int hashValue = 17;

      hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getFirstName());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getLastName());

      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }
  }
}
