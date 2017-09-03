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

package org.cp.elements.data.convert.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.factory.provider.PrototypeObjectFactory;
import org.junit.Test;

/**
 * The IdentifiableConverterTest class is a test suite of test cases testing the contract and functionality of the
 * IdentifiableConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.convert.support.IdentifiableConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class IdentifiableConverterTest {

  private IdentifiableConverter converter = new IdentifiableConverter(new PrototypeObjectFactory());

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Long.class, Identifiable.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(Identifiable.class, Identifiable.class));
    assertFalse(converter.canConvert(null, Identifiable.class));
    assertFalse(converter.canConvert(Identifiable.class, null));
    assertFalse(converter.canConvert(Identifiable.class, Long.class));
    assertFalse(converter.canConvert(Identifiable.class, String.class));
    assertFalse(converter.canConvert(String.class, Identifiable.class));
    assertFalse(converter.canConvert(Long.class, Long.class));
    assertFalse(converter.canConvert(null, Long.class));
    assertFalse(converter.canConvert(Long.class, null));
  }

  @Test
  public void testConvert() {
    Person person = converter.convert(1l, Person.class);

    assertNotNull(person);
    assertNotNull(person.getId());
    assertEquals(1l, person.getId().longValue());
  }

  @Test
  public void testConvertIdHolder() {
    IdHolder idHolder = converter.convert(2l, IdHolder.class);

    assertNotNull(idHolder);
    assertNotNull(idHolder.getId());
    assertEquals(2l, idHolder.getId().longValue());
  }

  @Test(expected = IllegalAccessError.class)
  public void testConvertAnonymousType() {
    try {
      converter.convert(4l, AnonymousType.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("Failed to convert Long value (4) into an object of type (%1$s)!", AnonymousType.class),
        expected.getMessage());
      assertTrue(expected.getCause() instanceof IllegalAccessError);
      throw (IllegalAccessError) expected.getCause();
    }
  }

  @SuppressWarnings("unused")
  public static class IdHolder implements Identifiable<Long> {

    private Long id;

    public Long getId() {
      return id;
    }

    public void setId(final Long id) {
      this.id = id;
    }

    public boolean isNew() {
      return (getId() == null);
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
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
      throw new IllegalAccessError("Id cannot be accessed!");
    }
  }

  @SuppressWarnings("unused")
  public static class Person extends IdHolder {

    private String firstName;
    private String lastName;

    public Person() {
    }

    public Person(final Long id) {
      setId(id);
    }

    public Person(final String firstName, final String lastName) {
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public String getFirstName() {
      return firstName;
    }

    public void setFirstName(final String firstName) {
      this.firstName = firstName;
    }

    public String getLastName() {
      return lastName;
    }

    public void setLastName(final String lastName) {
      this.lastName = lastName;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
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
