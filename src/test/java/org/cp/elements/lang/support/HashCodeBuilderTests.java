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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.time.LocalDate;
import java.time.Month;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.logging.Level;

import org.cp.elements.enums.Gender;
import org.cp.elements.lang.AssertionException;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.test.TestUtils;
import org.junit.Test;
import org.mockito.ArgumentMatcher;

import lombok.Data;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link HashCodeBuilder}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.support.HashCodeBuilder
 * @since 1.0.0
 */
public class HashCodeBuilderTests {

  @Test
  public void createHashCodeBuilderWithDefaults() {

    HashCodeBuilder builder = HashCodeBuilder.create();

    assertThat(builder).isNotNull();
    assertThat(builder.hashValue()).isEqualTo(HashCodeBuilder.DEFAULT_BASE_VALUE);
    assertThat(builder.multiplier()).isEqualTo(HashCodeBuilder.DEFAULT_MULTIPLIER);
  }

  @Test
  public void createHashCodeBuilderWithCustomBaseValueAndMultiplier() {

    HashCodeBuilder builder = HashCodeBuilder.create(16, 51);

    assertThat(builder).isNotNull();
    assertThat(builder.hashValue()).isEqualTo(16);
    assertThat(builder.multiplier()).isEqualTo(51);
  }

  @Test(expected = AssertionException.class)
  public void createHashCodeBuilderWithIllegalBaseValue() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> HashCodeBuilder.create(-1, 51),
      () -> "baseValue [-1] must be greater than 0");
  }

  @Test(expected = AssertionException.class)
  public void createHashCodeBuilderWithIllegalMultiplier() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> HashCodeBuilder.create(1, 0),
      () -> "multiplier [0] must be greater than 0");
  }

  @Test
  public void combineUsesBaseValueAndMultiplier() {

    HashCodeBuilder builder = HashCodeBuilder.create(2, 4);

    assertThat(builder).isNotNull();
    assertThat(builder.hashValue()).isEqualTo(2);
    assertThat(builder.multiplier()).isEqualTo(4);
    assertThat(builder.combine(8)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(16);
  }

  @Test
  public void setHashValue() {

    HashCodeBuilder builder = HashCodeBuilder.create(7, 11);

    assertThat(builder).isNotNull();
    assertThat(builder.hashValue()).isEqualTo(7);
    assertThat(builder.setHashValue(17)).isSameAs(builder);
    assertThat(builder.hashValue()).isEqualTo(17);
  }

  @Test
  public void buildWithTrue() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(true)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2);
  }

  @Test
  public void buildWithFalse() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(false)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(3);
  }

  @Test
  public void buildWithByte() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with((byte) 8)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(10);
  }

  @Test
  public void buildWithChar() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with('X')).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2 + 'X');
  }

  @Test
  public void buildWithShort() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with((short) 16384)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(16386);
  }

  @Test
  public void buildWithInt() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(64536)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(64538);
  }

  @Test
  public void buildWithLong() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(5189972182L)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2 + (int) (5189972182L ^ (5189972182L >>> 32)));
  }

  @Test
  public void buildWithFloat() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(3.14159f)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2 + Float.floatToIntBits(3.14159f));
  }

  @Test
  public void buildWithDouble() {

    final long value = Double.doubleToLongBits(Math.PI);

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with(Math.PI)).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2 + (int) (value ^ (value >>> 32)));
  }

  @Test
  public void buildWithObject() {

    HashCodeBuilder builder = HashCodeBuilder.create(1, 2);

    assertThat(builder).isNotNull();
    assertThat(builder.with("test")).isSameAs(builder);
    assertThat(builder.build()).isEqualTo(2 + "test".hashCode());
  }

  @Test
  public void hashCodeForPerson() {

    Person jonDoe = Person.newPerson(2L, Gender.MALE, LocalDate.of(2000, Month.APRIL, 1),
      "transient", "Jon", "Doe");

    assertThat(HashCodeBuilder.hashCodeFor(jonDoe).build()).isEqualTo(jonDoe.hashCode());
  }

  @Test
  public void hashCodeForObjectWithBadHashCodeImplementation() {

    HashCodeBuilder.logger = spy(HashCodeBuilder.logger);

    doReturn(true).when(HashCodeBuilder.logger).isLoggable(eq(Level.FINE));

    ObjectWithBadHashCodeImplementation object = ObjectWithBadHashCodeImplementation.create("nonTrasient", "test");

    assertThat(HashCodeBuilder.hashCodeFor(object).build()).isNotEqualTo(object.hashCode());

    verify(HashCodeBuilder.logger, atLeast(2)).isLoggable(eq(Level.FINE));

    verify(HashCodeBuilder.logger, times(1)).fine(SupplierArgumentMatcher.equalSuppliers(
      () -> String.format("Hashing field [objectValue] on object [%s]",
        ObjectWithBadHashCodeImplementation.class.getName())));

    verify(HashCodeBuilder.logger, times(1)).fine(SupplierArgumentMatcher.equalSuppliers(
      () -> String.format("Hashing field [stringValue] on object [%s]",
        ObjectWithBadHashCodeImplementation.class.getName())));
  }

  private static final class SupplierArgumentMatcher<T> implements ArgumentMatcher<Supplier<T>> {

    private Object actualValue;
    private Object expectedValue;

    private static <T> Supplier<T> equalSuppliers(Supplier<T> expected) {
      return argThat(new SupplierArgumentMatcher<>(expected));
    }

    private SupplierArgumentMatcher(Supplier<T> expected) {
      assertThat(expected).describedAs("Expected must not be null").isNotNull();
      this.expectedValue = expected.get();
    }

    @Override
    public boolean matches(Supplier<T> actual) {
      actualValue = Optional.ofNullable(actual).map(Supplier::get).orElseGet(null);

      return Optional.ofNullable(actualValue)
        .filter(localActualValue -> localActualValue.equals(expectedValue))
        .isPresent();
    }

    @Override
    public String toString() {
      return String.format("Expected [%1$s]; but was [%2$s]", expectedValue, actualValue);
    }
  }

  @Getter
  @RequiredArgsConstructor(staticName = "newPerson")
  static class Person {

    @NonNull Long id;

    @NonNull Gender gender;

    @NonNull LocalDate birthDate;

    @NonNull transient Object transientNonHashableField;

    @NonNull String firstName;
    @NonNull String lastName;

    public String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
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

      return ObjectUtils.equals(this.getBirthDate(), that.getBirthDate())
        && ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        && ObjectUtils.equals(this.getGender(), that.getGender())
        && ObjectUtils.equals(this.getId(), that.getId())
        && ObjectUtils.equals(this.getLastName(), that.getLastName());
    }

    @Override
    public int hashCode() {

      int hashValue = 17;

      hashValue = 37 * hashValue + ObjectUtils.hashCode(getBirthDate());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getFirstName());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getGender());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getLastName());

      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("{ @type = %1$s, firstName = %2$s, lastName = %3$s, birthDate = %4$s, gender = %5$s }",
        getClass().getName(), getFirstName(), getLastName(), getBirthDate(), getGender());
    }
  }

  @Data
  @RequiredArgsConstructor(staticName = "create")
  static class ObjectWithBadHashCodeImplementation {

    @NonNull /* transient */ Object objectValue;

    @NonNull String stringValue;

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ObjectWithBadHashCodeImplementation)) {
        return false;
      }

      ObjectWithBadHashCodeImplementation that = (ObjectWithBadHashCodeImplementation) obj;

      return ObjectUtils.equals(this.getStringValue(), that.getStringValue());
    }

    @Override
    public int hashCode() {

      int hashValue = 17;

      hashValue = 37 * hashValue + ObjectUtils.hashCode(getStringValue());

      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("{ @type = %1$s, objectValue = %2$s, stringValue = %3$s }",
        getClass().getName(), getObjectValue(), getStringValue());
    }
  }
}
