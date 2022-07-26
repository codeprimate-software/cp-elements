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

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.time.LocalDate;
import java.time.Period;
import java.util.Set;

import org.cp.elements.beans.annotation.Default;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Transient;
import org.cp.elements.util.stream.StreamUtils;
import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link Property} and {@link Annotation} metadata.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyMetadataUnitTests {

  @Test
  public void getPropertyAnnotationsIsCorrect() {

    Customer jonDoe = Customer.from("Jon Doe");

    BeanAdapter bean = BeanAdapter.from(jonDoe);

    Property salutation = bean.getModel().getProperty("salutation");

    assertThat(salutation).isNotNull();
    assertThat(salutation.getName()).isEqualTo("salutation");

    Set<Annotation> salutationAnnotations = salutation.getAnnotations();

    assertThat(salutationAnnotations).isNotNull();
    assertThat(salutationAnnotations).hasSize(4);

    Set<Class<? extends Annotation>> salutationAnnotationTypes =
      StreamUtils.toSet(salutationAnnotations.stream(), Annotation::annotationType);

    assertThat(salutationAnnotationTypes).containsExactlyInAnyOrder(Default.class,
      Transient.class, MockNonInheritedAnnotation.class,
      MockInheritedAnnotation.class);
  }

  @Test
  public void getInheritedDerivedPropertyAnnotationsIsCorrect() {

    Customer janeDoe = Customer.from("Jane Doe");

    BeanAdapter bean = BeanAdapter.from(janeDoe);

    Property age = bean.getModel().getProperty("age");

    assertThat(age).isNotNull();
    assertThat(age.getName()).isEqualTo("age");

    Set<Annotation> ageAnnotations = age.getAnnotations();

    assertThat(ageAnnotations).isNotNull();
    assertThat(ageAnnotations).isEmpty();
  }

  @Test
  public void getInheritedFieldBasedPropertyAnnotationsIsCorrect() {

    Customer janeDoe = Customer.from("Jane Doe");

    BeanAdapter bean = BeanAdapter.from(janeDoe);

    Property birthdate = bean.getModel().getProperty("birthdate");

    assertThat(birthdate).isNotNull();
    assertThat(birthdate.getName()).isEqualTo("birthdate");

    Set<Annotation> birthdateAnnotations = birthdate.getAnnotations();

    assertThat(birthdateAnnotations).isNotNull();
    assertThat(birthdateAnnotations).hasSize(1);

    Set<Class<? extends Annotation>> birthdateAnnotationTypes =
      StreamUtils.toSet(birthdateAnnotations.stream(), Annotation::annotationType);

    assertThat(birthdateAnnotationTypes).containsExactly(MockInheritedAnnotation.class);
  }

  @Test
  public void getDeclaredNamePropertyAnnotationsIsCorrect() {

    Person billyJean = Person.as("Billy Jean");

    BeanAdapter bean = BeanAdapter.from(billyJean);

    Property name = bean.getModel().getProperty("name");

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");

    Set<Annotation> nameAnnotations = name.getAnnotations();

    assertThat(nameAnnotations).isNotNull();
    assertThat(nameAnnotations).hasSize(2);

    Set<Class<? extends Annotation>> nameAnnotationTypes =
      StreamUtils.toSet(nameAnnotations.stream(), Annotation::annotationType);

    assertThat(nameAnnotationTypes).containsExactly(Required.class, MockNonInheritedAnnotation.class);
  }

  @Test
  public void getOverriddenInheritedNamePropertyAnnotationsIsCorrect() {

    Customer bobDoe = Customer.from("Bob Doe");

    BeanAdapter bean = BeanAdapter.from(bobDoe);

    Property name = bean.getModel().getProperty("name");

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");

    Set<Annotation> nameAnnotations = name.getAnnotations();

    assertThat(nameAnnotations).isNotNull();
    assertThat(nameAnnotations).hasSize(1);

    Set<Class<? extends Annotation>> nameAnnotationTypes =
      StreamUtils.toSet(nameAnnotations.stream(), Annotation::annotationType);

    assertThat(nameAnnotationTypes).containsExactly(Required.class);
  }

  // NOTE: Java's @Inherited Annotation only applies to Class type Annotation declarations.
  // NOTE: Lombok Annotations declare a RetentionPolicy of CLASS and therefore are not accessible/available at RUNTIME.

  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class Person {

    @Getter @Setter
    @MockInheritedAnnotation
    private LocalDate birthdate;

    @Required
    @lombok.NonNull
    @Getter(onMethod_ = { @NotNull, @MockNonInheritedAnnotation })
    private final String name;

    public int getAge() {

      LocalDate birthdate = getBirthdate();

      return birthdate != null
        ? Period.between(birthdate, LocalDate.now()).getYears()
        : 0;
    }
  }

  static class Customer extends Person {

    static Customer from(String name) {
      return new Customer(name);
    }

    static Customer from(Person person) {
      return new Customer(person.getName());
    }

    @Default("Sir")
    @Getter(onMethod_ = { @NotNull, @Transient, @MockNonInheritedAnnotation })
    @Setter(onMethod_ = { @Nullable, @MockInheritedAnnotation })
    private String salutation;

    Customer(String name) {
      super(name);
    }

    @Override
    public String getName() {
      return String.format("%1$s %2$s",
        StringUtils.hasText(getSalutation()) ? getSalutation() : "", super.getName()).trim();
    }
  }

  @Inherited
  @Retention(RetentionPolicy.RUNTIME)
  @Target({ ElementType.FIELD, ElementType.METHOD })
  @interface MockInheritedAnnotation {}

  @Retention(RetentionPolicy.RUNTIME)
  @Target({ ElementType.FIELD, ElementType.METHOD })
  @interface MockNonInheritedAnnotation {}

}
