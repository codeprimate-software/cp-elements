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

import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyReadException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyWriteException;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.AbstractMap;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.beans.PropertyReadException;
import org.cp.elements.beans.PropertyWriteException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Transient;
import org.cp.elements.lang.reflect.ModifierUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract Data Type (ADT) modeling a {@link Object bean} property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.lang.Comparable
 * @see java.lang.annotation.Annotation
 * @see java.lang.reflect.AnnotatedElement
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see org.cp.elements.lang.Nameable
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public class Property implements Comparable<Property>, Nameable<String> {

  public static final Function<AnnotatedElement, Set<Annotation>> ALL_ANNOTATIONS_RESOLVER =
    annotatedElement -> annotatedElement != null
      ? CollectionUtils.asSet(annotatedElement.getAnnotations())
      : Collections.emptySet();

  public static final Function<AnnotatedElement, Set<Annotation>> DECLARED_ANNOTATIONS_RESOLVER =
    annotatedElement -> annotatedElement != null
      ? CollectionUtils.asSet(annotatedElement.getDeclaredAnnotations())
      : Collections.emptySet();

  protected static final Function<AnnotatedElement, Set<Annotation>> DEFAULT_ANNOTATIONS_RESOLVER =
    DECLARED_ANNOTATIONS_RESOLVER;

  protected static final Predicate<AnnotatedElement> IS_REQUIRED = annotatedElement ->
    annotatedElement != null && annotatedElement.isAnnotationPresent(Required.class);

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT_ANNOTATED = annotatedElement ->
    annotatedElement != null
      && (annotatedElement.isAnnotationPresent(Transient.class)
        || annotatedElement.isAnnotationPresent(java.beans.Transient.class));

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT_FIELD = annotatedElement ->
    annotatedElement instanceof Field && ModifierUtils.isTransient(annotatedElement);

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT = IS_TRANSIENT_FIELD.or(IS_TRANSIENT_ANNOTATED);

  /**
   * Factory method used to construct a new instance of {@link Property} from the given, required {@link BeanModel}
   * modeling the bean containing the property and a given, required {@link PropertyDescriptor}
   * describing the bean property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property.
   * @return a new {@link Property} modeling the bean property.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see #Property(BeanModel, PropertyDescriptor)
   * @see org.cp.elements.beans.model.BeanModel
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.beans.PropertyDescriptor
   */
  @Dsl
  public static @NotNull Property from(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    return new Property(beanModel, propertyDescriptor);
  }

  /**
   * Resolves all {@link Annotation Annotations} declared on the given {@link AnnotatedElement} by using the given,
   * required {@link Function} encapsulating the {@literal Strategy} for resolving {@link Annotation Annotations}
   * declared on an {@link AnnotatedElement}.
   *
   * The default {@literal Strategies} provided by this {@link Property} class includes
   * {@link AnnotatedElement#getAnnotations()} and {@link AnnotatedElement#getDeclaredAnnotations()}.
   *
   * @param <T> {@link Class type} of {@link AnnotatedElement}, such as {@link Field} or {@link Method}.
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on the {@link AnnotatedElement}; should not be {@literal null},
   * but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @param annotatedElementAnnotationsCache {@link Map} acting as a {@literal cache} to lookup previously resolved
   * {@link Annotation Annotations} declared on the {@link AnnotatedElement}; must not be {@literal null}.
   * @param annotatedElement {@link AnnotatedElement}, such as a {@link Field} or {@link Method}, from which
   * the declared {@link Annotation Annotations} are resolved.
   * @return a {@link Set} of {@link Annotation Annotations} resolved from the {@link AnnotatedElement}.
   * @see java.lang.reflect.AnnotatedElement
   * @see java.lang.annotation.Annotation
   * @see java.util.function.Function
   * @see java.util.Map
   */
  @NullSafe
  protected static <T extends AnnotatedElement> Set<Annotation> getAnnotatedElementAnnotations(
      @NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver,
      @NotNull Map<T, Set<Annotation>> annotatedElementAnnotationsCache,
      @Nullable T annotatedElement) {

    return annotatedElement != null
      ? nullSafeAnnotatedElementAnnotationsCache(annotatedElementAnnotationsCache)
        .computeIfAbsent(annotatedElement, nullSafeAnnotationsResolver(annotationsResolver))
      : Collections.emptySet();
  }

  @NullSafe
  private static @NotNull <T extends AnnotatedElement> Map<T, Set<Annotation>> nullSafeAnnotatedElementAnnotationsCache(
      @Nullable Map<T, Set<Annotation>> annotatedElementAnnotations) {

    return annotatedElementAnnotations != null ? annotatedElementAnnotations
      : new AbstractMap<T, Set<Annotation>>() {

          // Key is never cached; Value is always computed with the given Function.
          @Override
          public Set<Annotation> computeIfAbsent(T key, Function<? super T, ? extends Set<Annotation>> mappingFunction) {
            return mappingFunction.apply(key);
          }

          @Override
          public Set<Entry<T, Set<Annotation>>> entrySet() {
            return Collections.emptySet();
          }
      };
  }

  @NullSafe
  private static @NotNull Function<AnnotatedElement, Set<Annotation>> nullSafeAnnotationsResolver(
      @Nullable Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    return annotationsResolver != null ? annotationsResolver : DEFAULT_ANNOTATIONS_RESOLVER;
  }

  private transient final AtomicReference<Field> fieldReference = new AtomicReference<>(null);

  private transient final FieldResolver fieldResolver = new PropertyNameFieldResolver();

  private final BeanModel beanModel;

  private transient final Map<Field, Set<Annotation>> fieldAnnotations = new WeakHashMap<>();
  private transient final Map<Method, Set<Annotation>> methodAnnotations = new WeakHashMap<>();

  private final PropertyDescriptor propertyDescriptor;

  /**
   * Constructs a new instance of {@link Property} initialized with the given, required {@link BeanModel}
   * modeling the bean containing this {@link Property} along with the given, required {@link PropertyDescriptor}
   * describing the bean {@link Property}.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected Property(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {

    this.beanModel = ObjectUtils.requireObject(beanModel, "BeanModel is required");
    this.propertyDescriptor = ObjectUtils.requireObject(propertyDescriptor, "PropertyDescriptor is required");
  }

  /**
   * Gets all resolvable {@link Annotation Annotations} declared on this {@link Property}.
   *
   * A {@link Property Property's} {@link Annotation Annotations} consist of
   * all {@link Annotation Annotation's} declared on the {@link Property Property's} {@link Field}
   * if the {@link Property} is not {@link #isDerived()}, as well as all {@link Annotation Annotations}
   * declared on the {@link #getReadMethod() accessor method} and {@link #getWriteMethod() mutator method}
   * of the {@link Property}, providing the {@link Property} is both {@link #isReadable()} and {@link #isWritable()}.
   *
   * @return a {@link Set} of all resolvable {@link Annotation Annotations} declared on this {@link Property}.
   * @see java.lang.annotation.Annotation
   * @see #getAnnotations(Function)
   * @see java.util.Set
   */
  public Set<Annotation> getAnnotations() {
    return getAnnotations(DEFAULT_ANNOTATIONS_RESOLVER);
  }

  /**
   * Gets all resolvable {@link Annotation Annotations} declared on this {@link Property} using the given, required
   * {@link Function} encapsulating the {@literal Strategy} for resolving {@link Annotation Annotations}
   * on all {@link AnnotatedElement AnnotatedElements} of this {@link Property}.
   *
   * A {@link Property Property's} {@link Annotation Annotations} consist of
   * all {@link Annotation Annotation's} declared on the {@link Property Property's} {@link Field}
   * if the {@link Property} is not {@link #isDerived()}, as well as all {@link Annotation Annotations}
   * declared on the {@link #getReadMethod() accessor method} and {@link #getWriteMethod() mutator method}
   * of the {@link Property}, providing the {@link Property} is both {@link #isReadable()} and {@link #isWritable()}.
   *
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on an {@link AnnotatedElement}; should not be {@literal null},
   * but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @return a {@link Set} of all resolved {@link Annotation Annotations} declared on this {@link Property}.
   * @see #getWriteMethodAnnotations(Function)
   * @see #getReadMethodAnnotations(Function)
   * @see #getFieldAnnotations(Function)
   * @see java.lang.annotation.Annotation
   * @see java.util.function.Function
   * @see java.util.Set
   */
  public Set<Annotation> getAnnotations(@NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    Set<Annotation> propertyAnnotations = new HashSet<>();

    propertyAnnotations.addAll(getFieldAnnotations(annotationsResolver));
    propertyAnnotations.addAll(getReadMethodAnnotations(annotationsResolver));
    propertyAnnotations.addAll(getWriteMethodAnnotations(annotationsResolver));

    return propertyAnnotations;
  }

  /**
   * Gets an {@link Annotation} declared on this {@link Property} of the given {@link Class type}.
   *
   * @param <T> {@link Class type} of the {@link Annotation}.
   * @param annotationType {@link Class type} of {@link Annotation} to find.
   * @return an {@link Annotation} declared on this {@link Property} of the given {@link Class type}
   * or {@literal null} if an {@link Annotation} of the given {@link Class type} was not declared
   * on this {@link Property}.
   * @see #getAnnotation(Class, Function)
   * @see java.lang.annotation.Annotation
   */
  protected @Nullable <T extends Annotation> T getAnnotation(@NotNull Class<T> annotationType) {
    return getAnnotation(annotationType, DEFAULT_ANNOTATIONS_RESOLVER);
  }

  /**
   * Gets an {@link Annotation} declared on this {@link Property} of the given {@link Class type}.
   *
   * @param <T> {@link Class type} of the {@link Annotation}.
   * @param annotationType {@link Class type} of {@link Annotation} to find.
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on an {@link AnnotatedElement}; should not be {@literal null},
   * but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @return an {@link Annotation} declared on this {@link Property} of the given {@link Class type}
   * or {@literal null} if an {@link Annotation} of the given {@link Class type} was not declared
   * on this {@link Property}.
   * @see java.lang.annotation.Annotation
   * @see java.util.function.Function
   * @see #getAnnotations(Function)
   */
  protected @Nullable <T extends Annotation> T getAnnotation(@NotNull Class<T> annotationType,
      @NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    return getAnnotations(annotationsResolver).stream()
      .filter(annotation -> annotation.annotationType().equals(annotationType))
      .findFirst()
      .map(annotationType::cast)
      .orElse(null);
  }

  /**
   * Gets the {@link BeanAdapter bean} to which this {@link Property} belongs.
   *
   * @return the {@link BeanAdapter bean} to which this {@link Property} belongs.
   * @see org.cp.elements.beans.model.BeanModel#getBean()
   * @see org.cp.elements.beans.model.BeanAdapter
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #getBeanModel()
   */
  @Dsl
  protected @NotNull BeanAdapter getBean() {
    return getBeanModel().getBean();
  }

  /**
   * Gets the {@link BeanModel} modeling the bean containing this {@link Property}.
   *
   * @return the {@link BeanModel} modeling the bean containing this {@link Property}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  public @NotNull BeanModel getBeanModel() {
    return this.beanModel;
  }

  /**
   * Gets the {@literal JavaBeans} {@link PropertyDescriptor} describing this bean {@link Property}.
   *
   * @return the {@literal JavaBeans} {@link PropertyDescriptor} describing this bean {@link Property}.
   * @see java.beans.PropertyDescriptor
   */
  public @NotNull PropertyDescriptor getDescriptor() {
    return this.propertyDescriptor;
  }

  /**
   * Gets the {@link Object} {@link Field} backing this bean {@link Property} if not derived.
   *
   * The {@link Object} {@link Field} may be {@literal null} if this bean {@link Property} is derived
   * from some other {@link Object} {@link Field}. Think of an age property on a Person type derived
   * from a person's date of birth.
   *
   * @return the {@link Object} {@link Field} backing this bean {@link Property} if not derived.
   * @see org.cp.elements.beans.model.FieldResolver#resolve(Property)
   * @see java.lang.reflect.Field
   */
  protected @Nullable Field getField() {
    return fieldReference.updateAndGet(field -> field != null ? field
      : this.fieldResolver.resolve(this));
  }

  /**
   * Gets all resolvable {@link Annotation Annotations} declared on the {@link Property Property's}
   * {@link #getField() Field}.
   *
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on the {@link Property Property's {@link Field};
   * should not be {@literal null}, but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @return a {@link Set} of {@link Annotation Annotations} resolved from
   * the {@link Property Property's} {@link Field}. Returns an {@link Collections#emptySet()}
   * if this {@link Property} is {@link #isDerived() derived}.
   * @see #getAnnotatedElementAnnotations(Function, Map, AnnotatedElement)
   * @see java.lang.annotation.Annotation
   * @see java.util.Set
   * @see #getField()
   */
  protected Set<Annotation> getFieldAnnotations(
      @NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    return getAnnotatedElementAnnotations(annotationsResolver, this.fieldAnnotations, getField());
  }

  /**
   * Alias for {@link #getReadMethod()}.
   *
   * @return the {@link Method} used to read from this {@link Property}.
   * @see java.lang.reflect.Method
   * @see #getReadMethod()
   */
  protected @Nullable Method getAccessorMethod() {
    return getReadMethod();
  }

  /**
   * Gets the {@link Method} used to read from this {@link Property}.
   *
   * @return the {@link Method} used to read from this {@link Property};
   * may return {@literal null} if the {@link Property} cannot be read.
   * @see java.lang.reflect.Method
   * @see #getWriteMethod()
   */
  protected @Nullable Method getReadMethod() {
    return getDescriptor().getReadMethod();
  }

  /**
   * Gets all resolvable {@link Annotation Annotations} declared on the {@link Property Property's}
   * {@link #getReadMethod() accessor method}.
   *
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on this {@link Property Property's}
   * {@link #getReadMethod() accessor method}; should not be {@literal null},
   * but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @return a {@link Set} of {@link Annotation Annotations} resolved from
   * the {@link Property Property's} {@link #getReadMethod() accessor method}.
   * Returns an {@link Collections#emptySet()} if this {@link Property} is not {@link #isReadable() readable}.
   * @see #getAnnotatedElementAnnotations(Function, Map, AnnotatedElement)
   * @see java.lang.annotation.Annotation
   * @see #getReadMethod()
   * @see java.util.Set
   */
  protected Set<Annotation> getReadMethodAnnotations(
      @NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    return getAnnotatedElementAnnotations(annotationsResolver, this.methodAnnotations, getReadMethod());
  }

  /**
   * Gets the underlying {@link Object POJO} backing the bean for this {@link Property}.
   *
   * @return the underlying {@link Object POJO} backing the bean for this {@link Property}.
   * @see org.cp.elements.beans.model.BeanAdapter#getTarget()
   * @see java.lang.Object
   * @see #getBean()
   */
  protected @NotNull Object getTargetObject() {
    return getBean().getTarget();
  }

  /**
   * Alias for {@link #getWriteMethod()}.
   *
   * @return the {@link Method} used to write to this {@link Property}.
   * @see java.lang.reflect.Method
   * @see #getWriteMethod()
   */
  protected @Nullable Method getMutatorMethod() {
    return getWriteMethod();
  }

  /**
   * Gets the {@link Method} used to write to this {@link Property}.
   *
   * @return the {@link Method} used to write to this {@link Property};
   * may return {@literal null} if the {@link Property} cannot be written,
   * such as for a read-only {@link Property}.
   * @see java.lang.reflect.Method
   * @see #getReadMethod()
   */
  protected @Nullable Method getWriteMethod() {
    return getDescriptor().getWriteMethod();
  }

  /**
   * Gets all resolvable {@link Annotation Annotations} declared on the {@link Property Property's}
   * {@link #getReadMethod() mutator method}.
   *
   * @param annotationsResolver {@link Function} encapsulating the {@literal Strategy} used to resolve
   * the {@link Annotation Annotations} declared on this {@link Property Property's}
   * {@link #getWriteMethod()}  mutator method}; should not be {@literal null},
   * but defaults to {@link #DECLARED_ANNOTATIONS_RESOLVER}.
   * @return a {@link Set} of {@link Annotation Annotations} resolved from
   * the {@link Property Property's} {@link #getWriteMethod() mutator method}.
   * Returns an {@link Collections#emptySet()} if this {@link Property} is not {@link #isWritable()}  writable}.
   * @see #getAnnotatedElementAnnotations(Function, Map, AnnotatedElement)
   * @see java.lang.annotation.Annotation
   * @see #getWriteMethod()
   * @see java.util.Set
   */
  protected Set<Annotation> getWriteMethodAnnotations(
      @NotNull Function<AnnotatedElement, Set<Annotation>> annotationsResolver) {

    return getAnnotatedElementAnnotations(annotationsResolver, this.methodAnnotations, getWriteMethod());
  }

  /**
   * Determines whether this {@link Property} is annotated.
   *
   * @return a boolean value indicating whether this {@link Property} is annotated.
   * @see #getAnnotations();
   */
  public boolean isAnnotated() {
    return !getAnnotations().isEmpty();
  }

  /**
   * Determines whether this {@link Property} is annotated with an {@link Annotation} of the given, required
   * {@link Annotation#annotationType()}.
   *
   * @param annotationType {@link Class type} of the {@link Annotation} to evaluate.
   * @return a boolean value indicating whether this {@link Property} is annotated with an {@link Annotation}
   * of the given, required {@link Annotation#annotationType()}.
   * @see java.lang.annotation.Annotation#annotationType()
   * @see #getAnnotations()
   */
  public boolean isAnnotatedWith(@NotNull Class<? extends Annotation> annotationType) {

    return getAnnotations().stream()
      .map(Annotation::annotationType)
      .anyMatch(actualAnnotationType -> actualAnnotationType.equals(annotationType));
  }

  /**
   * Determines whether the {@link Class type} of this {@link Property} is an {@link Class#isArray() array}.
   *
   * @return a boolean value indicating whether the {@link Class type} of this {@link Property}
   * is an {@link Class#isArray() array}.
   * @see #getType();
   */
  public boolean isArrayTyped() {
    return ClassUtils.isArray(getType());
  }

  /**
   * Determines whether the {@link Class type} of this {@link Property} is {@link Collection Collection-like}.
   *
   * @return a boolean value indicating whether the {@link Class type} of this {@link Property}
   * is {@link Collection Collection-like}.
   * @see #isArrayTyped()
   * @see #isListTyped()
   * @see #isMapTyped()
   * @see #isSetTyped()
   */
  public boolean isCollectionLike() {
    return isArrayTyped() || isListTyped() || isMapTyped() || isSetTyped();
  }

  /**
   * Determines whether the {@link Class type} of this {@link Property} is a {@link List}.
   *
   * @return a boolean value indicating whether the {@link Class type} of this {@link Property} is a {@link List}.
   * @see #isTypedAs(Class)
   * @see java.util.List
   */
  public boolean isListTyped() {
    return isTypedAs(List.class);
  }

  /**
   * Determines whether the {@link Class type} of this {@link Property} is a {@link Map}.
   *
   * @return a boolean value indicating whether the {@link Class type} of this {@link Property} is a {@link Map}.
   * @see #isTypedAs(Class)
   * @see java.util.Map
   */
  public boolean isMapTyped() {
    return isTypedAs(Map.class);
  }

  /**
   * Determines whether the {@link Class type} of this {@link Property} is a {@link Set}.
   *
   * @return a boolean value indicating whether the {@link Class type} of this {@link Property} is a {@link Set}.
   * @see #isTypedAs(Class)
   * @see java.util.Set
   */
  public boolean isSetTyped() {
    return isTypedAs(Set.class);
  }

  /**
   * Determines whether the {@link #getType()} of this {@link Property} is assignment compatible with
   * the given {@link Class type}.
   *
   * If the {@link Class type} argument is {@literal null}, then this method returns {@literal false}.
   *
   * @param type {@link Class} used to evaluate this {@link Property#getType() property type}.
   * @return a boolean value indicating whether the {@link #getType()} of this {@link Property}
   * is assignment compatible with the given {@link Class type}.
   * @see java.lang.Class
   * @see #getType()
   */
  @NullSafe
  public boolean isTypedAs(@Nullable Class<?> type) {
    return type != null && type.isAssignableFrom(getType());
  }

  /**
   * Determines whether this {@link Property} is derived from another bean {@link Property}.
   *
   * This means this {@link Property} is not backed by an {@link Object} {@link Field}.
   * For example, this {@link Property} may represent the {@literal age} property of a {@literal Person} type
   * where age is computed from the person's {@literal date of birth} using the {@literal birthdate} {@link Field}.
   *
   * @return a boolean value indicating whether this {@link Property} is derived from
   * another bean {@link Property}.
   * @see #getField()
   */
  public boolean isDerived() {
    return getField() == null;
  }

  /**
   * Determine whether this {@link Property} can be read.
   *
   * @return a boolean value indicating whether this {@link Property} can be read.
   * @see #getReadMethod()
   */
  public boolean isReadable() {
    return getReadMethod() != null;
  }

  /**
   * Determines whether this {@link Property} is required.
   *
   * A {@link Property} is {@literal required} if the {@link Field} backing this {@link Property},
   * the {@link Method getter method} or the {@link Method setter method} are annotated with
   * the Element's {@link Required} annotation.
   *
   * @return a boolean value indicating whether this {@link Property} is required.
   */
  public boolean isRequired() {

    return IS_REQUIRED.test(getField())
      || IS_REQUIRED.test(getReadMethod())
      || IS_REQUIRED.test(getWriteMethod());
  }

  /**
   * Determines whether this {@link Property} is serializable.
   *
   * A {@link Property} is {@literal serializable} if the {@link Property} is {@link #isReadable()}
   * and is not {@link #isTransient()}.
   *
   * @return a boolean value indicating whether this {@link Property} is serializable.
   * @see #isTransient()
   * @see #isReadable()
   */
  public boolean isSerializable() {
    return isReadable() && !isTransient();
  }

  /**
   * Determines whether this {@link Property} is transient.
   *
   * A {@link Property} is {@literal transient} if the {@link Field} backing this {@link Property} is declared with
   * the {@link transient} keyword, or the {@link Method getter method} or {@link Method setter method}
   * are annotated with the Elements {@link Transient} annotation or the JavaBeans {@link java.beans.Transient}
   * annotation.
   *
   * @return a boolean value indicating whether this {@link Property} is transient.
   */
  public boolean isTransient() {

    return IS_TRANSIENT.test(getField())
      || IS_TRANSIENT.test(getReadMethod())
      || IS_TRANSIENT.test(getWriteMethod());
  }

  /**
   * Determine whether this {@link Property} can be written.
   *
   * @return a boolean value indicating whether this {@link Property} can be written.
   * @see #getWriteMethod()
   */
  public boolean isWritable() {
    return getWriteMethod() != null;
  }

  /**
   * Gets the {@link String name} of this {@link Property}.
   *
   * @return the {@link String name} of this {@link Property}.
   * @see java.lang.String
   */
  public @NotNull String getName() {
    return getDescriptor().getName();
  }

  /**
   * Gets the {@link Class type} of this {@link Property}.
   *
   * @return the {@link Class type} of this {@link Property}.
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  public @NotNull Class<?> getType() {
    return ObjectUtils.returnFirstNonNullValue(getDescriptor().getPropertyType(), Object.class);
  }

  /**
   * Gets the {@link Object value} of this {@link Property}.
   *
   * @return the {@link Object value} of this {@link Property}.
   * @throws PropertyReadException if this {@link Property} cannot be read.
   * @see java.lang.Object
   */
  public @Nullable Object getValue() {

    Assert.state(isReadable(), newPropertyReadException("Property [%s] of bean [%s] is not readable",
      getName(), getBean()));

    return ObjectUtils.invoke(getTargetObject(), getReadMethod(), new Object[0], Object.class);
  }

  /**
   * Sets the {@link Object value} of this {@link Property}.
   *
   * @param value {@link Object value} to set for this {@link Property}.
   * @throws PropertyWriteException if this {@link Property} cannot be written.
   * @see java.lang.Object
   */
  public void setValue(Object value) {

    Assert.state(isWritable(), newPropertyWriteException("Property [%s] of bean [%s] is not writable",
      getName(), getBean()));

    ObjectUtils.invoke(getTargetObject(), getWriteMethod(), new Object[] { value }, Void.class);
  }

  /**
   * Compares this {@link Property} to the given, required {@link Property} to determine (sort) order.
   *
   * {@link Property Properties} are ordered by {@link #getName()}.
   *
   * @param property {@link Property} to compare with this {@link Property}.
   * @return an {@link Integer} value indicating the order of  this {@link Property} relative to the given,
   * required {@link Property}. Returns less than zero if this {@link Property} is sorted before the given
   * {@link Property}, greater than zero if this {@link Property} is sorted after the given {@link Property}
   * and zero if this {@link Property} is equal to the given {@link Property}.
   * @see #getName()
   */
  @Override
  public int compareTo(@NotNull Property property) {
    return this.getName().compareTo(property.getName());
  }

  /**
   * Determines whether this {@link Property} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to evaluate for equality with this {@link Property}.
   * @return a boolean value indicating whether this {@link Property} is equal to the given {@link Object}.
   * @see #getName()
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Property)) {
      return false;
    }

    Property that = (Property) obj;

    return ObjectUtils.equals(this.getName(), that.getName());
  }

  /**
   * Computes the {@literal hash value} for this {{@link Property}.
   *
   * @return an {@link Integer} value with the hash code of this {@link Property}.
   * @see #getName()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getName());
  }

  /**
   * Returns the {@link String name} of this {@link Property}.
   *
   * @return the {@link String name} of this {@link Property}.
   * @see #getName()
   */
  @Override
  public String toString() {
    return getName();
  }
}
