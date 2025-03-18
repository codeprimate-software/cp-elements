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

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.util.ServiceLoader;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryAware;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;

/**
 * {@link IdentifiableConverter} converts a {@link Long} into an instance of an {@link Identifiable} object.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @see org.cp.elements.lang.factory.ObjectFactoryAware
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IdentifiableConverter extends AbstractConverter<Long, Identifiable<Long>> implements ObjectFactoryAware {

  private ObjectFactory objectFactory;

  /**
   * Constructs a new {@link IdentifiableConverter}.
   */
  public IdentifiableConverter() {
    this(ObjectFactoryReferenceHolder.hasReference() ? ObjectFactoryReferenceHolder.get()
      : ServiceLoader.load(ObjectFactory.class).iterator().next());
  }

  /**
   * Constructs a new {@link IdentifiableConverter} initialized with the given {@link ObjectFactory}
   * used to construct an {@link Object} from an {@literal identifier (ID)}.
   *
   * @param objectFactory {@link ObjectFactory} used to construct an {@link Object} from an {@literal identifier (ID}.
   * @see org.cp.elements.lang.factory.ObjectFactory
   */
  public IdentifiableConverter(@NotNull ObjectFactory objectFactory) {
    setObjectFactory(objectFactory);
  }

  /**
   * Configures the {@link ObjectFactory} used to construct an {@link Object} from an {@literal identifier (ID)}.
   *
   * @param objectFactory reference to an {@link ObjectFactory}.
   * @see org.cp.elements.lang.factory.ObjectFactory
   */
  @Override
  public final void setObjectFactory(@NotNull ObjectFactory objectFactory) {
    this.objectFactory = objectFactory;
  }

  /**
   * Gets a reference to the configured {@link ObjectFactory}.
   *
   * @return a reference to the configured {@link ObjectFactory}.
   * @throws IllegalStateException if the {@link ObjectFactory} was not configured.
   * @see org.cp.elements.lang.factory.ObjectFactory
   */
  protected ObjectFactory getObjectFactory() {

    Assert.state(this.objectFactory != null, "No ObjectFactory was configured");

    return this.objectFactory;
  }

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   * @see #canConvert(Object, Class)
   */
  @Override
  public boolean canConvert(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
    return Long.class.equals(fromType) && toType != null && Identifiable.class.isAssignableFrom(toType);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class qualifying type QT}.
   *
   * @param <QT> {@link Class qualifying type} extending {@link Class type T}.
   * @param value {@link Object} of {@link Class type S} to convert.
   * @param identifiableType the {@link Class qualifying type} of the {@link Object} resolved in the conversion.
   * @return the converted {@link Object} of {@link Class qualifying type QT}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @throws IllegalArgumentException if {@link Class qualifying type} is {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   */
  @Override
  public <QT extends Identifiable<Long>> QT convert(Long value, Class<QT> identifiableType) {

    try {

      Identifiable<Long> identifiableObject =
        getObjectFactory().create(identifiableType, new Class<?>[] { Long.class }, value);

      if (identifiableObject.isNew()) {
        identifiableObject.setId(value);
      }

      return identifiableType.cast(identifiableObject);
    }
    catch (Throwable cause) {
      throw newConversionException(cause, "Cannot convert Long [%1$d] into an Object of type [%2$s]",
        value, identifiableType);
    }
  }
}
