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

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.data.convert.ConverterAdapter;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryAware;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;

/**
 * The IdentifiableConverter converts Long values into an instance of an Identifiable object.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @see org.cp.elements.lang.factory.ObjectFactoryAware
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IdentifiableConverter extends ConverterAdapter<Long, Identifiable<Long>> implements ObjectFactoryAware {

  private ObjectFactory objectFactory;

  public IdentifiableConverter() {
    if (ObjectFactoryReferenceHolder.hasReference()) {
      setObjectFactory(ObjectFactoryReferenceHolder.get());
    }
  }

  public IdentifiableConverter(final ObjectFactory objectFactory) {
    setObjectFactory(objectFactory);
  }

  protected ObjectFactory getObjectFactory() {
    Assert.state(objectFactory != null, "The reference to the ObjectFactory was not properly initialized!");
    return objectFactory;
  }

  @Override
  public final void setObjectFactory(final ObjectFactory objectFactory) {
    this.objectFactory = objectFactory;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (Long.class.equals(fromType) && isAssignableTo(toType, Identifiable.class));
  }

  @Override
  public <QT extends Identifiable<Long>> QT convert(final Long value, final Class<QT> identifiableType) {
    try {
      Identifiable<Long> identifiableObject = getObjectFactory().create(identifiableType, new Class[] { Long.class },
        value);

      if (identifiableObject.getId() == null) {
        identifiableObject.setId(value);
      }

      return identifiableType.cast(identifiableObject);
    }
    catch (Exception e) {
      throw new ConversionException(String.format("Failed to convert Long value (%1$d) into an object of type (%2$s)!",
        value, identifiableType), e);
    }
  }

}
