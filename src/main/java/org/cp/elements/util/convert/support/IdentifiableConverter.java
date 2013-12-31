/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.convert.support;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryAware;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The IdentifiableConverter converts Long values into an instance of an Identifiable object.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Long
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @see org.cp.elements.lang.factory.ObjectFactoryAware
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see org.cp.elements.util.convert.ConverterAdapter
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
