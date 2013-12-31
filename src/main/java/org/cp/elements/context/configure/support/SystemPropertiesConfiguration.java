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

package org.cp.elements.context.configure.support;

import java.util.Collections;
import java.util.Iterator;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;

/**
 * The SystemPropertiesConfiguration class is a Configuration implementation for reading configuration information
 * from System Properties.
 * <p/>
 * @author John J. Blum
 * @see java.lang.System
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SystemPropertiesConfiguration extends AbstractConfiguration {

  public SystemPropertiesConfiguration() {
  }

  public SystemPropertiesConfiguration(final Configuration parent) {
    super(parent);
  }

  /**
   * Determines whether the configuration property identified by name is present in the configuration settings, which
   * means the configuration property was declared but not necessarily defined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is present (declared) in the configuration
   * settings.
   * @see #isSet(String)
   */
  @Override
  public boolean isPresent(final String propertyName) {
    return System.getProperties().containsKey(propertyName);
  }

  @Override
  protected String doGetPropertyValue(final String propertyName) {
    return System.getProperty(propertyName);
  }

  @Override
  public Iterator<String> iterator() {
    return Collections.unmodifiableSet(System.getProperties().stringPropertyNames()).iterator();
  }

}
