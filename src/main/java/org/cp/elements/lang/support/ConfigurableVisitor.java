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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The ConfigurableVisitor class is a Visitor implementation that visits an object graph/hierarchy configuring
 * each object visited.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurableVisitor<T> implements Visitor {

  private final T configuration;

  /**
   * Constructs an instance of the ConfigurableVisitor class initialized with the specified configuration used to
   * configure objects visited in the graph/hierarchy.
   * <p/>
   * @param configuration the Configuration used to configure visited objects.
   */
  public ConfigurableVisitor(final T configuration) {
    Assert.notNull(configuration, "The configuration object cannot be null!");
    this.configuration = configuration;
  }

  /**
   * Visits Configurable objects in an object graph/hierarchy in order to apply configuration.
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Configurable#configure(Object)
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(final Visitable visitable) {
    if (visitable instanceof Configurable) {
      ((Configurable<T>) visitable).configure(configuration);
    }
  }

}
