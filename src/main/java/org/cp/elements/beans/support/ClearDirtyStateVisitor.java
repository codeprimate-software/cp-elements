/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.beans.support;

import org.cp.elements.beans.event.ChangeRecorder;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Visitor} implementation that {@literal clears} the dirty state of an {@link Object}
 * monitored by the {@link ChangeRecorder}.
 *
 * @author John Blum
 * @see org.cp.elements.beans.Bean
 * @see org.cp.elements.beans.event.ChangeRecorder
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
public class ClearDirtyStateVisitor implements Visitor {

  public static final ClearDirtyStateVisitor INSTANCE = new ClearDirtyStateVisitor();

  /**
   * @inheritDoc
   */
  @Override
  public void visit(@Nullable Visitable visitable) {

    if (visitable instanceof ChangeRecorder) {
      ((ChangeRecorder) visitable).clear();
    }
  }
}
