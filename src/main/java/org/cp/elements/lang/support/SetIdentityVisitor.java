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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * A {@link Visitor} implementation that assigns an unique identifier (ID) to an {@link Identifiable object}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SetIdentityVisitor<T extends Comparable<T>> implements Visitor {

  private final IdentifierSequence<T> identifierSequence;

  /**
   * Default constructor constructing an instance of the SetIdentifyVisitor class initialized with the
   * {@link UUIDIdentifierSequence} in order to generate unique IDs in when setting
   * the {@link Identifiable} object's identity during visitation.
   *
   * @see org.cp.elements.lang.support.UUIDIdentifierSequence
   * @see #SetIdentityVisitor(IdentifierSequence)
   */
  public SetIdentityVisitor() {
    this(new UUIDIdentifierSequence());
  }

  /**
   * Constructs an instance of the SetIdentityVisitor class initialized with the specified {@link IdentifierSequence}
   * that will be used to generate unique IDs when setting an {@link Identifiable} object's identity during visitation.
   *
   * @param identifierSequence the {@link IdentifierSequence} used to generate unique IDs.
   * @throws IllegalArgumentException if the IdentifierSequence is null.
   * @see org.cp.elements.lang.IdentifierSequence
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public SetIdentityVisitor(@NotNull IdentifierSequence identifierSequence) {

    assertThat(identifierSequence)
      .throwing(newIllegalArgumentException("The IdentifierSequence cannot be null"))
      .isNotNull();

    this.identifierSequence = identifierSequence;
  }

  /**
   * Gets a reference to the {@link IdentifierSequence} used to generate unique IDs  in sequence when setting
   * the {@link Identifiable} objects identity during visitation.
   *
   * @return the {@link IdentifierSequence} used to generated unique IDs.
   * @see org.cp.elements.lang.IdentifierSequence
   */
  protected IdentifierSequence<T> getIdentifierSequence() {
    return this.identifierSequence;
  }

  /**
   * Visits any {@link Visitable} objects implementing the {@link Identifiable} interface and assigns
   * a unique ID from the sequence.
   *
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Identifiable#setId(Comparable)
   * @see org.cp.elements.lang.Visitable
   * @see #getIdentifierSequence()
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(@Nullable Visitable visitable) {

    if (visitable instanceof Identifiable) {
      ((Identifiable<T>) visitable).setId(getIdentifierSequence().nextId());
    }
  }
}
