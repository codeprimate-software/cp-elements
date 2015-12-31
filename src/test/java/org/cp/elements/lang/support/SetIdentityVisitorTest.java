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

package org.cp.elements.lang.support;

import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.Visitable;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The SetIdentityVisitorTest class is a test suite of test cases testing the contract and functionality
 * of the SetIdentityVisitor class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.SetIdentityVisitor
 * @since 1.0.0
 */
public class SetIdentityVisitorTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  private SetIdentityVisitor visitor = new SetIdentityVisitor();

  @Test
  public void constructDefaultSetIdentifyVisitor() {
    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getIdentifierSequence(), is(instanceOf(UUIDIdentifierSequence.class)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructInitializedSetIdentityVisitor() {
    IdentifierSequence<Long> mockIdentifierSequence = mock(IdentifierSequence.class);
    SetIdentityVisitor<Long> visitor = new SetIdentityVisitor<>(mockIdentifierSequence);

    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getIdentifierSequence(), is(sameInstance(mockIdentifierSequence)));
  }

  @Test
  public void constructSetIdentityVisitorWithNull() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("The IdentifierSequence cannot be null");

    new SetIdentityVisitor<>(null);
  }

  @Test
  public void visitIdentifiableVisitable() {
    IdentifiableVisitable mockIdentifiableVisitable = mock(IdentifiableVisitable.class);
    SetIdentityVisitor<Long> visitor = new SetIdentityVisitor<>(new SimpleIdentifierSequence());

    visitor.visit(mockIdentifiableVisitable);

    verify(mockIdentifiableVisitable, times(1)).setId(eq(1l));
  }

  @Test
  public void visitNonIdentifiableVisitable() {
    Visitable mockVisitable = mock(Visitable.class);
    SetIdentityVisitor<?> visitor = new SetIdentityVisitor<>();

    visitor.visit(mockVisitable);
  }

  @Test
  public void visitNull() {
    visitor.visit(null);
  }

  protected interface IdentifiableVisitable extends Identifiable<Long>, Visitable {
  }

}
