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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Ordered;

/**
 * Unit Tests for {@link OrderedComparator}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.support.OrderedComparator
 * @since 1.0.0
 */
public class OrderedComparatorUnitTests {

  @Test
  public void compareIsCorrect() {

    Priority necessary = new Priority(1, "Necessary");
    Priority requirement = new Priority(2, "Requirement");
    Priority niceToHave = new Priority(3, "Nice-to-Have");
    Priority useless = new Priority(4, "Useless");

    List<Priority> priorities = new ArrayList<>(Arrays.asList(useless, niceToHave, necessary, requirement));

    Collections.sort(priorities);

    assertThat(priorities).containsExactly(necessary, niceToHave, requirement, useless);

    priorities.sort(OrderedComparator.INSTANCE);

    assertThat(priorities).containsExactly(necessary, requirement, niceToHave, useless);
  }

  static final class Priority implements Comparable<Priority>, Ordered {

    private final int index;
    private final String name;

    public Priority(final int index, final String name) {

      assert index > 0 : "The index of the Priority must be positive";
      assert name != null : "The name of the Priority cannot be null";

      this.index = index;
      this.name = name;
    }

    @Override
    public int getIndex() {
      return this.index;
    }

    @Override
    public void setIndex(int index) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    public String getName() {
      return name;
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(Priority priority) {
      return getName().compareTo(priority.getName());
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Priority)) {
        return false;
      }

      Priority that = (Priority) obj;

      return ObjectUtils.equals(getName(), that.getName());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getName());
    }

    @Override
    public String toString() {
      return getName();
    }
  }
}
