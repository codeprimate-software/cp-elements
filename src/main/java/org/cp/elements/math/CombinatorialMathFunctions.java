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
package org.cp.elements.math;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.MathUtils;
import org.cp.elements.lang.annotation.ExperimentalApi;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract utility class implementing combinatorial math equations and calculations.
 *
 * @author John Blum
 * @see BigInteger
 * @see MathUtils
 * @since 0.1.0
 */
public abstract class CombinatorialMathFunctions {

  /**
   * Computes all non-repeatable, order-independent {@literal combinations} for all the {@link T elements}
   * in the given {@link List}.
   *
   * @param <T> {@link Class Type} of elements in the {@link List}.
   * @param list {@link List} on which the combinations will be computer.
   * @return all {@literal combinations} for all the {@link T elements} in the {@link List}.
   * @throws IllegalArgumentException if the given {@link List} is {@literal null}
   * or does not contain unique elements.
   * @see List
   */
  // TODO: Analyze performance and research more efficient / optimal ways for computing combinations.
  @ExperimentalApi
  public static @NotNull <T> List<List<T>> combinations(@NotNull List<T> list) {

    Assert.notNull(list, "List is required");

    int setSize = list.size();

    Assert.isTrue(setSize == newSet(list).size(), "Elements in List %s must be unique", list);

    List<List<T>> combinations = newList(0, setSize);

    // combinations 1
    List<List<T>> baseCombinations = list.stream().map(Collections::singletonList).toList();

    combinations.addAll(baseCombinations);

    // combinations 2 to setSize - 1; combinationIndex is 1-based.
    for (int combinationIndex = 2; combinationIndex < setSize; combinationIndex++) {
      List<List<T>> newCombinations = newList(combinationIndex, setSize);
      for (List<T> combination : baseCombinations) {
        int startIndex = list.indexOf(combination.get(asIndex(combination.size()))) + 1;
        for (int includeIndex = startIndex; includeIndex < setSize; includeIndex++) {
          List<T> newCombination = newList(combination.size() + 1);
          newCombination.addAll(combination);
          newCombination.add(list.get(includeIndex));
          newCombinations.add(newCombination);
        }
      }
      combinations.addAll(newCombinations);
      baseCombinations = newCombinations;
    }

    // combinationIndex setSize
    if (!combinations.contains(list)) {
      combinations.add(list);
    }

    return combinations;
  }

  private static <T> Set<T> newSet(Collection<T> collection) {
    return new HashSet<>(collection);
  }

  private static <T> List<T> newList(int combinationSize, int setSize) {
    return newList(computeNumberOfCombinationsBetween(combinationSize, setSize));
  }

  private static <T> List<T> newList(int size) {
    return new ArrayList<>(size);
  }

  protected static int computeNumberOfCombinationsBetween(int combinationSize, int setSize) {

    int result = 0;

    for (int count = combinationSize; count <= setSize; count++) {
      result += computeNumberOfCombinations(count, setSize);

    }

    return result;
  }

  protected static int computeNumberOfCombinations(int combinationSize, int setSize) {

    BigInteger denominator = computeFactorial(combinationSize)
      .multiply(computeFactorial(setSize - combinationSize));

    return computeFactorial(setSize).divide(denominator).intValue();
  }

  protected static BigInteger computeFactorial(int value) {
    return MathUtils.factorial(asBigInteger(value));
  }

  private static int asIndex(int size) {
    return size - 1;
  }

  private static BigInteger asBigInteger(int value) {
    return BigInteger.valueOf(value);
  }
}
