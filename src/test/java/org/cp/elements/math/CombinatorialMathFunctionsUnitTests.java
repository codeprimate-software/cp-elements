/*
 * Copyright 2023-Present Author or Authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link CombinatorialMathFunctions}.
 *
 * @author John Blum
 * @see Test
 * @see org.cp.elements.math.CombinatorialMathFunctions
 * @since 0.1.0
 */
public class CombinatorialMathFunctionsUnitTests {

	@Test
	void combinationsForSetOfOneIsCorrect() {

		List<String> characters = List.of("A");
		List<List<String>> combinations = CombinatorialMathFunctions.combinations(characters);

		assertThat(combinations).isNotNull();
		assertThat(combinations).hasSize(1);
		assertThat(combinations).contains(List.of("A"));
	}

	@Test
	void combinationsForSetOfTwoIsCorrect() {

		List<String> strings = List.of("AA", "BB");
		List<List<String>> combinations = CombinatorialMathFunctions.combinations(strings);

		assertThat(combinations).isNotNull();
		assertThat(combinations).hasSize(3);
		assertThat(combinations).contains(List.of("AA"));
		assertThat(combinations).contains(List.of("BB"));
		assertThat(combinations).contains(List.of("AA", "BB"));
	}

	@Test
	void combinationsForSetOfThreeIsCorrect() {

		List<String> characters = List.of("A", "B", "C");
		List<List<String>> combinations = CombinatorialMathFunctions.combinations(characters);

		assertThat(combinations).isNotNull();
		assertThat(combinations).hasSize(7);
		assertThat(combinations).contains(List.of("A"));
		assertThat(combinations).contains(List.of("B"));
		assertThat(combinations).contains(List.of("C"));
		assertThat(combinations).contains(List.of("A", "B"));
		assertThat(combinations).contains(List.of("A", "C"));
		assertThat(combinations).contains(List.of("B", "C"));
		assertThat(combinations).contains(List.of("A", "B", "C"));
	}

	@Test
	void combinationsForSetOfFourIsCorrect() {

		List<String> strings = List.of("AA", "AB", "AC", "AD");
		List<List<String>> combinations = CombinatorialMathFunctions.combinations(strings);

		assertThat(combinations).isNotNull();
		assertThat(combinations).hasSize(15);
		assertThat(combinations).contains(List.of("AA"));
		assertThat(combinations).contains(List.of("AB"));
		assertThat(combinations).contains(List.of("AC"));
		assertThat(combinations).contains(List.of("AD"));
		assertThat(combinations).contains(List.of("AA", "AB"));
		assertThat(combinations).contains(List.of("AA", "AC"));
		assertThat(combinations).contains(List.of("AA", "AD"));
		assertThat(combinations).contains(List.of("AB", "AC"));
		assertThat(combinations).contains(List.of("AB", "AD"));
		assertThat(combinations).contains(List.of("AC", "AD"));
		assertThat(combinations).contains(List.of("AA", "AB", "AC"));
		assertThat(combinations).contains(List.of("AA", "AB", "AD"));
		assertThat(combinations).contains(List.of("AA", "AB", "AD"));
		assertThat(combinations).contains(List.of("AB", "AC", "AD"));
		assertThat(combinations).contains(List.of("AA", "AB", "AC", "AD"));
	}

	@Test
	void combinationsForSetOfFiveIsCorrect() {

		List<String> characters = List.of("A", "B", "C", "D", "E");
		List<List<String>> combinations = CombinatorialMathFunctions.combinations(characters);

		assertThat(combinations).isNotNull();
		assertThat(combinations).hasSize(31);
		assertThat(combinations).contains(List.of("A", "B"));
		assertThat(combinations).contains(List.of("A", "C"));
		assertThat(combinations).contains(List.of("A", "D"));
		assertThat(combinations).contains(List.of("A", "E"));
		assertThat(combinations).contains(List.of("B", "C"));
		assertThat(combinations).contains(List.of("B", "D"));
		assertThat(combinations).contains(List.of("B", "E"));
		assertThat(combinations).contains(List.of("C", "D"));
		assertThat(combinations).contains(List.of("C", "E"));
		assertThat(combinations).contains(List.of("D", "E"));
		assertThat(combinations).contains(List.of("A", "B", "C"));
		assertThat(combinations).contains(List.of("A", "B", "D"));
		assertThat(combinations).contains(List.of("A", "B", "E"));
		assertThat(combinations).contains(List.of("A", "C", "D"));
		assertThat(combinations).contains(List.of("A", "C", "E"));
		assertThat(combinations).contains(List.of("A", "D", "E"));
		assertThat(combinations).contains(List.of("B", "C", "D"));
		assertThat(combinations).contains(List.of("B", "C", "E"));
		assertThat(combinations).contains(List.of("B", "D", "E"));
		assertThat(combinations).contains(List.of("C", "D", "E"));
		assertThat(combinations).contains(List.of("A", "B", "C", "D"));
		assertThat(combinations).contains(List.of("A", "B", "C", "E"));
		assertThat(combinations).contains(List.of("A", "B", "D", "E"));
		assertThat(combinations).contains(List.of("A", "C", "D", "E"));
		assertThat(combinations).contains(List.of("B", "C", "D", "E"));
		assertThat(combinations).contains(List.of("A", "B", "C", "D", "E"));
	}

	@Test
	void combinationsOfNonUniqueElements() {

		Arrays.asList(List.of("A", "A"), List.of("A", "B", "A")).forEach(list ->
			assertThatIllegalArgumentException()
				.isThrownBy(() -> CombinatorialMathFunctions.combinations(list))
				.withMessage("Elements in List %s must be unique", list)
				.withNoCause());
	}

	@Test
	void combinationsForSetOfThirteenSixteenAndTwentyOneIsCorrect() {

		IntStream.of(13, 16, 21).forEach(count -> {

			String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".substring(0, count);

			List<String> characters = IntStream.range(0, alphabet.length())
				.mapToObj(alphabet::charAt)
				.map(String::valueOf)
				.toList();

			assertThat(characters).hasSize(alphabet.length());

			List<List<String>> combinations = CombinatorialMathFunctions.combinations(characters);

			int expectedSize = CombinatorialMathFunctions.computeNumberOfCombinationsBetween(1, count);

			assertThat(combinations).isNotNull();
			assertThat(combinations).hasSize(expectedSize);
		});
	}

	@Test
	void computeNumberOfCombinationsIsCorrect() {

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinations(0, 5))
			.isEqualTo(1);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinations(0, 20))
			.isEqualTo(1);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinations(1, 5))
			.isEqualTo(5);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinations(2, 5))
			.isEqualTo(10);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinations(3, 5))
			.isEqualTo(10);
	}

	@Test
	void computeNumberOfCombinationsBetweenCombinationSizeAndSetSizeIsCorrect() {

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(3, 5))
			.isEqualTo(16);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(2, 5))
			.isEqualTo(26);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(1, 5))
			.isEqualTo(31);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(0, 5))
			.isEqualTo(32);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(1, 13))
			.isEqualTo(8_191);

		assertThat(CombinatorialMathFunctions.computeNumberOfCombinationsBetween(1, 26))
			.isEqualTo(67_108_863);
	}
}
