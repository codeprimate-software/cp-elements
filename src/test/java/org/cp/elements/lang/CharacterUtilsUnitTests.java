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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link CharacterUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Character
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.CharacterUtils
 * @since 1.0.0
 */
public class CharacterUtilsUnitTests {

  @Test
  public void isBlankWithBlanksReturnsTrue() {

    assertThat(CharacterUtils.isBlank('\0')).isTrue();
    assertThat(CharacterUtils.isBlank(' ')).isTrue();
    assertThat(CharacterUtils.isBlank('\n')).isTrue();
    assertThat(CharacterUtils.isBlank('\r')).isTrue();
    assertThat(CharacterUtils.isBlank('\t')).isTrue();
  }

  @Test
  public void isBlankWithNonBlanksReturnsFalse() {

    assertThat(CharacterUtils.isBlank('!')).isFalse();
    assertThat(CharacterUtils.isBlank('0')).isFalse();
    assertThat(CharacterUtils.isBlank('_')).isFalse();
    assertThat(CharacterUtils.isBlank('\\')).isFalse();
    assertThat(CharacterUtils.isBlank('x')).isFalse();
  }

  @Test
  public void isBlankWithNullIsNullSafeReturnsTrue() {
    assertThat(CharacterUtils.isBlank(null)).isTrue();
  }

  @Test
  public void isDigitWithDigitsReturnsTrue() {

    assertThat(CharacterUtils.isDigit('0')).isTrue();
    assertThat(CharacterUtils.isDigit('1')).isTrue();
    assertThat(CharacterUtils.isDigit('2')).isTrue();
    assertThat(CharacterUtils.isDigit('3')).isTrue();
    assertThat(CharacterUtils.isDigit('4')).isTrue();
    assertThat(CharacterUtils.isDigit('5')).isTrue();
    assertThat(CharacterUtils.isDigit('6')).isTrue();
    assertThat(CharacterUtils.isDigit('7')).isTrue();
    assertThat(CharacterUtils.isDigit('8')).isTrue();
    assertThat(CharacterUtils.isDigit('9')).isTrue();
  }

  @Test
  public void isDigitWithNonDigitsReturnsFalse() {

    assertThat(CharacterUtils.isDigit('!')).isFalse();
    assertThat(CharacterUtils.isDigit('O')).isFalse();
    assertThat(CharacterUtils.isDigit('I')).isFalse();
    assertThat(CharacterUtils.isDigit('l')).isFalse();
    assertThat(CharacterUtils.isDigit('V')).isFalse();
    assertThat(CharacterUtils.isDigit('X')).isFalse();
    assertThat(CharacterUtils.isDigit('$')).isFalse();
    assertThat(CharacterUtils.isDigit('.')).isFalse();
  }

  @Test
  public void isDigitWithNullIsNullSafeReturnsFalse() {
    assertThat(CharacterUtils.isDigit(null)).isFalse();
  }

  @Test
  public void isLetterWithLowerCaseLetterReturnsTrue() {

    assertThat(CharacterUtils.isLetter('a')).isTrue();
    assertThat(CharacterUtils.isLetter('b')).isTrue();
    assertThat(CharacterUtils.isLetter('c')).isTrue();
    assertThat(CharacterUtils.isLetter('d')).isTrue();
    assertThat(CharacterUtils.isLetter('e')).isTrue();
    assertThat(CharacterUtils.isLetter('f')).isTrue();
    assertThat(CharacterUtils.isLetter('g')).isTrue();
    assertThat(CharacterUtils.isLetter('h')).isTrue();
    assertThat(CharacterUtils.isLetter('i')).isTrue();
    assertThat(CharacterUtils.isLetter('j')).isTrue();
    assertThat(CharacterUtils.isLetter('k')).isTrue();
    assertThat(CharacterUtils.isLetter('l')).isTrue();
    assertThat(CharacterUtils.isLetter('m')).isTrue();
    assertThat(CharacterUtils.isLetter('n')).isTrue();
    assertThat(CharacterUtils.isLetter('o')).isTrue();
    assertThat(CharacterUtils.isLetter('p')).isTrue();
    assertThat(CharacterUtils.isLetter('q')).isTrue();
    assertThat(CharacterUtils.isLetter('r')).isTrue();
    assertThat(CharacterUtils.isLetter('s')).isTrue();
    assertThat(CharacterUtils.isLetter('t')).isTrue();
    assertThat(CharacterUtils.isLetter('u')).isTrue();
    assertThat(CharacterUtils.isLetter('v')).isTrue();
    assertThat(CharacterUtils.isLetter('w')).isTrue();
    assertThat(CharacterUtils.isLetter('x')).isTrue();
    assertThat(CharacterUtils.isLetter('y')).isTrue();
    assertThat(CharacterUtils.isLetter('z')).isTrue();
  }

  @Test
  public void isLetterWithUpperCaseLetterReturnsTrue() {

    assertThat(CharacterUtils.isLetter('A')).isTrue();
    assertThat(CharacterUtils.isLetter('B')).isTrue();
    assertThat(CharacterUtils.isLetter('C')).isTrue();
    assertThat(CharacterUtils.isLetter('D')).isTrue();
    assertThat(CharacterUtils.isLetter('E')).isTrue();
    assertThat(CharacterUtils.isLetter('F')).isTrue();
    assertThat(CharacterUtils.isLetter('G')).isTrue();
    assertThat(CharacterUtils.isLetter('H')).isTrue();
    assertThat(CharacterUtils.isLetter('I')).isTrue();
    assertThat(CharacterUtils.isLetter('J')).isTrue();
    assertThat(CharacterUtils.isLetter('K')).isTrue();
    assertThat(CharacterUtils.isLetter('L')).isTrue();
    assertThat(CharacterUtils.isLetter('M')).isTrue();
    assertThat(CharacterUtils.isLetter('N')).isTrue();
    assertThat(CharacterUtils.isLetter('O')).isTrue();
    assertThat(CharacterUtils.isLetter('P')).isTrue();
    assertThat(CharacterUtils.isLetter('Q')).isTrue();
    assertThat(CharacterUtils.isLetter('R')).isTrue();
    assertThat(CharacterUtils.isLetter('S')).isTrue();
    assertThat(CharacterUtils.isLetter('T')).isTrue();
    assertThat(CharacterUtils.isLetter('U')).isTrue();
    assertThat(CharacterUtils.isLetter('V')).isTrue();
    assertThat(CharacterUtils.isLetter('W')).isTrue();
    assertThat(CharacterUtils.isLetter('X')).isTrue();
    assertThat(CharacterUtils.isLetter('Y')).isTrue();
    assertThat(CharacterUtils.isLetter('Z')).isTrue();
  }

  @Test
  public void isLetterWithDigitsReturnsFalse() {

    assertThat(CharacterUtils.isLetter('0')).isFalse();
    assertThat(CharacterUtils.isLetter('1')).isFalse();
    assertThat(CharacterUtils.isLetter('2')).isFalse();
    assertThat(CharacterUtils.isLetter('3')).isFalse();
    assertThat(CharacterUtils.isLetter('4')).isFalse();
    assertThat(CharacterUtils.isLetter('5')).isFalse();
    assertThat(CharacterUtils.isLetter('6')).isFalse();
    assertThat(CharacterUtils.isLetter('7')).isFalse();
    assertThat(CharacterUtils.isLetter('8')).isFalse();
    assertThat(CharacterUtils.isLetter('9')).isFalse();
  }

  @Test
  public void isLetterWithNullIsNullSafeReturnsFalse() {
    assertThat(CharacterUtils.isLetter(null)).isFalse();
  }

  @Test
  public void isLetterWithSymbolsReturnsFalse() {

    assertThat(CharacterUtils.isLetter('$')).isFalse();
    assertThat(CharacterUtils.isLetter('!')).isFalse();
    assertThat(CharacterUtils.isLetter('?')).isFalse();
    assertThat(CharacterUtils.isLetter('.')).isFalse();
    assertThat(CharacterUtils.isLetter('@')).isFalse();
  }

  @Test
  public void valueOfIsCorrect() {

    assertThat(CharacterUtils.valueOf(null)).isEqualTo('\0');
    assertThat(CharacterUtils.valueOf('\0')).isEqualTo('\0');
    assertThat(CharacterUtils.valueOf('0')).isEqualTo('0');
    assertThat(CharacterUtils.valueOf('1')).isEqualTo('1');
    assertThat(CharacterUtils.valueOf('2')).isEqualTo('2');
    assertThat(CharacterUtils.valueOf('9')).isEqualTo('9');
    assertThat(CharacterUtils.valueOf('a')).isEqualTo('a');
    assertThat(CharacterUtils.valueOf('Z')).isEqualTo('Z');
    assertThat(CharacterUtils.valueOf('!')).isEqualTo('!');
    assertThat(CharacterUtils.valueOf('?')).isEqualTo('?');
  }
}
