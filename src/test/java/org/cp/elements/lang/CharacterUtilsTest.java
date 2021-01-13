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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The CharacterUtilsTest class is a test suite of test cases testing the contract and functionality of the
 * CharacterUtils class.
 *
 * @author John J. Blum
 * @see java.lang.Character
 * @see org.cp.elements.lang.CharacterUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CharacterUtilsTest {

  @Test
  public void testIsBlank() {
    assertTrue(CharacterUtils.isBlank(null));
    assertTrue(CharacterUtils.isBlank('\0'));
    assertTrue(CharacterUtils.isBlank(' '));
    assertTrue(CharacterUtils.isBlank('\n'));
    assertTrue(CharacterUtils.isBlank('\t'));
    assertFalse(CharacterUtils.isBlank('0'));
    assertFalse(CharacterUtils.isBlank('_'));
    assertFalse(CharacterUtils.isBlank('\\'));
    assertFalse(CharacterUtils.isBlank('x'));
  }

  @Test
  public void testIsDigit() {
    assertTrue(CharacterUtils.isDigit('0'));
    assertTrue(CharacterUtils.isDigit('1'));
    assertTrue(CharacterUtils.isDigit('2'));
    assertTrue(CharacterUtils.isDigit('3'));
    assertTrue(CharacterUtils.isDigit('4'));
    assertTrue(CharacterUtils.isDigit('5'));
    assertTrue(CharacterUtils.isDigit('6'));
    assertTrue(CharacterUtils.isDigit('7'));
    assertTrue(CharacterUtils.isDigit('8'));
    assertTrue(CharacterUtils.isDigit('9'));
    assertFalse(CharacterUtils.isDigit(null));
    assertFalse(CharacterUtils.isDigit('!'));
    assertFalse(CharacterUtils.isDigit('O'));
    assertFalse(CharacterUtils.isDigit('l'));
    assertFalse(CharacterUtils.isDigit('I'));
    assertFalse(CharacterUtils.isDigit('V'));
    assertFalse(CharacterUtils.isDigit('X'));
    assertFalse(CharacterUtils.isDigit('$'));
    assertFalse(CharacterUtils.isDigit('.'));
  }

  @Test
  public void testIsLetterLowerCase() {
    assertTrue(CharacterUtils.isLetter('a'));
    assertTrue(CharacterUtils.isLetter('b'));
    assertTrue(CharacterUtils.isLetter('c'));
    assertTrue(CharacterUtils.isLetter('d'));
    assertTrue(CharacterUtils.isLetter('e'));
    assertTrue(CharacterUtils.isLetter('f'));
    assertTrue(CharacterUtils.isLetter('g'));
    assertTrue(CharacterUtils.isLetter('h'));
    assertTrue(CharacterUtils.isLetter('i'));
    assertTrue(CharacterUtils.isLetter('j'));
    assertTrue(CharacterUtils.isLetter('k'));
    assertTrue(CharacterUtils.isLetter('l'));
    assertTrue(CharacterUtils.isLetter('m'));
    assertTrue(CharacterUtils.isLetter('n'));
    assertTrue(CharacterUtils.isLetter('o'));
    assertTrue(CharacterUtils.isLetter('p'));
    assertTrue(CharacterUtils.isLetter('q'));
    assertTrue(CharacterUtils.isLetter('r'));
    assertTrue(CharacterUtils.isLetter('s'));
    assertTrue(CharacterUtils.isLetter('t'));
    assertTrue(CharacterUtils.isLetter('u'));
    assertTrue(CharacterUtils.isLetter('v'));
    assertTrue(CharacterUtils.isLetter('w'));
    assertTrue(CharacterUtils.isLetter('x'));
    assertTrue(CharacterUtils.isLetter('y'));
    assertTrue(CharacterUtils.isLetter('z'));
  }

  @Test
  public void testIsLetterUpperCase() {
    assertTrue(CharacterUtils.isLetter('A'));
    assertTrue(CharacterUtils.isLetter('B'));
    assertTrue(CharacterUtils.isLetter('C'));
    assertTrue(CharacterUtils.isLetter('D'));
    assertTrue(CharacterUtils.isLetter('E'));
    assertTrue(CharacterUtils.isLetter('F'));
    assertTrue(CharacterUtils.isLetter('G'));
    assertTrue(CharacterUtils.isLetter('H'));
    assertTrue(CharacterUtils.isLetter('I'));
    assertTrue(CharacterUtils.isLetter('J'));
    assertTrue(CharacterUtils.isLetter('K'));
    assertTrue(CharacterUtils.isLetter('L'));
    assertTrue(CharacterUtils.isLetter('M'));
    assertTrue(CharacterUtils.isLetter('N'));
    assertTrue(CharacterUtils.isLetter('O'));
    assertTrue(CharacterUtils.isLetter('P'));
    assertTrue(CharacterUtils.isLetter('Q'));
    assertTrue(CharacterUtils.isLetter('R'));
    assertTrue(CharacterUtils.isLetter('S'));
    assertTrue(CharacterUtils.isLetter('T'));
    assertTrue(CharacterUtils.isLetter('U'));
    assertTrue(CharacterUtils.isLetter('V'));
    assertTrue(CharacterUtils.isLetter('W'));
    assertTrue(CharacterUtils.isLetter('X'));
    assertTrue(CharacterUtils.isLetter('Y'));
    assertTrue(CharacterUtils.isLetter('Z'));
  }

  @Test
  public void testIsLetterUsingSymbols() {
    assertFalse(CharacterUtils.isLetter(null));
    assertFalse(CharacterUtils.isLetter('0'));
    assertFalse(CharacterUtils.isLetter('1'));
    assertFalse(CharacterUtils.isLetter('$'));
    assertFalse(CharacterUtils.isLetter('!'));
    assertFalse(CharacterUtils.isLetter('?'));
    assertFalse(CharacterUtils.isLetter('.'));
    assertFalse(CharacterUtils.isLetter('@'));
  }

  @Test
  public void testValueOf() {
    assertEquals('\0', CharacterUtils.valueOf(null));
    assertEquals('\0', CharacterUtils.valueOf('\0'));
    assertEquals('0', CharacterUtils.valueOf('0'));
    assertEquals('1', CharacterUtils.valueOf('1'));
    assertEquals('9', CharacterUtils.valueOf('9'));
    assertEquals('a', CharacterUtils.valueOf('a'));
    assertEquals('Z', CharacterUtils.valueOf('Z'));
    assertEquals('!', CharacterUtils.valueOf('!'));
    assertEquals('?', CharacterUtils.valueOf('?'));
  }

}
