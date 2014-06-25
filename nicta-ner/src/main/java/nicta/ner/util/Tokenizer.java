/*
 * #%L
 * NICTA Named Entity Recogniser library
 * %%
 * Copyright (C) 2010 - 2014 NICTA
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/gpl-3.0.html>.
 * #L%
 */
package nicta.ner.util;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;

import java.io.IOException;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static java.lang.Character.isLetterOrDigit;
import static java.lang.Character.isSpaceChar;
import static java.text.BreakIterator.DONE;
import static nicta.ner.util.Strings.endsWith;
import static nicta.ner.util.Strings.equalss;
import static nicta.ner.util.Strings.isSingleUppercaseChar;
import static nicta.ner.util.Tokenizer.Mode.WITHOUT_PUNCTUATION;
import static nicta.ner.util.Tokenizer.Mode.WITH_PUNCTUATION;

/**
 * This class utilizes a Java standard class to token the input sentence.
 * <p/>
 * This class is not thread safe.
 */
public class Tokenizer {

    public enum Mode {
        WITH_PUNCTUATION,
        WITHOUT_PUNCTUATION
    }

    private static final ImmutableCollection<String> ABBREVIATION_EXCEPTIONS;

    static {
        try { ABBREVIATION_EXCEPTIONS = ImmutableList.copyOf(IO.lines(Tokenizer.class, "TokenizerAbbreviation")); }
        catch (final IOException e) { throw new RuntimeException("Could not load the TokenizerAbbreviation file.", e); }
    }

    private final Mode mode;
    private List<String> currentSentence;

    public Tokenizer(final Mode mode) { this.mode = mode; }

    /**
     * Tokenize some text - not thread safe.
     * @param text tokenize this
     * @return tokenized text
     */
    public List<List<String>> process(final String text) {
        final List<List<String>> paragraph = new ArrayList<>();
        currentSentence = new ArrayList<>();

        // use a BreakIterator to iterate out way through the words of the text
        final BreakIterator wordIterator = BreakIterator.getWordInstance(new Locale("en", "US"));
        wordIterator.setText(text);

        // simply iterate through the text, keeping track of a start and end index of the current word
        int startIdx = wordIterator.first();
        for (int endIdx = wordIterator.next(); endIdx != DONE; startIdx = endIdx, endIdx = wordIterator.next()) {

            // get the word out, and skip if it is empty
            final String word = text.substring(startIdx, endIdx);
            final String trimmedWord = word.trim();
            if (trimmedWord.isEmpty()) continue;

            if (((mode == WITH_PUNCTUATION) || (mode == WITHOUT_PUNCTUATION && isLetterOrDigit(word.charAt(0))))) {
                boolean canBreakSentence = true;
                if (word.contains("'")) {
                    wordContainsApostrophe(word);
                }
                else if (".".equals(trimmedWord)) {
                    canBreakSentence = wordIsFullStop(word);
                }
                else if (":".equals(trimmedWord)) {
                    // if the current sentence is not empty, and the colon is not surrounded on both sides by spaces
                    if (!currentSentence.isEmpty()
                        && !isSpaceChar(text.charAt(startIdx - 1)) && !isSpaceChar(text.charAt(endIdx))) {

                        // get the previous and next words
                        final int previousWordIndex = currentSentence.size() - 1;
                        final String previousWord = currentSentence.get(previousWordIndex);
                        startIdx = endIdx;
                        endIdx = wordIterator.next();
                        // check if we reached the end of the text
                        if (endIdx == DONE) {
                            currentSentence.add(word);
                            break;
                        }
                        else {
                            // otherwise try to merge the 3 words back together again
                            final String nextWord = text.substring(startIdx, endIdx);
                            mergeWordsIntoSentence(previousWord, word, nextWord, previousWordIndex);
                        }
                    }
                    else currentSentence.add(word);
                }
                else currentSentence.add(word);

                // handling the end of a sentence
                if (canBreakSentence && equalss(trimmedWord, ".", ";", "?", "!")) {
                    paragraph.add(currentSentence);
                    currentSentence = new ArrayList<>();
                }
            }
        }

        if (!currentSentence.isEmpty()) paragraph.add(currentSentence);
        return paragraph;
    }

    @SuppressWarnings("StringConcatenationMissingWhitespace")
    private void mergeWordsIntoSentence(final String previousWord, final String word, final String nextWord,
                                        final int previousWordIndex) {
        // make sure the previous and next words both start with a letter or digit
        if (isLetterOrDigit(previousWord.charAt(0)) && isLetterOrDigit(nextWord.charAt(0))) {
            // merge the 3 words again and add the result, remove previous word from sentence
            currentSentence.remove(previousWordIndex);
            currentSentence.add(previousWord + word + nextWord);
        }
        // otherwise just add the word and next word
        else {
            currentSentence.add(word);
            currentSentence.add(nextWord);
        }
    }

    private boolean wordIsFullStop(final String word) {
        boolean canBreakSentence = true;
        final int previousWordIndex = currentSentence.size() - 1;
        if (previousWordIndex == -1) currentSentence.add(word);
        else {
            final String previousWord = currentSentence.get(previousWordIndex);
            if (ABBREVIATION_EXCEPTIONS.contains(previousWord)
                || isSingleUppercaseChar(previousWord)
                || previousWord.contains(".")) {
                currentSentence.remove(previousWordIndex);
                currentSentence.add(previousWord + ".");
                // do not break the sentence
                canBreakSentence = false;
            }
            else currentSentence.add(word);
        }
        return canBreakSentence;
    }

    private void wordContainsApostrophe(final String word) {
        if (word.endsWith("n't")) {
            final String w1 = word.substring(0, word.length() - 3);
            if (!w1.isEmpty()) currentSentence.add(w1);
            currentSentence.add("n't");
        }
        else if (endsWith(word, "'s", "'ll", "'re", "'m", "'ve", "'d")) {
            final int p = word.indexOf("'");
            final String w1 = word.substring(0, p);
            final String w2 = word.substring(p);
            if (!w1.isEmpty()) currentSentence.add(w1);
            if (!w2.isEmpty()) currentSentence.add(w2);
        }
        else currentSentence.add(word);
    }
}
