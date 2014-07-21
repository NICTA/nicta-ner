/*
 * #%L
 * NICTA t3as Named-Entity Recognition library
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
package org.t3as.ner.extractor;

import org.t3as.ner.NerResultSet;
import org.t3as.ner.data.Date;
import org.t3as.ner.data.DatePhraseModel;
import org.t3as.ner.data.Name;
import org.t3as.ner.Phrase;
import org.t3as.ner.Token;
import org.t3as.ner.resource.Configuration;
import org.t3as.ner.util.Dictionary;
import org.t3as.ner.util.IO;
import org.t3as.ner.util.Tokenizer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static java.lang.Character.isLetterOrDigit;
import static java.lang.Character.isUpperCase;
import static org.t3as.ner.data.Date.DateType;
import static org.t3as.ner.data.Date.DateType.NONE;
import static org.t3as.ner.data.Date.getDateType;
import static org.t3as.ner.util.Dictionary.isPastTense;
import static org.t3as.ner.util.Dictionary.isPlural;
import static org.t3as.ner.util.Strings.equalsIgnoreCase;
import static org.t3as.ner.util.Strings.startsWith;
import static org.t3as.ner.util.Tokenizer.Mode.WITH_PUNCTUATION;

// TODO: full of very long methods, please to fix

/** Rule-based expert system. */
public class NameExtractor {

    private static final Tokenizer TOKENIZER = new Tokenizer(WITH_PUNCTUATION);
    private static final Set<String> NON_NAME_WORDS;

    static {
        try { NON_NAME_WORDS = IO.lowerCasedWordSet(NameExtractor.class, "NON_NAME_WORDS"); }
        catch (final IOException ioe) { throw new RuntimeException("Could not load the NON_NAME_WORDS file.", ioe); }
    }

    private final int nameTypeScoreDimension;

    public NameExtractor(final Configuration conf) {
        nameTypeScoreDimension = conf.getNameTypes().size();
    }

    /** This method will parse the text input into tokens and name phrases. */
    public NerResultSet process(final String _text) {
        // tokenization
        final List<List<Token>> tokens = TOKENIZER.process(_text);
        // extract name phrase:
        final List<List<Phrase>> phrases = new ArrayList<>();

        for (final List<Token> tokenList : tokens) {
            // for each sentence
            final List<Phrase> sentencePhrase = new ArrayList<>();
            int wordPtr = 0;
            int phraseStubStartPtr = 0;
            while (wordPtr < tokenList.size()) {

                // iterate through every words in the sentence
                final List<Token> currentPhrase = new ArrayList<>();

                // get the phrase stub
                while (true) {
                    // we merge the names combined with the word 'and'
                    // if the second phrase is much shorter than the first phrase
                    // example: Canberra Institution of Science and Technology
                    boolean mergeAndFlag = false;
                    if ("and".equalsIgnoreCase(tokenList.get(wordPtr).text)
                        && !currentPhrase.isEmpty()
                        && wordPtr + 1 < tokenList.size()
                        && detectNameWordInSentenceByPosition(tokenList, wordPtr + 1)) {
                        if (wordPtr + currentPhrase.size() - 1 >= tokenList.size()) {
                            mergeAndFlag = true;
                        }
                        else {
                            for (int i = wordPtr + 1; i <= wordPtr + currentPhrase.size() - 1; i++) {
                                if (!detectNameWordInSentenceByPosition(tokenList, i)) {
                                    mergeAndFlag = true;
                                    break;
                                }
                            }
                        }
                    }

                    if (detectNameWordInSentenceByPosition(tokenList, wordPtr)
                        || (equalsIgnoreCase(tokenList.get(wordPtr).text, "of", "the", "'s")
                            && !currentPhrase.isEmpty()
                            && wordPtr + 1 < tokenList.size()
                            && detectNameWordInSentenceByPosition(tokenList, wordPtr + 1))
                        || mergeAndFlag) {
                        if (currentPhrase.isEmpty()) phraseStubStartPtr = wordPtr; // record the index of the first word
                        currentPhrase.add(tokenList.get(wordPtr));
                        wordPtr++;
                        if (wordPtr == tokenList.size()) break;
                    }
                    else break;
                }

                //get the attached 'the' or 'a'
                int phrasePos = phraseStubStartPtr;
                int phraseLen = currentPhrase.size();

                // if phrasePos is zero, then it is the first word, there won't be any 'the' or 'a' attached to this word.
                if (phrasePos > 0) {
                    final Token attachedArticle = tokenList.get(phrasePos - 1);
                    if ("the".equalsIgnoreCase(attachedArticle.text) || "a".equalsIgnoreCase(attachedArticle.text)) {
                        phrasePos--;
                        phraseLen++;
                    }
                }

                //handling non-name single words such as: Monday, January, etc.
                if (currentPhrase.size() == 1 && phraseStubStartPtr == phrasePos) {
                    if (NON_NAME_WORDS.contains(currentPhrase.get(0).text.toLowerCase())) {
                        currentPhrase.clear();
                        wordPtr--;
                    }
                }

                if (currentPhrase.isEmpty()) {
                    // if the current pointed word is not a name phrase
                    // ESTIMATE IF IT IS A TIME/DATE
                    final List<Token> currentDatePhrase = new ArrayList<>();
                    final DatePhraseModel dpm = new DatePhraseModel();
                    int tempPtr = wordPtr;
                    while (true) {
                        final Token word = tokenList.get(tempPtr);
                        final DateType type = getDateType(word.text);
                        if (type != NONE
                            || equalsIgnoreCase(word.text, "of", ",", "the")
                               && !currentDatePhrase.isEmpty()
                               && tempPtr + 1 != tokenList.size()
                               && getDateType(tokenList.get(tempPtr + 1).text) != NONE
                               && getDateType(tokenList.get(tempPtr - 1).text) != NONE) {
                            // is date word:
                            currentDatePhrase.add(word);
                            dpm.addType(type);
                            tempPtr++;
                            if (tempPtr == tokenList.size()) break;
                        }
                        else break;
                    }
                    if (dpm.isDate()) {
                        // add the phrase to phrase array
                        sentencePhrase.add(new Date(currentDatePhrase, wordPtr, currentDatePhrase.size(), wordPtr,
                                                    nameTypeScoreDimension));
                        wordPtr = tempPtr;
                    }
                }
                else if (!(currentPhrase.size() == 1
                           && (isPastTense(currentPhrase.get(0).text) || isPlural(currentPhrase.get(0).text)))) {
                    sentencePhrase.add(new Name(currentPhrase, phrasePos, phraseLen, phraseStubStartPtr,
                                                nameTypeScoreDimension));
                }

                wordPtr++;
            }

            phrases.add(sentencePhrase);
        }

        // find attached words(preps):
        for (int si = 0; si < tokens.size(); si++) {
            final List<Token> sentenceToken = tokens.get(si);
            final List<Phrase> sentencePhrase = phrases.get(si);
            for (int pi = 0; pi < sentencePhrase.size(); pi++) {
                getAttachedPrep(sentenceToken, sentencePhrase, pi);
            }
        }

        return new NerResultSet(phrases, tokens);
    }

    /** Detects if a particular word in a sentence is a name. */
    private static boolean detectNameWordInSentenceByPosition(final List<Token> _text, final int _pos) {
        boolean isFirstWord = false;
        boolean nextWordIsName = false;
        if (_pos == 0 || !isLetterOrDigit((_text.get(_pos - 1).text.charAt(0)))) {
            isFirstWord = true;
            //noinspection SimplifiableIfStatement
            if (_text.size() > _pos + 1) {
                final String plus1 = _text.get(_pos + 1).text;
                nextWordIsName = ("of".equalsIgnoreCase(plus1) || "'s".equalsIgnoreCase(plus1))
                                 ? ((_text.size() > (_pos + 2)) && isName(_text.get(_pos + 2).text, false, false))
                                 : isName(plus1, false, false);
            }
            else nextWordIsName = false;
        }
        //noinspection UnnecessaryLocalVariable
        final boolean isName = isName(_text.get(_pos).text, isFirstWord, nextWordIsName);

        /*
        String wordType = dict.checkup(_text.get(_pos).toLowerCase());
        if (isFirstWord && !isName && wordType != null && wordType.startsWith("JJ")) {
            // if the first word is determined not to be a name but it is an adj.,
            // and if the second word is a name, we consider the first word to be a name as well.
            if (isName(_text.get(_pos + 1), false))
                return true;
        }
        */

        return isName;
    }

    /** This method detects a word if it is a potential name word. */
    private static boolean isName(final String _text, final boolean isFirstWord, final boolean nextWordIsName) {
        if (hasManyCaps(_text)) return true;
        else if (startsWithCap(_text)) {
            if (isFirstWord) {
                // if the word is the first word in a sentence and
                // ends with -ly (adv) -> consider it not a name
                if (_text.endsWith("ly")) return false;

                // we need to deal with the first word in the sentence very carefully.
                // as we can not tell if the first word is a name by detecting upper case characters.
                final String type_original = Dictionary.checkup(_text);
                final String type_lowercase = Dictionary.checkup(_text.toLowerCase());
                if (type_original == null) {
                    // if the word cannot be found in the dictionary, we consider it as a name entity.
                    if (type_lowercase == null) return true;
                }
                else if ("NNP".equals(type_original)) return true;
                //noinspection IfMayBeConditional,SimplifiableIfStatement
                if (startsWith(type_lowercase, "IN", "CC", "RB", "WRB"/*, "JJ"*/, ".")) return false;
                else return nextWordIsName;
            }
            else return true;
        }
        else return false;
    }

    /** This method will find all the attached preps of a phrase. */
    private static void getAttachedPrep(final List<Token> sentenceToken, final List<Phrase> sentencePhrase,
                                        final int index) {
        final String prep;
        boolean nameSequenceMeetEnd = true;
        final Collection<Phrase> phraseSequence = new HashSet<>();

        int phrasePtr = index;
        Phrase currentNamePhrase = sentencePhrase.get(phrasePtr);
        int phrasePtrInSentence;

        if (currentNamePhrase.attachedWordMap.get("prep") != null) return;

        // we need to find out all the name phrases in a sequence:
        // Example: Students who came from China, America and Australia are here.
        // in the example above, China, America and Australia are three name phrases all attached to the prep: from.
        // first loop, search forward to find all the names before the pointer and the attached prep
        while (true) {
            currentNamePhrase = sentencePhrase.get(phrasePtr);
            phrasePtrInSentence = currentNamePhrase.phrasePosition;

            if (phrasePtrInSentence == 0) return;

            final String attachedWord = sentenceToken.get(phrasePtrInSentence - 1).text;

            // if the attached word is a comma or 'and'/'or', we consider it as a conj.
            if (",".equalsIgnoreCase(attachedWord)) {
                nameSequenceMeetEnd = false;
                phraseSequence.add(currentNamePhrase);
            }
            else if ("and".equalsIgnoreCase(attachedWord) || "or".equalsIgnoreCase(attachedWord)) {
                // meet end
                phraseSequence.add(currentNamePhrase);
                nameSequenceMeetEnd = true;
            }
            else if (Dictionary.checkup(attachedWord.toLowerCase()) != null && Dictionary
                    .checkup(attachedWord.toLowerCase())
                    .startsWith("IN")) {
                prep = attachedWord;
                phraseSequence.add(currentNamePhrase);
                break;
            }
            else {
                return;
            }
            phrasePtr--;
            if (phrasePtr < 0) return;
            if (sentencePhrase.get(phrasePtr).isDate) return;
            if (sentencePhrase.get(phrasePtr).phrasePosition + sentencePhrase.get(phrasePtr).phraseLength + 1
                != phrasePtrInSentence) return;
            // method terminates if the phrase before is not next to this phrase. This means the name sequence is broken.
        }

        phrasePtr = index + 1;

        // second loop, search backward to find the names behind the pointer
        //noinspection LoopConditionNotUpdatedInsideLoop
        while (!nameSequenceMeetEnd) {
            if (phrasePtr == sentencePhrase.size()) return;

            currentNamePhrase = sentencePhrase.get(phrasePtr);
            if (currentNamePhrase.isDate) return;
            phrasePtrInSentence = currentNamePhrase.phrasePosition;

            if (sentencePhrase.get(phrasePtr - 1).phrasePosition + sentencePhrase.get(phrasePtr - 1).phraseLength + 1
                != currentNamePhrase.phrasePosition) return;
            // method terminates if the phrase after is not next to this phrase.

            final String attachedWord = sentenceToken.get(phrasePtrInSentence - 1).text;
            // if the attached word is a comma or 'and'/'or', we consider it as a conj.
            if (",".equalsIgnoreCase(attachedWord)) {
                phraseSequence.add(currentNamePhrase);
            }
            else if ("and".equalsIgnoreCase(attachedWord) || "or".equalsIgnoreCase(attachedWord)) {
                // meet end
                phraseSequence.add(currentNamePhrase);
                break;
            }
            else {
                return;
            }

            phrasePtr++;
        }

        // finally, attach the prep with the words in the phraseSequence
        for (final Phrase name : phraseSequence) {
            name.attachedWordMap.put("prep", prep);
        }
    }


    /**
     * This method detects if the word's first character is in capital size.
     * @return true if it is; false if it is not.
     */
    private static boolean startsWithCap(final String word) {
        return !"i".equalsIgnoreCase(word)
               && isAllowedChar(word.charAt(0))
               && isUpperCase(word.charAt(0));
    }

    /**
     * This method detects if the word has more than one character which is capital sized.
     * @return true if there are more than one character upper cased; return false if less than one character is upper cased.
     */
    private static boolean hasManyCaps(final String word) {
        if ("i".equalsIgnoreCase(word)) return false;
        int capCharCount = 0;
        for (int i = 0; i < word.length(); i++) {
            if (isUpperCase(word.charAt(i))) capCharCount++;
            if (capCharCount == 2) return true;
        }
        return false;
    }

    /** This method tests if the input character is a legal character (a-z || A-Z). */
    private static boolean isAllowedChar(final char c) {
        //noinspection OverlyComplexBooleanExpression
        return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
    }
}
