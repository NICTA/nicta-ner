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
package nicta.ner;

import nicta.ner.data.Phrase;

import java.util.HashMap;
import java.util.List;

/**
 * This class encapsulated the two result set used in the NameExtractor class
 * into one single class.
 * <p/>
 * The map result can be got from this class as well.
 * @author William Han
 */
public class NERResultSet {
    public List<List<String>> tokens;
    public List<List<Phrase>> phrases;

    private HashMap<String, String> _result = null;

    /**
     * This method returns a map format set of the result.
     * @return
     */
    public HashMap<String, String> getMappedResult() {
        if (_result != null) return _result;
        _result = new HashMap<String, String>();
        for (List<Phrase> pa : phrases) {
            for (Phrase p : pa) {
                String ps = p.phraseString();
                _result.put(ps, p.phraseType.toString());
            }
        }
        return _result;
    }

    public String toString() {
        return NERResultSet.resultToString(this);
    }

    /**
     * Convert the result set into a standard String output.
     * @param _ner
     * @return
     */
    public static String resultToString(NERResultSet _ner) {
        StringBuilder sb = new StringBuilder();

        for (int si = 0; si < _ner.tokens.size(); si++) {
            List<String> sentence = _ner.tokens.get(si);
            List<Phrase> phrases = _ner.phrases.get(si);
            for (int wi = 0; wi < sentence.size(); wi++)
                sb.append(sentence.get(wi) + " ");
            sb.append("\n===============================================\n");
            for (int pi = 0; pi < phrases.size(); pi++) {
                Phrase np = phrases.get(pi);
                String ptext = "";
                for (int wi = 0; wi < phrases.get(pi).phrase.length; wi++) {
                    ptext += (phrases.get(pi).phrase[wi] + " ");
                }
                ptext = ptext.trim();

                ///*
                String stext = "";
                for (int sci = 0; sci < np.score.length; sci++) {
                    if (sci != 0) stext += ", ";
                    stext += np.score[sci];
                }
                sb.append(np.phrasePosition + ": " + ptext + "\t" + np.phraseType.toString() + "\t"
                          + stext + "\t"
                          + np.attachedWordMap.get("prep") + "\t"
                          + np.phrasePosition + ":" + np.phraseStubPosition + ":" + np.phraseStubLength + ":"
                          + np.phraseLength
                          + "\n");
                //*/
                //sb.append(ptext + "=" + np.phraseType + "\n");
            }
            sb.append("\n\n");
        }
        return sb.toString();
    }
}
