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

import java.io.*;
import java.util.*;

public class OrgIdNameLookup {
    public static void main(String[] args) throws IOException {
        final File ids = new File(args[0]);
        final File lookup = new File(args[1]);

        final BufferedReader lookupReader = new BufferedReader(new FileReader(lookup));
        final Map<String, String> m = new HashMap<>();
        int count = 0;
        for (String line; (line = lookupReader.readLine()) != null; ) {
            final String[] parts = line.split("\t", 2);
            m.put(parts[0], parts[1]);
            count++;
            if (count % 100000 == 0) {
                System.err.println(count);
            }
        }
        lookupReader.close();

        final BufferedReader idReader = new BufferedReader(new FileReader(ids));
        for (String line; (line = idReader.readLine()) != null; ) {
            System.out.println(m.get(line));
        }
    }
}
