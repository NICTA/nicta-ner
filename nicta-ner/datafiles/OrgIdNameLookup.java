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
