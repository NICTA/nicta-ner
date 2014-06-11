Command line shell interface to process the input text

    > java -Xmx200m -jar NamedEntityAnalyser.jar
    Type in texts, -q for quit.
    > <Input text>

Find named entities from the text document:

    > java -Xmx200m -jar NamedEntityAnalyser.jar <text fileName>

Using jar in the java code:

1. import ner package

        import nicta.ner.NamedEntityAnalyzer;
        import nicta.ner.NERResultSet;
    
2. Create analyzer
    
        NamedEntityAnalyzer namedEntityAnalyzer = new NamedEntityAnalyzer();
    
3. string analysis

        NERResultSet nerResultSet = namedEntityAnalyzer.process("String input here.");
    
4. file analysis

        NERResultSet nerResultSet = namedEntityAnalyzer.process(NamedEntityAnalyzer.ReadFileAsString("File name"));
    
5. output to string

        String result = nerResultSet.toString();
    
6. output to HashMap

        HashMap<String, String> resultMap = nerResultSet.getMappedResult();
