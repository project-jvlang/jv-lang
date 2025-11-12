package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    private static (Int String) makePair(int base) {
        final String label = "pair";
        return new Makepair_Result(base, label);
    }
    
    public static void main(String[] args) throws java.lang.Exception {
        final (Int String) __jv_tuple_0 = GeneratedMain.makePair(42);
        final Object number = __jv_tuple_0._1();
        final Object text = __jv_tuple_0._2();
        System.out.println(String.format("makePair -> number=%s label=%s", number, text));
    }
}

public record Makepair_Result(int value, String label) {}
