package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    private static Makepair_Result makePair(int base) {
        final String label = "pair";
        return new Makepair_Result(base, label);
    }
    
    public static void main(String[] args) throws java.lang.Exception {
        final Makepair_Result __jv_tuple_0 = GeneratedMain.makePair(42);
        final int number = __jv_tuple_0.value();
        final java.lang.String text = __jv_tuple_0.label();
        System.out.println(String.format("makePair -> number=%s label=%s", number, text));
    }
}

public record Makepair_Result(int value, String label) {
    public int base() { return this.value(); }
    public int _1() { return this.value(); }
    public String _2() { return this.label(); }
}
