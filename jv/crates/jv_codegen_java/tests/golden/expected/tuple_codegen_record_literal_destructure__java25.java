package tuples.codegen;

public final class GeneratedMain {
    static final int left = 9;
    static final int right = 4;
    static final Divmod_Result result = new Divmod_Result(right, left);
    static int quotient;
    static int remainder;
    
    public static void main(String[] args) throws Exception {
        quotient = result.quotient();
        remainder = result.remainder();
    }
}

public record Divmod_Result(int quotient, int remainder) {
    public int _1() { return this.quotient(); }
    public int _2() { return this.remainder(); }
}
