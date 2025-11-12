package tuples.codegen;

public final class GeneratedMain {
    public static void main(String[] args) throws Exception {
        final int left = 9;
        final int right = 4;
        final Divmod_Result result = new Divmod_Result(right, left);
        int quotient;
        int remainder;
        quotient = result.quotient();
        remainder = result.remainder();
    }
}

public record Divmod_Result(int quotient, int remainder) {}
