public record Point(double x, double y) {}

public class Main {
    public static void main(String[] args) {
        Point point = new Point(3.0, 4.0);
        System.out.println("Point: " + point.x() + ", " + point.y());
    }
}